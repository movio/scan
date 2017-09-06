package co.movio.streamops

import cats._
import cats.arrow._
import cats.implicits._

/* Represent scans as first class objects to be able
 * to compose them.
 *
 * You can create a Scan via:
 *
 * - Scan.apply:
 *     Create a Scan using an initial state and step function.
 * - Scan.{lift,sum,max,min,prev,average}:
 *     Use predefined Scan's.
 *
 * We can compose Scan's in two ways:
 *
 * - Applicative ( `(scan1 |@| scan2).map((r1, r2) => r1 + r2)` :
 *     Runs two scans on one stream in a single pass, and
 *     zips them.
 * - Arrow ( `scan1 >>> scan2` ):
 *     Runs one scan over a stream, and use its results to
 *     feed another stream.
 *
 * No matter how they're built, Scan's are always single-pass.
 */
sealed trait Scan[Input, Output] {
  type State
  val initialState: State
  val step: (State, Input) ⇒ (State, Output)

  def scan(elems: Stream[Input]): Stream[Output] = {
    def go(input: Stream[Input], state: State): Stream[Output] =
      input match {
        case Stream.Empty ⇒ Stream.empty
        case head #:: tail ⇒
          val (newState, result) = step(state, head)
          result #:: go(tail, newState)
      }
    go(elems, initialState)
  }

  def fold(elems: Stream[Input]): Option[Output] =
    scan(elems).lastOption
}

object Scan {
  def apply[S, A, B](s: S)(f: (S, A) ⇒ (S, B)): Scan[A, B] =
    new Scan[A, B] {
      override type State = S
      override val initialState = s
      override val step = f
    }

  def lift[A, B]: (A => B) => Scan[A, B] = arrow.lift
  def id[T]: Scan[T, T] = arrow.id

  def sum[T](implicit s: Semigroup[T]): Scan[T, T] =
    Scan[Option[T], T, T](None) {
      case (None, n) => (Some(n), n)
      case (Some(o), n) => {
        val m = o |+| n
        (Some(m), m)
      }
    }

  def count[T](pred: T => Boolean): Scan[T, Int] =
    lift[T, Int](t => if (pred(t)) 1 else 0) >>> sum[Int]

  def prev[T]: Scan[T, Option[T]] =
    Scan(None: Option[T])((o, n) => (Some(n), o))

  def mean: Scan[Double, Double] = {
    val additive: Semigroup[Double] = new Semigroup[Double] {
      override def combine(x: Double, y: Double) = x + y
    }
    (sum[Double](additive) |@| count(Function.const(true))) map ((s, c) => s / c)
  }

  def min[T](implicit ord: Ordering[T]): Scan[T, T] =
    sum(new Semigroup[T] {
      override def combine(x: T, y: T) = if (ord.lt(x, y)) x else y
    })

  def max[T](implicit ord: Ordering[T]): Scan[T, T] =
    min(ord.reverse)

  def first[T]: Scan[T, T] = Scan[Option[T], T, T](None) {
    case (None, new_) => (Some(new_), new_)
    case (Some(old), _) => (Some(old), old)
  }

  def last[T]: Scan[T, T] = Scan.id

  def findFirst[T](pred: T => Boolean): Scan[T, Option[T]] =
    Scan[Option[T], T, Option[T]](None) {
      case (None, new_) if pred(new_) => (Some(new_), Some(new_))
      case (old, _) => (old, old)
    }

  def findLast[T](pred: T => Boolean): Scan[T, Option[T]] =
    Scan[Option[T], T, Option[T]](None) {
      case (old, new_) if pred(new_) => (Some(new_), Some(new_))
      case (old, _) => (old, old)
    }

  def collect[T]: Scan[T, Seq[T]] =
    Scan[Seq[T], T, Seq[T]](Seq.empty) {
      case (past, now) =>
        val l = now +: past
        (l, l)
    }.map(_.reverse)

  implicit def apply[T]: Apply[Scan[T, ?]] =
    new Apply[Scan[T, ?]] {
      override def ap[A, B](ff: Scan[T, A ⇒ B])(fa: Scan[T, A]) =
        Scan[(ff.State, fa.State), T, B](ff.initialState, fa.initialState) {
          case ((ffs, fas), t) ⇒ {
            val (ffs_, f) = ff.step(ffs, t)
            val (fas_, a) = fa.step(fas, t)
            ((ffs_, fas_), f(a))
          }
        }

      override def map[A, B](fa: Scan[T, A])(f: A ⇒ B) =
        Scan[fa.State, T, B](fa.initialState) {
          case (s, t) => fa.step(s, t).map(f)
        }
    }

  implicit val arrow: Arrow[Scan] = new Arrow[Scan] {
    override def lift[A, B](f: A => B): Scan[A, B] =
      id[A].map(f)

    override def id[A]: Scan[A, A] =
      Scan[Unit, A, A](())(Tuple2(_, _))

    override def compose[A, B, C](f: Scan[B, C], g: Scan[A, B]): Scan[A, C] =
      Scan[(f.State, g.State), A, C]((f.initialState, g.initialState)) {
        case ((fs, gs), a) =>
          val (gs_, b) = g.step(gs, a)
          val (fs_, c) = f.step(fs, b)
          ((fs_, gs_), c)
      }

    override def first[A, B, C](fa: Scan[A, B]): Scan[(A, C), (B, C)] =
      Scan[fa.State, (A, C), (B, C)](fa.initialState) {
        case (o, (a, c)) => {
          val (n, b) = fa.step(o, a)
          (n, (b, c))
        }
      }
  }
}
