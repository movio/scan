package co.movio.streamops

import cats._
import cats.arrow._
import cats.implicits._

// TODO: Update and extract documentation.

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
sealed trait LinearScan[Input, Output] {
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

  def andThen[Next](next: LinearScan[Output, Next]): LinearScan[Input, Next] =
    this >>> next

  def map[T](f: Output => T): LinearScan[Input, T] = this andThen LinearScan.lift(f)

  def using[T](f: T => Input): LinearScan[T, Output] = LinearScan.lift(f) andThen this

}

object LinearScan {
  def apply[State_, Input, Output](s: State_)(
      f: (State_, Input) ⇒ (State_, Output)): LinearScan[Input, Output] =
    new LinearScan[Input, Output] {
      override type State = State_
      override val initialState: State_ = s
      override val step: (State_, Input) => (State_, Output) = f
    }

  def lift[Input, Output]: (Input => Output) => LinearScan[Input, Output] =
    arrowInstance.lift

  def zip[A, B1, B2](x1: LinearScan[A, B1], x2: LinearScan[A, B2]): LinearScan[A, (B1, B2)] =
    (x1 |@| x2).map((_, _))

  def zip[A, B1, B2, B3](x1: LinearScan[A, B1],
                         x2: LinearScan[A, B2],
                         x3: LinearScan[A, B3]): LinearScan[A, (B1, B2, B3)] =
    (x1 |@| x2 |@| x3).map((_, _, _))

  def zip[A, B1, B2, B3, B4](x1: LinearScan[A, B1],
                             x2: LinearScan[A, B2],
                             x3: LinearScan[A, B3],
                             x4: LinearScan[A, B4]): LinearScan[A, (B1, B2, B3, B4)] =
    (x1 |@| x2 |@| x3 |@| x4).map((_, _, _, _))

  def zip[A, B1, B2, B3, B4, B5](x1: LinearScan[A, B1],
                                 x2: LinearScan[A, B2],
                                 x3: LinearScan[A, B3],
                                 x4: LinearScan[A, B4],
                                 x5: LinearScan[A, B5]): LinearScan[A, (B1, B2, B3, B4, B5)] =
    (x1 |@| x2 |@| x3 |@| x4 |@| x5).map((_, _, _, _, _))

  def zip[A, B1, B2, B3, B4, B5, B6](
      x1: LinearScan[A, B1],
      x2: LinearScan[A, B2],
      x3: LinearScan[A, B3],
      x4: LinearScan[A, B4],
      x5: LinearScan[A, B5],
      x6: LinearScan[A, B6]): LinearScan[A, (B1, B2, B3, B4, B5, B6)] =
    (x1 |@| x2 |@| x3 |@| x4 |@| x5 |@| x6).map((_, _, _, _, _, _))

  def sum[T](implicit evidence: Numeric[T]): LinearScan[T, T] =
    LinearScan[Option[T], T, T](None) {
      case (None, n) => (Option(n), n)
      case (Some(o), n) => {
        import evidence._
        val m = o + n
        (Option(m), m)
      }
    }

  def count[T](predicate: T => Boolean): LinearScan[T, Int] =
    lift[T, Int](t => if (predicate(t)) 1 else 0) >>> sum[Int]

  def previous[T]: LinearScan[T, Option[T]] =
    LinearScan(None: Option[T])((o, n) => (Option(n), o))

  def mean[T](implicit evidence: Numeric[T]): LinearScan[T, Double] = {
    import evidence._
    LinearScan[(Int, T), T, Double]((0, zero)) {
      case ((count, sum), num) =>
        val newCount = count + 1
        val newSum = sum + num
        ((newCount, newSum), newSum.toDouble() / newCount)
    }
  }

  def min[T: Ordering]: LinearScan[T, T] =
    LinearScan[Option[T], T, T](None) {
      case (None, value) => (Some(value), value)
      case (Some(currentMin), value) => {
        val newMin = implicitly[Ordering[T]].min(currentMin, value)
        (Some(newMin), newMin)
      }
    }

  def max[T: Ordering]: LinearScan[T, T] =
    min(implicitly[Ordering[T]].reverse)

  def first[T]: LinearScan[T, T] = LinearScan[Option[T], T, T](None) {
    case (None, new_) => (Some(new_), new_)
    case (Some(old), _) => (Some(old), old)
  }

  def last[T]: LinearScan[T, T] = arrowInstance.id

  def findFirst[T](pred: T => Boolean): LinearScan[T, Option[T]] =
    LinearScan[Option[T], T, Option[T]](None) {
      case (None, new_) if pred(new_) => (Option(new_), Option(new_))
      case (old, _) => (old, old)
    }

  def findLast[T](pred: T => Boolean): LinearScan[T, Option[T]] =
    LinearScan[Option[T], T, Option[T]](None) {
      case (old, new_) if pred(new_) => (Option(new_), Option(new_))
      case (old, _) => (old, old)
    }

  def collect[T]: LinearScan[T, Seq[T]] =
    LinearScan[Seq[T], T, Seq[T]](Seq.empty) {
      case (past, now) =>
        val l = now +: past
        (l, l)
    }.map(_.reverse)

  implicit private def applicativeInstance[T]: Apply[LinearScan[T, ?]] =
    new Apply[LinearScan[T, ?]] {
      override def ap[A, B](ff: LinearScan[T, A ⇒ B])(fa: LinearScan[T, A]) =
        LinearScan[(ff.State, fa.State), T, B](ff.initialState, fa.initialState) {
          case ((ffs, fas), t) ⇒ {
            val (ffs_, f) = ff.step(ffs, t)
            val (fas_, a) = fa.step(fas, t)
            ((ffs_, fas_), f(a))
          }
        }

      override def map[A, B](fa: LinearScan[T, A])(f: A ⇒ B) =
        LinearScan[fa.State, T, B](fa.initialState) {
          case (s, t) => fa.step(s, t).map(f)
        }
    }

  implicit private val arrowInstance: Arrow[LinearScan] = new Arrow[LinearScan] {
    override def lift[A, B](f: A => B): LinearScan[A, B] =
      id[A].map(f)

    override def id[A]: LinearScan[A, A] =
      LinearScan[Unit, A, A](())((_, _))

    override def compose[A, B, C](f: LinearScan[B, C], g: LinearScan[A, B]): LinearScan[A, C] =
      LinearScan[(f.State, g.State), A, C]((f.initialState, g.initialState)) {
        case ((fs, gs), a) =>
          val (gs_, b) = g.step(gs, a)
          val (fs_, c) = f.step(fs, b)
          ((fs_, gs_), c)
      }

    override def first[A, B, C](fa: LinearScan[A, B]): LinearScan[(A, C), (B, C)] =
      LinearScan[fa.State, (A, C), (B, C)](fa.initialState) {
        case (o, (a, c)) => {
          val (n, b) = fa.step(o, a)
          (n, (b, c))
        }
      }
  }
}
