package co.movio.scan

import cats._
import cats.arrow._
import cats.implicits._

/** @groupname running Running Scan's
  * @groupprio running 0
  * @groupname composing Composing Scan's
  * @groupprio composing 1
  */
sealed trait Scan[Input, Output] {
  protected type State
  protected val initialState: State
  protected val step: (State, Input) ⇒ (State, Output)

  /** Lazily run the scan over given [[Stream]].
    *
    * The output [[Stream]] will yield the same number of
    * elements as the input [[Stream]].
    *
    * {{{
    * > Scan.sum.scan(Stream(1, 2, 3, 4))
    * Stream(1, 3, 6, 10)
    * }}}
    *
    * @group running
    */
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

  /** Execute the scan over given [[Stream]] and return the final result.
    *
    * It will return `None` when the input stream is empty.
    *
    * {{{
    * > Scan.sum.fold(Stream(1, 2, 3, 4))
    * Some(10)
    * }}}
    *
    * @group running
    */
  def fold(elems: Stream[Input]): Option[Output] =
    scan(elems).lastOption

  /** Create a `Scan` feeding this `Scan`'s output to `next`.
    *
    * {{{
    * > val twice = Scan.lift[Int, Int](_ * 2)
    * > val inc   = Scan.lift[Int, Int](_ + 1)
    * > twice.andThen(inc).scan(Stream(1, 2, 3))
    * Stream(3, 5, 7)
    * }}}
    *
    * If you use `cats`, you can use the `Arrow` instance instead:
    * {{{
    * > import cats.implicits._
    * > twice >>> inc
    * }}}
    *
    * @group composing
    */
  def andThen[Next](next: Scan[Output, Next]): Scan[Input, Next] =
    this >>> next

  /** Create a `Scan` which runs given `Scan` in parallel with `this`.
    * {{{
    * > val twice = Scan.lift[Int, Int](_ * 2)
    * > val inc   = Scan.lift[Int, Int](_ + 1)
    * > twice.zip(inc).scan(Stream(1, 2, 3))
    * Stream((2, 2), (4, 3), (6, 4))
    * }}}
    *
    * If you use `cats`, you can use the `Apply` instance instead.
    * {{{
    * > import cats.implicits._
    * > Apply[Scan[Int, ?]].tuple2(twice, inc)
    * }}}
    *
    * @group composing
    */
  def zip[Other](other: Scan[Input, Other]): Scan[Input, (Output, Other)] =
    (this |@| other).map((_, _))
}

/** @groupname building Building Scan's
  * @groupprio building 0
  * @groupname instances Instances
  * @groupprio instances 1
  */
object Scan {

  /** Construct a `Scan` given the initial state and the step function.
    * {{{
    * > val grow: Scan[String, String] = Scan(1) { case (s, i) => (s + 1, i * s}
    * > sum.scan(Stream("a", "b", "c"))
    * Stream("a", "bb", "ccc")
    * }}}
    *
    * @group building
    */
  def apply[S, A, B](initialState_ : S)(step_ : (S, A) ⇒ (S, B)): Scan[A, B] =
    new Scan[A, B] {
      override type State = S
      override val initialState = initialState_
      override val step = step_
    }

  /** Lifts a function to a `Scan`.
    *
    * {{{
    * > Scan.lift(_ * 2).scan(Stream(1, 2, 3))
    * Stream(2, 4, 6)
    * }}}
    *
    * @group building
    */
  def lift[A, B]: (A => B) => Scan[A, B] =
    arrowInstance.lift

  /** Identity with respect to `[[Scan.arrowInstance]]`.
    *
    * @group building
    */
  def id[T]: Scan[T, T] =
    arrowInstance.id

  /** Combine all values within a container using [[Semigroup.combine.]].
    *
    * @group building
    */
  def sum[T: Semigroup]: Scan[T, T] =
    Scan[Option[T], T, T](None) {
      case (None, n) => (Option(n), n)
      case (Some(o), n) => {
        val m = o |+| n
        (Option(m), m)
      }
    }

  /** Computes the sum of all elements matching given predicate.
    *
    * @group building
    */
  def count[T](pred: T => Boolean): Scan[T, Int] =
    lift[T, Int](t => if (pred(t)) 1 else 0) >>> sum[Int]

  /** Computes the mean of all elements.
    *
    * @group building
    */
  def mean: Scan[Double, Double] = Scan[(Int, Double), Double, Double]((0, 0)) {
    case ((count, sum), num) =>
      val newCount = count + 1
      val newSum = sum + num
      ((newCount, newSum), newSum / newCount)
  }

  /** For every element, yields the smallest element until the element (inclusive).
    *
    * @group building
    */
  def min[T: Ordering]: Scan[T, T] =
    sum(new Semigroup[T] {
      override def combine(x: T, y: T) = implicitly[Ordering[T]].min(x, y)
    })

  /** For every element, yields the greatest element until the element (inclusive).
    *
    * @group building
    */
  def max[T: Ordering]: Scan[T, T] =
    min(implicitly[Ordering[T]].reverse)

  /** Yields the preceeding element for every element except the first one.
    *
    * {{{
    * > Scan.prev.scan(Stream(1, 2, 3))
    * Stream(None, Some(1), Some(2))
    * }}}
    *
    * @group building
    */
  def prev[T]: Scan[T, Option[T]] =
    Scan(None: Option[T])((o, n) => (Option(n), o))

  /** Always yields the last element. Synonymous with [[Scan.id]].
    *
    * {{{
    * > Scan.first.scan(Stream(1, 2, 3))
    * Stream(1, 1, 1)
    * }}}
    *
    * @group building
    */
  def first[T]: Scan[T, T] = Scan[Option[T], T, T](None) {
    case (None, new_) => (Some(new_), new_)
    case (Some(old), _) => (Some(old), old)
  }

  /** Always yields the last element. Synonymous with [[Scan.id]].
    *
    * {{{
    * > Scan.last.scan(Stream(1, 2, 3))
    * Stream(1, 2, 3)
    * }}}
    *
    * @group building
    */
  def last[T]: Scan[T, T] = Scan.id

  /** Yields the first element matching the predicate.
    *
    * @group building
    */
  def findFirst[T](pred: T => Boolean): Scan[T, Option[T]] =
    Scan[Option[T], T, Option[T]](None) {
      case (None, new_) if pred(new_) => (Option(new_), Option(new_))
      case (old, _) => (old, old)
    }

  /** @group building */
  def findLast[T](pred: T => Boolean): Scan[T, Option[T]] =
    Scan[Option[T], T, Option[T]](None) {
      case (old, new_) if pred(new_) => (Option(new_), Option(new_))
      case (old, _) => (old, old)
    }

  /** @group building */
  def collect[T]: Scan[T, Seq[T]] =
    Scan[Seq[T], T, Seq[T]](Seq.empty) {
      case (past, now) =>
        val l = now +: past
        (l, l)
    }.map(_.reverse)

  /** @group instances */
  implicit def applicativeInstance[T]: Apply[Scan[T, ?]] =
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

  /** @group instances */
  implicit val arrowInstance: Arrow[Scan] = new Arrow[Scan] {
    override def lift[A, B](f: A => B): Scan[A, B] =
      id[A].map(f)

    override def id[A]: Scan[A, A] =
      Scan[Unit, A, A](())((_, _))

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
