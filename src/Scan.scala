package co.movio.scan

import cats._
import cats.arrow._
import cats.implicits._

import scala.math.Numeric._

/** @groupname running Running
  * @groupprio running 0
  * @groupname composing Composing
  * @groupprio composing 1
  */
sealed trait Scan[Input, Output] {
  protected type State
  protected val initialState: State
  protected val step: (State, Input) ⇒ (State, Output)

  /** Lazily run the scan over given `Stream`.
    *
    * The output `Stream` will yield the same number of
    * elements as the input `Stream`.
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

  /** Execute the scan over given `Stream` and return the final result.
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
    * If you use `[[https://typelevel.org/cats/ cats]]`, you can use the
    * `Arrow` instance instead:
    * {{{
    * > import cats.implicits._
    * > twice >>> inc
    * }}}
    *
    * @group composing
    */
  def andThen[Next](next: Scan[Output, Next]): Scan[Input, Next] =
    this >>> next

  /** Create a `Scan` which runs given `Scan` in parallel with this Scan.
    * {{{
    * > val twice = Scan.lift[Int, Int](_ * 2)
    * > val inc   = Scan.lift[Int, Int](_ + 1)
    * > twice.zip(inc).scan(Stream(1, 2, 3))
    * Stream((2, 2), (4, 3), (6, 4))
    * }}}
    *
    * If you use `[[https://typelevel.org/cats/ cats]]`, you can use the
    * `Apply` instance instead:
    * {{{
    * > import cats.implicits._
    * > Apply[Scan[Int, ?]].tuple2(twice, inc)
    * }}}
    *
    * @see [[Scan$.zip[A,B1,B2]* Scan.zip]]
    * @group composing
    */
  def zip[T](other: Scan[Input, T]): Scan[Input, (Output, T)] =
    Scan.applicativeInstance.tuple2(this, other)

  /** Apply a function to the result of thus Scan.
    *
    * @group composing
    */
  def map[T](f: Output => T): Scan[Input, T] =
    Scan.applicativeInstance.map(this)(f)

  /** Maps a function before feeding the input to this Scan.
    *
    * {{{
    * > val sumFirsts: Scan [(Int, String), Int] = Scan.sum[Int].using(_._1)
    * > sumFirsts.scan(Stream((1, "a"), (2, "b"), (3, "c")))
    * Stream(1, 3, 6)
    * }}}
    * @group composing
    */
  def using[T](f: T => Input): Scan[T, Output] =
    this.lmap(f)
}

/** @groupname building Building
  * @groupprio building 0
  * @groupname composing Composing
  * @groupprio composing 1
  * @groupname instances Instances
  * @groupprio instances 1
  */
object Scan {

  /** Construct a `Scan` given the initial state and the step function.
    * {{{
    * > val grow: Scan[String, String] = Scan(1) { case (s, i) => (s + 1, i * s) }
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

  /** Combine all values within a container using
    * [[https://typelevel.org/cats/typeclasses/semigroup.html Semigroup.combine]].
    *
    * @group building
    */
  def combine[T: Semigroup]: Scan[T, T] =
    Scan[Option[T], T, T](None) {
      case (None, n) => (Option(n), n)
      case (Some(o), n) => {
        val m = o |+| n
        (Option(m), m)
      }
    }

  /** Sum all values within a container.
    *
    * @group building
    */
  def sum[T](implicit evidence: Numeric[T]): Scan[T, T] =
    Scan[T, T, T](evidence.zero) {
      case (o, n) => {
        val s = evidence.plus(o, n)
        (s, s)
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

  /** For every element, yields the least element until the element (inclusive).
    *
    * @group building
    */
  def min[T: Ordering]: Scan[T, T] =
    combine(new Semigroup[T] {
      override def combine(x: T, y: T) = implicitly[Ordering[T]].min(x, y)
    })

  /** For every element, yields the greatest element until the element (inclusive).
    *
    * @group building
    */
  def max[T: Ordering]: Scan[T, T] =
    min(implicitly[Ordering[T]].reverse)

  /** Yields the preceding element for every element except the first one.
    *
    * {{{
    * > Scan.previous.scan(Stream(1, 2, 3))
    * Stream(None, Some(1), Some(2))
    * }}}
    *
    * @group building
    */
  def previous[T]: Scan[T, Option[T]] =
    Scan(None: Option[T])((o, n) => (Option(n), o))

  /** Always yields the first element.
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
      case (_, new_) if pred(new_) => (Option(new_), Option(new_))
      case (old, _) => (old, old)
    }

  /** @group building
    *
    * Collect every preceding element (inclusive) to a `Seq` *in reverse order*.
    *
    * {{{
    * > Scan.collect.scan(Stream(1, 2, 3))
    * Stream(Seq(1), Seq(2, 1), Seq(3, 2, 1))
    * }}}
    */
  def collect[T]: Scan[T, Seq[T]] =
    Scan[Seq[T], T, Seq[T]](Seq.empty) {
      case (past, now) =>
        val l = now +: past
        (l, l)
    }

  /** @group instances */
  implicit def applicativeInstance[T]: Apply[Scan[T, ?]] =
    new Apply[Scan[T, ?]] {
      override def ap[A, B](ff: Scan[T, A ⇒ B])(fa: Scan[T, A]) =
        Scan[(ff.State, fa.State), T, B]((ff.initialState, fa.initialState)) {
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

  /** @group composing */
  def zip[A, B1, B2](s1: Scan[A, B1], s2: Scan[A, B2]): Scan[A, (B1, B2)] =
    applicativeInstance.tuple2(s1, s2)

  /** @group composing */
  def zip[A, B1, B2, B3](s1: Scan[A, B1],
                         s2: Scan[A, B2],
                         s3: Scan[A, B3]): Scan[A, (B1, B2, B3)] =
    applicativeInstance.tuple3(s1, s2, s3)

  /** @group composing */
  def zip[A, B1, B2, B3, B4](s1: Scan[A, B1],
                             s2: Scan[A, B2],
                             s3: Scan[A, B3],
                             s4: Scan[A, B4]): Scan[A, (B1, B2, B3, B4)] =
    applicativeInstance.tuple4(s1, s2, s3, s4)

  /** @group composing */
  def zip[A, B1, B2, B3, B4, B5](s1: Scan[A, B1],
                                 s2: Scan[A, B2],
                                 s3: Scan[A, B3],
                                 s4: Scan[A, B4],
                                 s5: Scan[A, B5]): Scan[A, (B1, B2, B3, B4, B5)] =
    applicativeInstance.tuple5(s1, s2, s3, s4, s5)

  /** @group composing */
  def zip[A, B1, B2, B3, B4, B5, B6](s1: Scan[A, B1],
                                     s2: Scan[A, B2],
                                     s3: Scan[A, B3],
                                     s4: Scan[A, B4],
                                     s5: Scan[A, B5],
                                     s6: Scan[A, B6]): Scan[A, (B1, B2, B3, B4, B5, B6)] =
    applicativeInstance.tuple6(s1, s2, s3, s4, s5, s6)

  /** @group composing */
  def zip[A, B1, B2, B3, B4, B5, B6, B7](s1: Scan[A, B1],
                                         s2: Scan[A, B2],
                                         s3: Scan[A, B3],
                                         s4: Scan[A, B4],
                                         s5: Scan[A, B5],
                                         s6: Scan[A, B6],
                                         s7: Scan[A, B7]): Scan[A, (B1, B2, B3, B4, B5, B6, B7)] =
    applicativeInstance.tuple7(s1, s2, s3, s4, s5, s6, s7)

  /** @group composing */
  def zip[A, B1, B2, B3, B4, B5, B6, B7, B8](
      s1: Scan[A, B1],
      s2: Scan[A, B2],
      s3: Scan[A, B3],
      s4: Scan[A, B4],
      s5: Scan[A, B5],
      s6: Scan[A, B6],
      s7: Scan[A, B7],
      s8: Scan[A, B8]): Scan[A, (B1, B2, B3, B4, B5, B6, B7, B8)] =
    applicativeInstance.tuple8(s1, s2, s3, s4, s5, s6, s7, s8)

  /** @group composing */
  def zip[A, B1, B2, B3, B4, B5, B6, B7, B8, B9](
      s1: Scan[A, B1],
      s2: Scan[A, B2],
      s3: Scan[A, B3],
      s4: Scan[A, B4],
      s5: Scan[A, B5],
      s6: Scan[A, B6],
      s7: Scan[A, B7],
      s8: Scan[A, B8],
      s9: Scan[A, B9]): Scan[A, (B1, B2, B3, B4, B5, B6, B7, B8, B9)] =
    applicativeInstance.tuple9(s1, s2, s3, s4, s5, s6, s7, s8, s9)

  /** @group composing */
  def zip[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10](
      s1: Scan[A, B1],
      s2: Scan[A, B2],
      s3: Scan[A, B3],
      s4: Scan[A, B4],
      s5: Scan[A, B5],
      s6: Scan[A, B6],
      s7: Scan[A, B7],
      s8: Scan[A, B8],
      s9: Scan[A, B9],
      s10: Scan[A, B10]): Scan[A, (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10)] =
    applicativeInstance.tuple10(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10)

  /** @group composing */
  def zip[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11](
      s1: Scan[A, B1],
      s2: Scan[A, B2],
      s3: Scan[A, B3],
      s4: Scan[A, B4],
      s5: Scan[A, B5],
      s6: Scan[A, B6],
      s7: Scan[A, B7],
      s8: Scan[A, B8],
      s9: Scan[A, B9],
      s10: Scan[A, B10],
      s11: Scan[A, B11]): Scan[A, (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11)] =
    applicativeInstance.tuple11(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11)

  /** @group composing */
  def zip[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12](
      s1: Scan[A, B1],
      s2: Scan[A, B2],
      s3: Scan[A, B3],
      s4: Scan[A, B4],
      s5: Scan[A, B5],
      s6: Scan[A, B6],
      s7: Scan[A, B7],
      s8: Scan[A, B8],
      s9: Scan[A, B9],
      s10: Scan[A, B10],
      s11: Scan[A, B11],
      s12: Scan[A, B12]): Scan[A, (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12)] =
    applicativeInstance.tuple12(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12)

  /** @group composing */
  def zip[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13](
      s1: Scan[A, B1],
      s2: Scan[A, B2],
      s3: Scan[A, B3],
      s4: Scan[A, B4],
      s5: Scan[A, B5],
      s6: Scan[A, B6],
      s7: Scan[A, B7],
      s8: Scan[A, B8],
      s9: Scan[A, B9],
      s10: Scan[A, B10],
      s11: Scan[A, B11],
      s12: Scan[A, B12],
      s13: Scan[A, B13]): Scan[A, (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13)] =
    applicativeInstance.tuple13(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13)

  /** @group composing */
  def zip[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14](
      s1: Scan[A, B1],
      s2: Scan[A, B2],
      s3: Scan[A, B3],
      s4: Scan[A, B4],
      s5: Scan[A, B5],
      s6: Scan[A, B6],
      s7: Scan[A, B7],
      s8: Scan[A, B8],
      s9: Scan[A, B9],
      s10: Scan[A, B10],
      s11: Scan[A, B11],
      s12: Scan[A, B12],
      s13: Scan[A, B13],
      s14: Scan[A, B14]): Scan[A, (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14)] =
    applicativeInstance.tuple14(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14)

  /** @group composing */
  def zip[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15](s1: Scan[A, B1],
                                                                               s2: Scan[A, B2],
                                                                               s3: Scan[A, B3],
                                                                               s4: Scan[A, B4],
                                                                               s5: Scan[A, B5],
                                                                               s6: Scan[A, B6],
                                                                               s7: Scan[A, B7],
                                                                               s8: Scan[A, B8],
                                                                               s9: Scan[A, B9],
                                                                               s10: Scan[A, B10],
                                                                               s11: Scan[A, B11],
                                                                               s12: Scan[A, B12],
                                                                               s13: Scan[A, B13],
                                                                               s14: Scan[A, B14],
                                                                               s15: Scan[A, B15])
    : Scan[A, (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15)] =
    applicativeInstance.tuple15(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15)

  /** @group composing */
  def zip[A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16](
      s1: Scan[A, B1],
      s2: Scan[A, B2],
      s3: Scan[A, B3],
      s4: Scan[A, B4],
      s5: Scan[A, B5],
      s6: Scan[A, B6],
      s7: Scan[A, B7],
      s8: Scan[A, B8],
      s9: Scan[A, B9],
      s10: Scan[A, B10],
      s11: Scan[A, B11],
      s12: Scan[A, B12],
      s13: Scan[A, B13],
      s14: Scan[A, B14],
      s15: Scan[A, B15],
      s16: Scan[A, B16])
    : Scan[A, (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16)] =
    applicativeInstance.tuple16(s1,
                                s2,
                                s3,
                                s4,
                                s5,
                                s6,
                                s7,
                                s8,
                                s9,
                                s10,
                                s11,
                                s12,
                                s13,
                                s14,
                                s15,
                                s16)

  /** @group instances */
  implicit val arrowInstance: Arrow[Scan] = new Arrow[Scan] {
    override def lift[A, B](f: A => B): Scan[A, B] =
      Scan[Unit, A, B](()) { case ((), a) => ((), f(a)) }

    override def id[A]: Scan[A, A] =
      Scan.lift(identity)

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
