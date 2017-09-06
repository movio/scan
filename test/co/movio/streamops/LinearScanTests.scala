package co.movio.streamops

import org.scalacheck._
import org.scalatest._
import Matchers._
import org.scalatest.prop._

@SuppressWarnings(
  Array("org.wartremover.warts.NonUnitStatements",
        "org.wartremover.warts.TraversableOps",
        "org.wartremover.warts.Any"))
class LinearScanTest extends FunSuite with GeneratorDrivenPropertyChecks {
  import Utils._

  test("len l == len (scan l)") {
    forAll { (scan: LinearScan[Int, Int], input: List[Int]) =>
      assert(input.length == scan.scan(input.toStream).length)
    }
  }

  test("scan . dropRight 1 == dropRight 1 . scan") {
    forAll { (scan: LinearScan[Int, Int], input: List[Int]) =>
      assert(
        scan.scan(input.toStream).dropRight(1) ==
          scan.scan(input.dropRight(1).toStream)
      )
    }
  }

  test("Scans can be combine using `zip`") {
    forAll { (scan1: LinearScan[Int, Int], scan2: LinearScan[Int, Int], input: List[Int]) =>
      val zipped: LinearScan[Int, (Int, Int)] = LinearScan.zip(scan1, scan2)
      val r1: List[(Int, Int)] = zipped.scan(input.toStream).toList
      val r2: List[(Int, Int)] = scan1.scan(input.toStream).toList.zip(scan2.scan(input.toStream))
      assert(r1 == r2, s"$r1 != $r2")
    }
  }

  test("Scans can be chained using `andThen`") {
    forAll { (f: LinearScan[Int, Int], g: LinearScan[Int, Int], input: Stream[Int]) =>
      {
        whenever(input.nonEmpty) {
          val actual: Stream[Int] = (f andThen g).scan(input)
          val expected: Stream[Int] = g.scan(f.scan(input))
          assert(actual == expected)
        }
      }
    }
  }

  test("last . scan == foldLeft") {
    forAll { (init: Int, step: (Int, Int) => (Int, Int), input: List[Int]) =>
      whenever(input.nonEmpty) {
        val scan = LinearScan(init)(step)
        val actual: Int = scan.scan(input.toStream).last
        val expected: Int = {
          val foldInit: (Int, Int) = (init, 0)
          val foldStep: ((Int, Int), Int) => (Int, Int) = {
            case ((oldState, _), elem) => step(oldState, elem)
          }
          input.foldLeft(foldInit)(foldStep)._2
        }
        assert(actual == expected, s"Actual: $actual \nExpected $expected")
      }
    }
  }

  // Assert if given scan gives the same result as reference implementation `f`.
  def sameAs[T: Arbitrary, R](f: List[T] => R, scan: LinearScan[T, R])(input: List[T]) =
    whenever(input.nonEmpty) {
      val actual = scan.fold(input.toStream).get
      val expected = f(input)
      assert(actual === expected)
    }

  test("sum") {
    forAll(sameAs[Int, Int](_.sum, LinearScan.sum) _)
  }

  test("count") {
    forAll { (input: List[Int], pred: Int => Boolean) =>
      sameAs[Int, Int](_.count(pred), LinearScan.count(pred))(input)
    }
  }

  test("prev") {
    forAll { input: List[Boolean] =>
      whenever(input.length > 1) {
        sameAs[Boolean, Option[Boolean]](_.dropRight(1).lastOption, LinearScan.previous)(input)
      }
    }
  }

  test("mean") {
    forAll(sameAs[Double, Double](i => i.sum / i.length, LinearScan.mean) _)
  }

  test("min") {
    forAll(sameAs[Int, Int](_.min, LinearScan.min) _)
  }

  test("max") {
    forAll(sameAs[Int, Int](_.max, LinearScan.max) _)
  }

  test("first") {
    forAll(sameAs[Boolean, Boolean](_.head, LinearScan.first) _)
  }

  test("last") {
    forAll(sameAs[Boolean, Boolean](_.last, LinearScan.last) _)
  }

  test("findFirst") {
    forAll { (input: List[Int], pred: Int => Boolean) =>
      sameAs[Int, Option[Int]](_.filter(pred).headOption, LinearScan.findFirst(pred))(input)
    }
  }

  test("findLast") {
    forAll { (input: List[Int], pred: Int => Boolean) =>
      sameAs[Int, Option[Int]](_.filter(pred).lastOption, LinearScan.findLast(pred))(input)
    }
  }

  test("collect") {
    forAll(sameAs[Int, Seq[Int]](_.toSeq, LinearScan.collect) _)
  }

  test("stack overflow") {
    LinearScan.max.fold(Stream.fill(1000000)(1)) shouldBe Some(1)
  }

  test("lazyness") {
    var count = 0
    val scan = LinearScan.lift[Int, Int](i => { count = count + 1; i })
    scan.scan(Stream.fill(1000)(1)).take(10).foreach(i => {})
    count shouldBe 10
  }
}

object Utils {
  implicit val genLinearScan: Gen[LinearScan[Int, Int]] = {
    for {
      fun <- Gen.function2[Int, Int, (Int, Int)](Gen.zip(Gen.choose(-99, 99), Gen.choose(-99, 99)))
      init <- Gen.choose(-99, 99)
    } yield LinearScan(init)(fun)
  }

  implicit val arbLinearScan: Arbitrary[LinearScan[Int, Int]] = Arbitrary(genLinearScan)

}
