package co.movio.scan

import cats.implicits._

import org.scalacheck._

import org.scalatest._
import Matchers._
import org.scalatest.prop._
import org.scalatest.Assertions._

class ScanTest extends FunSuite with GeneratorDrivenPropertyChecks {
  import Utils._

  test("len l == len (scan l)") {
    forAll { (scan: Scan[Int, Int], input: List[Int]) =>
      assert(input.length == scan.scan(input.toStream).length)
    }
  }

  test("scan . dropRight 1 == dropRight 1 . scan") {
    forAll { (scan: Scan[Int, Int], input: List[Int]) =>
      assert(
        scan.scan(input.toStream).dropRight(1) ==
          scan.scan(input.dropRight(1).toStream)
      )
    }
  }

  test("Scans can be combine using `zip`") {
    forAll { (scan1: Scan[Int, Int], scan2: Scan[Int, Int], input: List[Int]) =>
      val zipped: Scan[Int, (Int, Int)] = Scan.zip(scan1, scan2)
      val r1: List[(Int, Int)] = zipped.scan(input.toStream).toList
      val r2: List[(Int, Int)] = scan1.scan(input.toStream).toList.zip(scan2.scan(input.toStream))
      assert(r1 == r2, s"$r1 != $r2")
    }
  }

  test("Scans can be chained using `andThen`") {
    forAll { (f: Scan[Int, Int], g: Scan[Int, Int], input: Stream[Int]) =>
      {
        whenever(input.nonEmpty) {
          val actual: Stream[Int] = (f andThen g).scan(input)
          val expected: Stream[Int] = g.scan(f.scan(input))
          assert(actual == expected)
        }
      }
    }
  }

  test("((scan1 |@| scan2) map (,)) xs == (scan1 xs, scan2 xs)") {
    forAll { (scan1: Scan[Int, Int], scan2: Scan[Int, Int], input: List[Int]) =>
      val zipped = (scan1 |@| scan2) map Tuple2.apply
      val r1: List[(Int, Int)] = zipped.scan(input.toStream).toList
      val r2: List[(Int, Int)] = scan1.scan(input.toStream).toList.zip(scan2.scan(input.toStream))
      assert(r1 == r2, s"$r1 != $r2")
    }
  }

  test("f >>> g == g.scan . f.scan") {
    forAll { (f: Scan[Int, Int], g: Scan[Int, Int], input: Stream[Int]) =>
      {
        whenever(input.nonEmpty) {
          val actual: Stream[Int] = (f >>> g).scan(input)
          val expected: Stream[Int] = g.scan(f.scan(input))
          assert(actual == expected)
        }
      }
    }
  }

  test("last . scan == foldLeft") {
    forAll { (init: Int, step: (Int, Int) => (Int, Int), input: List[Int]) =>
      whenever(input.nonEmpty) {
        val scan = Scan(init)(step)
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
  def sameAs[T: Arbitrary, R](f: List[T] => R, scan: Scan[T, R])(input: List[T]) =
    whenever(input.nonEmpty) {
      val actual = scan.fold(input.toStream).get
      val expected = f(input)
      assert(actual === expected)
    }

  test("sum") {
    forAll(sameAs[Int, Int](_.sum, Scan.sum) _)
  }

  test("combine") {
    forAll(sameAs[Int, Int](_.sum, Scan.combine) _)
  }

  test("count") {
    forAll { (input: List[Int], pred: Int => Boolean) =>
      sameAs[Int, Int](_.filter(pred).length, Scan.count(pred))(input)
    }
  }

  test("previous") {
    forAll { input: List[Boolean] =>
      whenever(input.length > 1) {
        sameAs[Boolean, Option[Boolean]](_.dropRight(1).lastOption, Scan.previous)(input)
      }
    }
  }

  test("mean") {
    forAll(sameAs[Double, Double](i => i.sum / i.length, Scan.mean) _)
  }

  test("min") {
    forAll(sameAs[Int, Int](_.min, Scan.min) _)
  }

  test("max") {
    forAll(sameAs[Int, Int](_.max, Scan.max) _)
  }

  test("first") {
    forAll(sameAs[Boolean, Boolean](_.head, Scan.first) _)
  }

  test("last") {
    forAll(sameAs[Boolean, Boolean](_.last, Scan.last) _)
  }

  test("findFirst") {
    forAll { (input: List[Int], pred: Int => Boolean) =>
      sameAs[Int, Option[Int]](_.filter(pred).headOption, Scan.findFirst(pred))(input)
    }
  }

  test("findLast") {
    forAll { (input: List[Int], pred: Int => Boolean) =>
      sameAs[Int, Option[Int]](_.filter(pred).lastOption, Scan.findLast(pred))(input)
    }
  }

  test("collect") {
    forAll(sameAs[Int, Seq[Int]](_.toSeq.reverse, Scan.collect) _)
  }

  test("stack overflow") {
    Scan.max.fold(Stream.fill(1000000)(1)) shouldBe Some(1)
  }

  test("laziness") {
    var count = 0
    val scan = Scan.lift[Int, Int](i => { count = count + 1; i })
    scan.scan(Stream.fill(1000)(1)).take(10).foreach(i => {})
    count shouldBe 10
  }
}

object Utils {
  implicit val genScan: Gen[Scan[Int, Int]] = {
    for {
      fun <- Gen.function2[Int, Int, (Int, Int)](Gen.zip(Gen.choose(-99, 99), Gen.choose(-99, 99)))
      init <- Gen.choose(-99, 99)
    } yield Scan(init)(fun)
  }

  implicit val arbScan: Arbitrary[Scan[Int, Int]] = Arbitrary(genScan)

}
