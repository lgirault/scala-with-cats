package gp

import cats.tests.CatsSuite
import gp.Chap3Functors.Trees._
import org.scalacheck.{Arbitrary, Gen}
import cats.laws.discipline.FunctorTests
import gp.Chap3Functors.{Box, Codecs, Print}
import org.scalatest.FunSuite

object Chap3TestSuite {


  implicit def arbTree[A: Arbitrary]: Arbitrary[Tree[A]] =
    Arbitrary(Gen.lzy(Gen.oneOf(
      for {e <- Arbitrary.arbitrary[A]} yield Leaf(e),
      for {
        t1 <- arbTree.arbitrary
        t2 <- arbTree.arbitrary
      } yield Branch(t1, t2)))
    )


}

import Chap3TestSuite.arbTree

class TreeFunctorLawTests extends CatsSuite {
  checkAll("Tree.FunctorLaws", FunctorTests[Tree].functor[Int, Int, String])
}

class FunctorTestSuite extends FunSuite {

  test("Printable") {
    import Print.format

    assert(format(Box("hello")) === "\"hello\"")
    assert(format(Box(true)) === "yes")
  }

  test("Codec") {
    import Codecs.encode
    import Codecs.decode
    assert(encode(123.4) === "123.4")
    assert(decode[Double]("123.4") === 123.4)

    assert(encode(Box(123.4)) === "123.4")
    assert(decode[Box[Double]]("123.4") === Box(123.4))
  }

}



