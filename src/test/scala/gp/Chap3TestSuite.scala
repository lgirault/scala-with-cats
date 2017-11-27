package gp

import cats.tests.CatsSuite
import gp.Chap3Functor.Trees._
import org.scalacheck.{Arbitrary, Gen}
import cats.laws.discipline.FunctorTests

object Chap3TestSuite {


  implicit def arbTree[A: Arbitrary]: Arbitrary[Tree[A]] =
    Arbitrary(Gen.lzy(Gen.oneOf(
      for {e <- Arbitrary.arbitrary[A] } yield Leaf(e),
      for {
        t1 <- arbTree.arbitrary
        t2 <- arbTree.arbitrary
      } yield Branch(t1,t2) ))
    )


}

import Chap3TestSuite.arbTree

class TreeLawTests extends CatsSuite {
  checkAll("Tree.FunctorLaws", FunctorTests[Tree].functor[Int, Int, String])
}