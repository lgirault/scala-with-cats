package gp


import cats.Monad
import cats.laws.discipline.{FunctorTests, MonadTests}
import cats.tests.CatsSuite
import org.scalatest.FunSuite
import gp.Chap4Monads.Trees._
import org.scalacheck.{Arbitrary, Gen}

class Chap4TestSuite extends FunSuite {

  test("Eval") {
    val l = Range(1, 10000).toList
    //println(Chap4Monads.foldRightUnsafe(l, BigInt(1))( _ * _))
    println(Chap4Monads.foldRightSafe(l, BigInt(1))(_ * _))
  }

  test("Writer") {
    import Chap4Monads.Writers._
    println(loggeFactorial(10).written)
  }

  test("Reader") {
    import Chap4Monads.Readers._
    import gp.Chap4Monads.Db

    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )
    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret")

    val db = Db(users, passwords)
    assert(checkLogin(1, "zerocool").run(db) === true)
    assert(checkLogin(4, "davinci").run(db) === false)

  }

  test("Super calculator") {
    import gp.Chap4Monads.SuperCalculator._

    assert(evalOne("42").runA(Nil).value === 42)


    val program = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    } yield ans

    assert(program.runA(Nil).value === 3)


    val program2 = evalAll(List("1", "2", "+", "3", "*"))
    assert(program2.runA(Nil).value === 9)

  }

  test("trees") {


    //import cats.syntax.monad._
    import cats.syntax.functor._ // map

    val t1 = Monad[Tree].pure(1)

    assert(t1.map(_.toString) === leaf("1"))
  }
}

class TreeMonadLawTests extends CatsSuite {


  implicit def arbTree[A: Arbitrary]: Arbitrary[Tree[A]] =
    Arbitrary(Gen.lzy(Gen.frequency(
      (4, for {e <- Arbitrary.arbitrary[A]} yield Leaf(e)),
      (3, for {
        t1 <- arbTree.arbitrary
        t2 <- arbTree.arbitrary
      } yield Branch(t1, t2))))
    )

  checkAll("Tree.FunctorLaws", FunctorTests[Tree].functor[Int, Int, String])
  checkAll("Tree.MonadLaws", MonadTests[Tree].monad[Int, Int, String])
}


class LazyTestSuite extends FunSuite {

  var count: Int = 0

  lazy val hello: String = {
    count += 1

    "hello"
  }

  test("lazy") {
    assert(count === 0)
    println(hello)
    println(hello)
    assert(count === 1)
  }

  var count2: Int = 0

  def hello2: String = {
    count2 += 1
    "hello2"
  }

  def callByValue(x: String): Unit = {
    println(x)
    println(x)
  }

  def callByName(x: => String): Unit = {
    println(x)
    println(x)
  }


  test("callByName") {
    assert(count2 === 0)
    callByValue(hello2)
    assert(count2 === 1)
    callByName(hello2)
    assert(count2 === 3)
  }

  var count3: Int = 0

  lazy val hello3: String = {
    count3 += 1

    "hello3"
  }

  test("mixing lazy and call by value") {
    assert(count3 === 0)
    callByName(hello3)
    assert(count3 === 1)
  }

}