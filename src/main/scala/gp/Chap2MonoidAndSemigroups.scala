package gp

import cats._
import cats.implicits._

object Chap2MonoidAndSemigroups {

  implicit val orMonoid: Monoid[Boolean] = new Monoid[Boolean] {

    def empty: Boolean = false

    def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  implicit val andMonoid: Monoid[Boolean] = new Monoid[Boolean] {

    def empty: Boolean = true

    def combine(x: Boolean, y: Boolean): Boolean = x && y
  }


  implicit def unionMonoid[T]: Monoid[Set[T]] = new Monoid[Set[T]] {

    def empty: Set[T] = Set.empty

    def combine(x: Set[T], y: Set[T]): Set[T] = x union y
  }

  object SuperAdder {

    def add[T[_], E](items: T[E])
                    (implicit t: Traverse[T],
                     m: Monoid[E]): E = t.fold(items)

  }



  def main(args: Array[String]): Unit = {

    val intOptRes = 5.some |+| 6.some |+|
      none[Int] |+| Monoid[Option[Int]].empty

    println(intOptRes)

    println(SuperAdder.add(List(1, 3, 5))(Traverse[List], Monoid[Int]))

    val t1 = (1, 5, 6)
    val t2 = (5, 1, 0)
    println(t1 |+| t2)
  }

}
