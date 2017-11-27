package gp

//import cats.Functor
//import cats._
//import cats.instances.function.catsStdInstancesForFunction1
//import cats.implicits._
import cats.{Eq, Functor}
import cats.instances.function._
import cats.syntax.functor._
import cats.syntax.eq._

object Chap3Functor {

  val f =
    ((x: Int) => x.toDouble) map
      (_ + 1) map
      (_ * 2)


  object Trees {

    sealed trait Tree[+A]

    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    def branch[A](left: Tree[A], right: Tree[A]) : Tree[A] = Branch(left,right)

    final case class Leaf[A](value: A) extends Tree[A]

    def leaf[A](value : A): Tree[A] = Leaf(value)

    implicit def treeEq[A](implicit equ: Eq[A]): Eq[Tree[A]] =
      new Eq[Tree[A]] {
        def eqv(x: Tree[A], y: Tree[A]): Boolean = (x, y) match {
          case (Leaf(v1), Leaf(v2)) => v1 === v2
          case (Branch(b1l, b1r), Branch(b2l, b2r)) =>
            b1l === b2l && b1r === b2r
          case _ => false
        }
      }

    implicit val functor: Functor[Tree] = new Functor[Tree] {
      def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
        fa match {
          case Leaf(a) => Leaf(f(a))
          case Branch(a1, a2) =>
            Branch(map(a1)(f), map(a2)(f))
        }
    }



    val _ = branch(leaf(2), leaf(3)) map (_ * 2)
  }

}
