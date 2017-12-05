package gp

import cats.data.{Reader, Writer}
import cats.syntax.applicative._
import cats.{Eq, Eval, Id, Monad}
import cats.syntax.option._
import cats.syntax.eq._

import scala.annotation.tailrec

object Chap4Monads {

  object SimpleMonad {

    trait Monad[F[_]] {
      def pure[A](a: A): F[A]

      def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

      def map[A, B](value: F[A])(func: A => B): F[B] =
        flatMap(value)(func andThen pure)
    }


    val idMonad = new Monad[Id] {

      def pure[A](a: A): Id[A] = a

      def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)

    }

  }

  def foldRightUnsafe[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRightUnsafe(tail, acc)(fn))
      case Nil =>
        acc
    }


  def foldRightSafe[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    //    val app = Applicative[Eval]
    //    val evalFn = app.pure(fn)

    def aux(as: List[A], acc: Eval[B]): Eval[B] =
      as match {
        case head :: tail =>
          Eval.defer(aux(tail, acc)) map (fn(head, _))
        case Nil => acc
      }

    aux(as, Eval.now(acc)).value
  }

  object Writers {

    def slowly[A](body: => A) =
      try body finally Thread.sleep(10)

    def factorial(n: Int): Int = {
      val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
      println(s"fact $n $ans")
      ans
    }

    type Logged[A] = Writer[Vector[String], A]


    import cats.instances.vector._
    import cats.syntax.applicative._ // for pure
    import cats.syntax.writer._ // for tell


    def loggeFactorial(n: Int): Writer[String, Int] = {

      def aux(n: Int): Logged[Int] = {

        for {
          ans <- slowly[Logged[Int]] {
            if (n == 0) 1.pure[Logged] else aux(n - 1) map (_ * n)
          }
          _ <- Vector(s"fact $n $ans").tell
        } yield ans
      }

      aux(n).mapWritten(log => log.mkString("\n"))
    }
  }

  case class Db(
                 usernames: Map[Int, String],
                 passwords: Map[String, String]
               )

  object Readers {

    type DbReader[T] = Reader[Db, T]


    def findUsername(userId: Int): DbReader[Option[String]] =
      Reader((db: Db) => db.usernames get userId)

    def checkPassword(
                       username: String,
                       password: String): DbReader[Boolean] =
      Reader((db: Db) => db.passwords get username contains password)

    def checkLogin(
                    userId: Int,
                    password: String): DbReader[Boolean] = {
      findUsername(userId).flatMap {
        nameOpt =>
          nameOpt.map(checkPassword(_, password)).getOrElse(Reader(_ => false))
      }
    }

    def checkLogin2(
                     userId: Int,
                     password: String): DbReader[Boolean] = {
      import cats.data.OptionT

      val checked: OptionT[DbReader, Boolean] = for {
        name <- OptionT(findUsername(userId))
        check <- OptionT.liftF(checkPassword(name, password))
      } yield check

      checked getOrElseF Reader(_ => false)
    }

  }

  object SuperCalculator {

    import cats.data.State

    type CalcState[A] = State[List[Int], A]

    def evalOne(sym: String): CalcState[Int] = State(stack =>

      try {
        val n = sym.toInt
        (n :: stack, n)
      }
      catch {
        case _: NumberFormatException =>
          val op: (Int, Int) => Int =
            sym match {
              case "+" => (x, y) => x + y
              case "*" => (x, y) => x * y
              case _ => throw new IllegalArgumentException()
            }

          val n1 :: n2 :: remaining = stack
          val res = op(n1, n2)
          (res :: remaining, res)
      }
    )

    def evalAll(input: List[String]): CalcState[Int] =
      input.foldLeft(0.pure[CalcState]) {
        case (state, sym) =>
          for {
            _ <- state
            x <- evalOne(sym)
          } yield x
      }

  }


  object Trees {

    sealed trait Tree[+A]

    final case class Branch[A](left: Tree[A], right: Tree[A])
      extends Tree[A]

    final case class Leaf[A](value: A) extends Tree[A]

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](value: A): Tree[A] =
      Leaf(value)


    object Tree {

      implicit def treeEq[A](implicit equ: Eq[A]): Eq[Tree[A]] =
        new Eq[Tree[A]] {
          def eqv(x: Tree[A], y: Tree[A]): Boolean = (x, y) match {
            case (Leaf(v1), Leaf(v2)) => v1 === v2
            case (Branch(b1l, b1r), Branch(b2l, b2r)) =>
              b1l === b2l && b1r === b2r
            case _ => false
          }
        }

      implicit val monad:Monad[Tree] = new Monad[Tree] {

        def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
          case Leaf(a) => f(a)
          case Branch(a1, a2) => Branch(flatMap(a1)(f), flatMap(a2)(f))
        }

        case class TreeZipper[+A, +B](sibling: Either[Tree[B], Tree[Either[A, B]]], previous: Option[TreeZipper[A, B]] = None)

        def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {

          @tailrec
          def aux(t: Option[Tree[Either[A, B]]],
                  zip: Option[TreeZipper[A, B]]): Tree[B] =

            if (t.isEmpty)
              zip.get match {
                case TreeZipper(Left(result), None) => result
                case TreeZipper(Left(right), Some(TreeZipper(Left(left), prev))) =>
                  aux(none, TreeZipper(Left(Branch(left, right)), prev).some)
                case TreeZipper(Left(done), Some(TreeZipper(Right(todo), prev))) =>
                  aux(Some(todo), TreeZipper(Left(done), prev).some)

                case TreeZipper(Right(result), _) => throw new Error("invalid state")
              }
            else t.get match {
              case Leaf(Right(b)) => aux(None, TreeZipper(Left(Leaf(b)), zip).some)
              case Leaf(Left(a1)) => aux(Some(f(a1)), zip)
              case Branch(x, y) => aux(Some(x), TreeZipper(Right(y), zip).some)

            }

          aux(Some(f(a)), None)
        }

        def pure[A](x: A): Tree[A] = leaf(x)
      }
    }
  }

}
