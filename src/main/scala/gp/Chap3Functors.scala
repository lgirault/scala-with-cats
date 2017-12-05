package gp

//import cats.Functor
//import cats._
//import cats.instances.function.catsStdInstancesForFunction1
//import cats.implicits._
import cats.{Eq, Functor}
import cats.instances.function._
import cats.syntax.functor._
import cats.syntax.eq._

object Chap3Functors {

  val f =
    ((x: Int) => x.toDouble) map
      (_ + 1) map
      (_ * 2)


  object Trees {

    sealed trait Tree[+A]

    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

    final case class Leaf[A](value: A) extends Tree[A]

    def leaf[A](value: A): Tree[A] = Leaf(value)

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

  object Print {


    object Printable {

      implicit def boxPrintable[A](implicit printer: Printable[A]): Printable[Box[A]] =
        printer.contramap((b: Box[A]) => b.value)

      implicit val stringPrintable: Printable[String] =
        new Printable[String] {
          def format(value: String): String =
            "\"" + value + "\""
        }
      implicit val booleanPrintable: Printable[Boolean] =
        new Printable[Boolean] {
          def format(value: Boolean): String =
            if (value) "yes" else "no"
        }
    }

    trait Printable[A] {
      self =>
      def format(value: A): String

      def contramap[B](func: B => A): Printable[B] =
        new Printable[B] {
          def format(value: B): String =
            self.format(func(value))
        }
    }

    def format[A](value: A)(implicit p: Printable[A]): String =
      p.format(value)


  }

  final case class Box[A](value: A)


  object Codecs {

    trait Codec[A] {
      self =>
      def encode(value: A): String

      def decode(value: String): A

      def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
        def encode(value: B): String = self.encode(enc(value))

        def decode(value: String): B = dec(self.decode(value))
      }
    }

    object Codec {
      implicit val stringCodec: Codec[String] =
        new Codec[String] {
          def encode(value: String): String = value
          def decode(value: String): String = value
        }


      implicit val intCodec: Codec[Int] =
        stringCodec.imap(_.toInt, _.toString)

      implicit val booleanCodec: Codec[Boolean] =
        stringCodec.imap(_.toBoolean, _.toString)

      implicit val doubleCodec: Codec[Double] =
        stringCodec.imap(_.toDouble, _.toString)

      implicit def boxCode[A](implicit codec: Codec[A]): Codec[Box[A]] =
        codec.imap(Box.apply, _.value)
    }


    def encode[A](value: A)(implicit c: Codec[A]): String =
      c.encode(value)

    def decode[A](value: String)(implicit c: Codec[A]): A =
      c.decode(value)

  }

}
