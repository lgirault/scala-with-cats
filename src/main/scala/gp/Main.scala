package gp

import cats._
import cats.implicits._
final case class Cat(name: String, age: Int, color: String)

object Main extends App {

  implicit val catEq : Eq[Cat] = (c1 : Cat, c2 : Cat) => c1 == c2


  val tiger = Cat("Tiger", 3, "Brown and Orange")

  val princess = Cat("Princess", 8, "Snow white")


  val cat1 = Cat("Garfield",38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(cat1 === cat2 )

  println(optionCat1 === optionCat2 )


}
