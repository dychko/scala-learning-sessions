package semigroups

import cats.data.NonEmptyList
import cats.kernel.{Monoid, Semigroup}
import cats.implicits._

object MainSemigroup extends App {

  val result1: Int = Semigroup[Int].combine(1, 2)

  Semigroup.apply[Int]

  println(Semigroup.apply[NonEmptyList[Int]].combine(NonEmptyList(1, Nil), NonEmptyList(1, Nil)))

  println(result1)


  trait BooleanSemigroup extends Semigroup[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  implicit val booleanAndSemigroup: Semigroup[Boolean] = new BooleanSemigroup {}

  implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] with BooleanSemigroup  {
    override def empty: Boolean = true
  }

  val result2 = Semigroup[Boolean].combine(true, true)
  println("Boolean combine: " + result2)

  val result3 = Monoid[Boolean].combine(false, Monoid[Boolean].empty)
  println("Boolean monoid combine with empty: " + result3)


  case class Cocktail(ingredient: List[String], color: String)

  implicit val cocktailMonoid: Monoid[Cocktail] = new Monoid[Cocktail] {
    override def empty: Cocktail = Cocktail(ingredient = List.empty, color = "transparent")

    override def combine(x: Cocktail, y: Cocktail): Cocktail = {
      Cocktail(
        ingredient = x.ingredient |+| y.ingredient,
        color = y.color
      )
    }
  }


  val vodka = Cocktail(ingredient = List("vodka"), color = "transparent")
  val redBull = Cocktail(ingredient = List("red bull"), color = "pink")
  val cokeAndRum = Cocktail(ingredient = List("coke", "rum"), color = "brown")

  val fancyCocktail1 = (vodka |+| redBull)|+| cokeAndRum
  val fancyCocktail2 = vodka |+|(redBull|+| cokeAndRum)

  println(fancyCocktail1)
  println(fancyCocktail2)

}
