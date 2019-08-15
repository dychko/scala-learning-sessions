package semigroups

import cats.data.NonEmptyList
import cats.kernel.{Monoid, Semigroup}
import cats.implicits._

object MainSemigroup extends App {

  val result1: Int = Semigroup[Int].combine(1, 2)

  Semigroup.apply[Int]

  println(Semigroup.apply[NonEmptyList[Int]].combine(NonEmptyList(1, Nil), NonEmptyList(1, Nil)))

  println(result1)

}
