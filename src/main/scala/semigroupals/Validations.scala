package semigroupals

import cats.data.Validated
import cats.syntax.either._
import cats.syntax.apply._
import cats.instances.list._
import cats.instances.string._
import cats.instances.either._
import cats.syntax.validated._

import scala.util.Try

object Validations extends App {
  Validated.Valid(1)
  Validated.Invalid("A")

  val v = 1.valid[String]
  val i = "a".invalid[Int]

  println((List("a").invalid[Int], List("v").invalid[Int]).tupled)

  println((1.valid[String], 2.valid[String]).tupled)

  v.map(_ + 20)
  i.leftMap(_ + "error")
  v.bimap(_ + "error", _ + 20)
  v.getOrElse(10)
  i.ensure("error")(_ > 10)

  println(Validated.fromEither(Left("a")))
  println(Validated.fromEither(Right("a")))
  println(Validated.fromTry(Try("a".toInt)))

  i.andThen(a => 10.valid[String].map(_ + a))

  // name is not empty
  // age is not negative

  type FormData = Map[String, String]

  def getValue(key: String)(formData: FormData) =
    formData.get(key).toRight("Missing key").toValidatedNel

  def getName = getValue("name") _
  def getAge = getValue("age") _

  def parseInt(value: String) =
    Either
      .catchOnly[NumberFormatException](value.toInt)
      .leftMap(_ => "Age needs to be a valid int")
      .toValidatedNel

  def nonEmpty(value: String) =
    Right(value).ensure("Emtpy")(_.nonEmpty).toValidatedNel

  def nonNegative(value: Int) =
    Right(value).ensure("Negative")(_ > 0).toValidatedNel

  def readName(formData: FormData) = getName(formData).andThen(nonEmpty)

  def readAge(formData: FormData) =
    getAge(formData).andThen(nonEmpty).andThen(parseInt).andThen(nonNegative)

  case class User(name: String, age: Int)
  val t = Map("name" -> "", "age" -> "J")
  println((readName(t), readAge(t)).mapN(User.apply))
}
