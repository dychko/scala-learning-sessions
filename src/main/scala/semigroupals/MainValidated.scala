package semigroupals

import cats.Semigroupal
import cats.data.Validated
import cats.implicits._
import cats.instances.list._
import cats.instances.int._
import cats.instances.string._
import cats.syntax.apply._
import cats.instances.all._
import cats.syntax.validated._
import cats.instances.invariant._

import scala.language.higherKinds
import scala.util.Try

object MainValidated extends App {

//  case class A(i: Int, j: String)
//
//  val f = (Future(1), Future("a")).mapN(A.apply) // A(1,a)
//
//  val t = (Option(123), Option("abc")).tupled // Option((123,abc))
//
//  val s = Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _) // Option(6)
//  println(s)
//  val z = Semigroupal[Option].product(Option(1), Option(3))
//  println(z)
//
//  println(Await.result(f, 1.seconds))

  // free of Monad => can accumulate errors
//  type AllErrors[A] = Validated[List[String], A]
//  Semigroupal[AllErrors].product(
//    Validated.invalid(List("Error1")),
//    Validated.invalid(List("Error2"))) // Invalid(List(Error1,Error2))

  // way to construct
  // apply
  Validated.Valid(123)
  Validated.Invalid(List("a")) // but this doesn't show explicitly the types
  // => often preferred
  Validated.valid[List[String], Int](1)
  Validated.invalid[List[String], Int](List("a"))
  // or extensions via implicits
  import cats.syntax.validated._
  1.valid[List[String]]
  List("a").invalid[Int] // similar to either

  import scala.io.Source
  // we also have helpers
  // we can catch exceptions with validated from exceptions, try, either, option
  val z1 = Validated.fromEither(Left("Bad")) // Invalid(Bad)
  val z2 = Validated.fromEither(Right(1)) // Valid(1)
  val z3 = Validated.fromTry(Try(new Exception("e"))) // ot just "a".toInt
  val z4 = Validated.catchOnly[NumberFormatException]("a".toInt) // Invalid(java.lang.NumberFormatException: For input string: "a") - it will catch it
//  val z5 = Validated.catchOnly[NumberFormatException](throw new RuntimeException(
//    "e")) // it will throw the exception so it won't wrap it in context
  println(z1, z2, z3, z4)

//  // combining instances of validated
//  // it needs semigroups in scrope cause that's how it combines them => cats.instances.list._
//  ("s".invalid[Int], "f".invalid[Int]).tupled
//  (List("s").invalid[Int], List("k").invalid[Int]).tupled

  // methods of validated
  val a = 100.valid[String].map(_ + 100) // Valid(200)
  println(a)
  val b = "a".invalid[Int].leftMap(_ + "b") // Invalid(ab)
  println(b)

  println(123.valid[String].bimap(_ + "error", _ + 100)) // Valid(223)
  println("a".invalid[Int].bimap(_ + "error", _ + 100)) // Invalid(aerror)

  // similar to flatMap -> it doesn't have flatMap implementation
  val m1 = 123.valid[String].andThen { a =>
    100.valid[String].map(_ + a)
  } // Valid(223)
  println(m1)

  val m2 = 123.valid[String].andThen { a =>
    "aaa".invalid[Int].map(_ + a)
  } // Invalid(aaa)
  println(m2)

  val m3 = 123.valid[String].andThen { a =>
    "aaa".invalid[Int].map(_ + a).andThen(a => "bbb".invalid[Int])
  } // Invalid(aaa) => stops at the first invalid like monad
  println(m3)

  // retrieveing and ensuring
  123.valid[String].ensure("Specific error")(_ > 0)

  "a".invalid[Int].getOrElse(19)

  "a".invalid[Int].fold(_ + "error", _ + 10)

  // conversion between validated and monads
  val t = 123.valid[String].toEither
  t.toValidated

  // example
  //  he name and age must be specified
  //  the name must not be blank
  //  the age must be a valid non-negave integer

  // we will make it all fail fast with Either and then use `toValidated` in a mapN + User.apply

  type FormData = Map[String, String]

  type FailSlow[A] = Validated[List[String], A]

  case class User(name: String, age: Int)

  def getValue(key: String)(form: FormData) =
    form.get(key).toRight(List("This key doesn't exist"))

  def getName = getValue("name") _
  def getAge = getValue("age") _

  val name = getName(Map("name" -> "John")) // Right(John)

  def parseInt(value: String) =
    Either
      .catchOnly[NumberFormatException](value.toInt)
      .leftMap(_ => List(s"The $value must be an integer"))

  // validations
  def nonBlank(value: String) =
    Right(value).ensure(List(s"The $value should not be blank"))(_.nonEmpty)

  def nonNegative(value: Int) =
    Right(value).ensure(List(s"The $value should not be negative"))(_ > 0)

  def readName(data: FormData) = getName(data).flatMap(nonBlank)

  def readAge(data: FormData) =
    getAge(data)
      .flatMap(nonBlank)
      .flatMap(parseInt)
      .flatMap(nonNegative) // we want to see where it fails so we flatmap them and get the exact place
  // e.g readAge(Map("age" -> "-1")) is our third validation and the output is Left(List(The -1 should not be negative))

  println(readAge(Map("age" -> "-1")))

  def readUser(data: FormData) =
    (readName(data).toValidated, readAge(data).toValidated).mapN(User.apply)

  println(readUser(Map("name" -> "John", "age" -> "1")))
}
