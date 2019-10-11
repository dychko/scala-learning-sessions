package monads

import scala.util.Try

object Monads extends App {

  def parseInt(stringValue: String): Option[Int] = {
    Try(stringValue.toInt).toOption
  }

  def divideInts(a: Int, b: Int): Option[Int] = {
    if (b == 0) None else Some(a / b)
  }

  def divideStrings(stringA: String, stringB: String): Option[Int] = {
    parseInt(stringA).flatMap { parsedA =>
      parseInt(stringB).flatMap { parsedB =>
        divideInts(parsedA, parsedB)
      }
    }
  }

  def divideStringsWithComprehension(stringA: String, stringB: String): Option[Int] = {
    for {
      parsedA <- parseInt(stringA)
      parsedB <- parseInt(stringB)
      result <- divideInts(parsedA, parsedB)
    } yield result
  }

  println(divideStrings("123", "0"))
  println(divideStrings("123", "3"))
  println(divideStringsWithComprehension("123", "0"))
  println(divideStringsWithComprehension("123", "3"))

  // pure: A => F[A]
  // flatMap: F[A] => (A => F[B]) => F[B]
  // (F[A], A => F[B]) => F[B]
  trait Monad[F[_]] {
    def pure[A](value: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(x => pure(func(x)))
  }

  object OptionalMonad extends Monad[Option] {
    override def pure[A](value: A): Option[A] = {
      Option(value)
    }

    override def flatMap[A, B](value: Option[A])(func: A => Option[B]): Option[B] = {
      value match {
        case Some(a) => func(a)
        case None => None
      }
    }
  }

  private val option1 = OptionalMonad.pure(1)
  println(OptionalMonad.map(option1)(x => x + 5))
}

