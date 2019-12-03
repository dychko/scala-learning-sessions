package writermonad

import cats.data.Writer

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import cats.syntax.writer._
import cats.instances.vector._
import cats.syntax.applicative._

object WriterMonadExercise extends App {

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  //  factorial(5)

  Await.result(Future.sequence(Vector(
    Future(factorial(3)),
    Future(factorial(3))
  )), 5.seconds)

  type Logged[A] = Writer[Vector[String], A]

  def factorialWriter(n: Int): Logged[Int] = {
    slowly(
      for {
        ans <- if (n == 1) 1.pure[Logged] else factorialWriter(n - 1).map(_ * n)
        _ <- Vector(s"fact $n $ans").tell
      } yield ans
    )
  }

  private val vector: Vector[Logged[Int]] =
    Await.result(Future.sequence(Vector(
      Future(factorialWriter(15)),
      Future(factorialWriter(2))
    )), 5.seconds)

  println(vector)

}
