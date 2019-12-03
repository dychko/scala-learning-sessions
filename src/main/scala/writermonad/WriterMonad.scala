package writermonad

import cats.Id
import cats.data.{Writer, WriterT}
import cats.syntax.writer._
import cats.instances.vector._
import cats.syntax.applicative._

object WriterMonad extends App {

  Writer(Vector("step1", "step2"), 42)

  type Logged[A] = Writer[Vector[String], A]

  private val pureWriter: Logged[Int] = 42.pure[Logged]
  println(s"Pure: $pureWriter")

  private val tellWriter: Writer[Vector[String], Unit] = Vector("bla").tell
  println(s"TellWriter: $tellWriter")

  private val writer1: Writer[Vector[String], Int] = 42.writer(Vector("bla"))
  println(writer1.run)

  private val flatmapped: WriterT[Id, Vector[String], Int] = writer1.flatMap(_ => 43.writer(Vector("bla2")))
  println(flatmapped)

  private val mapped: WriterT[Id, Vector[String], Int] = writer1.map(x => x + 10)
  println(mapped)

  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100
  )

  val writer4 = writer1.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val res2 = res * 1000
    (log2, res2)
  }

  val writer5 =
    for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b", "c").tell
      b <- 32.writer(Vector("x", "y", "z"))
    } yield a + b

  val writer6 = 10.pure[Logged]
    .flatMap(a =>
      Vector.apply("a", "b", "c").tell
        .flatMap(_ =>
          32.writer(Vector("x", "y", "z"))
            .map(b => a + b)
        )
    )

  println(writer6.run)
}
