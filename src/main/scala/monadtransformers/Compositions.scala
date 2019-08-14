package monadtransformers

import cats.{Functor, Monad}
import cats.implicits._

object Compositions extends App {

  val o1 = Option[Int](1)
  val l1 = List[String]("hello")


  Option[Int](1).map(_ => -1)
  List[Int](2).map(_ => -2)

  val x: Option[List[Int]] = Some(List(1, 2, 3)).map(x => x ++ List(4))

  val optionListF = Functor[Option].compose(Functor[List])

  val data: Option[List[Int]] = Some(List(1,2,3))

  println(optionListF.map(data)(_ + 2))

  Monad[Option].compose(Monad[List])

}
