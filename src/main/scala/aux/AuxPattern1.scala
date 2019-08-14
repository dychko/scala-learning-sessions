package aux

import scala.language.higherKinds

object AuxPattern1 extends App {

  trait Unwrap[T[_], R] {
    type Out

    def apply(tr: T[R]): Out
  }


  // T[R] - for example: List[String], List[Int]

  def extractor[T[_], R](in: T[R])(implicit unwrap: Unwrap[T, R]): unwrap.Out = {
    unwrap(in)
  }


  object Unwrap {

    implicit object listStringSize extends Unwrap[List, String] {
      type Out = Int

      def apply(tr: List[String]): Int = tr.size
    }

    implicit object listIntMax extends Unwrap[List, Int] {
      type Out = Int

      def apply(tr: List[Int]): Int = tr.max
    }

  }

  trait Printer[T] {
    def apply(t: T): (String, T)
  }

  object Printer {

    implicit object stringPrinter extends Printer[String] {
      def apply(s: String): (String, String) = ("String: " + s, s)
    }

    implicit object intPrinter extends Printer[Int] {
      def apply(i: Int): (String, Int) = ("Int: " + i, i)
    }

  }

  // Doesn't work
  //  def extractor2[T[_], R](in: T[R])
  //                         (implicit unwrap: Unwrap[T, R], withPrinter: Printer[unwrap.Out]): (String, unwrap.Out) = {
  //    withPrinter(unwrap(in))
  //  }


  // Aux Pattern - Explicit implementation

  trait UnwrapAux[T[_], R, Out] {
    def apply(tr: T[R]): Out
  }


  object UnwrapAux {

    implicit object listStringSize extends UnwrapAux[List, String, Int] {
      def apply(tr: List[String]): Int = tr.size
    }

    implicit object listIntMax extends UnwrapAux[List, Int, Int] {
      def apply(tr: List[Int]): Int = tr.max
    }

  }

  def extractor2[T[_], R, Out](in: T[R])(
    implicit
    unwrap: UnwrapAux[T, R, Out],
    withPrinter: Printer[Out]
  ): (String, Out) = {
    withPrinter(unwrap(in))
  }

  object Unwrap2 {
    implicit def unwrap[T[_], R, Out0](implicit unwrapAux: UnwrapAux[T, R, Out0]) = new Unwrap[T, R] {
      type Out = Out0

      def apply(tr: T[R]): Out = unwrapAux(tr)
    }
  }


  // Aux Pattern - Concise implementation


  object Unwrap3 {

    type Aux[T[_], R, Out0] = Unwrap[T, R] {
      type Out = Out0
    }

    implicit object listStringSize extends Unwrap[List, String] {
      type Out = Int

      def apply(tr: List[String]): Int = tr.size
    }

    implicit object listIntMax extends Unwrap[List, Int] {
      type Out = Int

      def apply(tr: List[Int]): Int = tr.max
    }

  }

  def extractor3[T[_], R, Out](in: T[R])(
    implicit
    unwrap: Unwrap3.Aux[T, R, Out],
    withPrinter: Printer[Out]
  ): (String, Out) = {
    withPrinter(unwrap(in))
  }

}
