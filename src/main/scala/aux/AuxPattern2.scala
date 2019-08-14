package aux

object AuxPattern2 extends App {

  trait Last[A] {
    type B

    def last(a: A): B
  }

  object Last {

    type Aux[A, B0] = Last[A] {type B = B0}


    implicit def tuple1Last[A]: Aux[Tuple1[A], A] = new Last[Tuple1[A]] {
      type B = A

      def last(a: Tuple1[A]): A = a._1
    }

    implicit def tuple2Last[A, C]: Aux[(A, C), C] = new Last[(A, C)] {
      type B = C

      def last(a: (A, C)): C = a._2
    }
  }

  def sort[A, B](list: List[A])(implicit last: Last.Aux[A, B], ord: Ordering[B]): List[A] = list.sortBy(last.last)


  val list = List((3, 4), (1, 2))
  println(sort(list))

}