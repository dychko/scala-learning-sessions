package statemonad

import cats.data.State
import cats.implicits._

object StateMonad extends App {
  // S => (S, A)
  val s = State[Int, String] { state =>
    val current = state * 100
    (current, s"String $current")
  }

  println(s.run(10).value)
  println(s.runS(10).value)
  println(s.runA(10).value)

  val s1 = State[Int, String] { state =>
    val current = state * 2
    (current, s"String $current")
  }

  val s2 = State[Int, String] { state =>
    val current = state * 100
    (current, s"String $current")
  }

  val composition = for {
    r1 <- s1
    r2 <- s2
  } yield (r1, r2)

  println(composition.run(100).value)

  println(State.pure[Int, String]("String").run(12).value)
  println(State.set[Int](20).run(12).value)
  println(State.modify[Int](_ + 200).run(12).value)
  println(State.inspect[Int, String](_ + "200").run(12).value)

  object Calculator {
    // 1 2 + 3 * = (1 + 2) * 3
    type CalcState[A] = State[List[Int], A]

    def evalOne(in: String) = in match {
      case "+" => operation((o1, o2) => o1 + o2)
      case "-" => operation(_ - _)
      case "/" => operation(_ / _)
      case "*" => operation(_ * _)
      case num => operand(num.toInt)
    }

    def operand(num: Int) = State[List[Int], Int] { state =>
      (num :: state, num)
    }

    def operation(f: (Int, Int) => Int) = State[List[Int], Int] { state =>
      state match {
        case n1 :: n2 :: tail =>
          val result = f(n1, n2)
          (result :: tail, result)
      }
    }

    def evalAll(in: List[String]) = in.foldLeft(State.pure[List[Int], Int](0)) {
      (acc: State[List[Int], Int], next: String) =>
        acc.flatMap(_ => evalOne(next))
    }
  }

  println(
    Calculator.evalAll(List("1", "2", "+", "3", "4", "+", "*")).runA(Nil).value)
}
