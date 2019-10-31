println("hello")


// eager, memoized
val x = {
  println("Computing X")
  math.random
}

x

x

// lazy, not memoized
def y = {
  println("Computing Y")
  math.random
}

y // first access

y // second access

// lazy, memoized
lazy val z = {
  println("Computing z")
  math.random
}

z // first

z // second

import cats.Eval

val xEval = Eval.now(1000 + math.random)
val yEval = Eval.always(2000 + math.random)
val zEval = Eval.later(3000 + math.random)

yEval.value
yEval.value

zEval.value
zEval.value

//val hello = Eval.now {
//  println("Step 1"); "hello"
//}.map { s => println("Step 2"); s"$s world"}
//
//
//hello.value

val sum = for {
  s1 <- Eval.now {
    println("Step 1"); 5
  }
  s2 <- Eval.always {
    println("Step 2"); math.random
  }
} yield {
  s1 + s2
}

sum.value



val memoized = Eval.now {
  println("Step 1 memoized");
  "hello"
}
  .flatMap { x =>
    Eval.always {
      println("Step 2 memoized");
      x + " world"
    }
  }.memoize.flatMap { y =>
  Eval.always {
    println("Step 3 not memoized");
    y + " again!"
  }
}

memoized.value
memoized.value


def factorial(n: Int): Eval[BigInt] = {
  println(s"factorial($n)")
  if (n == 0) Eval.later(1) else Eval.later(factorial(n - 1)).flatMap { v =>
    v.flatMap { vv =>
      println(vv * n)
      Eval.now(vv * n)

    }
  }
}

factorial(3).value

//factorial(50000).value

