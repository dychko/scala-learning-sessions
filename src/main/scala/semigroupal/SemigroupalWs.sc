import cats.Semigroupal
import cats.syntax.either._
import cats.instances.option._
import cats.syntax.apply._


def parseInt(str: String): Either[String, Int] = {
  Either.catchOnly[NumberFormatException](str.toInt)
    .leftMap(_ => s"Couldn't read a string: $str")
}

for {
  a <- parseInt("a")
  b <- parseInt("b")
  c <- parseInt("c")
} yield (a + b + c)

// context1.flatMap(value1 => context2)

//  trait Semigroupal[F[_]] {
//    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
//  }

Semigroupal[Option].product(Some(1), Some("hello"))
Semigroupal[Option].product(Some(2), None)

Semigroupal.tuple3(Option(1), Option(2), Option(3))

Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)

(Option(123), Option("abc")).tupled

case class Cat(name: String, born: Int, color: String)

(Option("Garfield"),
  Option(1978),
  Option("Orange & black")
  ).mapN(Cat.apply)