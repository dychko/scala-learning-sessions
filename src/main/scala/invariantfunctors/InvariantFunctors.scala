package invariantfunctors

import cats.{Contravariant, Monoid, Show}
import cats.syntax.contravariant._
import cats.syntax.invariant._
import cats.instances.string._
import cats.syntax.semigroup._


object InvariantFunctors extends App {


  trait Codec[A] {
    def encode(value: A): String
    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = {
      val self = this
      new Codec[B] {
        override def encode(value: B): String = self.encode(enc(value))

        override def decode(value: String): B = dec(self.decode(value))
      }
    }

  }

  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)


  implicit val stringCodec: Codec[String] = new Codec[String] {
    override def encode(value: String): String = value

    override def decode(value: String): String = value
  }


  implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)


  println(encode(1))
  println(decode[Int]("1"))

  case class Money[A](value: A)

  implicit def moneyCodec[A](implicit c: Codec[A]): Codec[Money[A]] =
    c.imap[Money[A]](Money.apply, _.value)

  println(encode(Money(100)))
  println(decode[Money[String]]("10009"))

  val showString: Show[String] = Show[String]

  val showSymbol: Show[Symbol] = Contravariant[Show].contramap(showString)((s: Symbol) => s"'${s.name}")

  // Show[String],  f: Symbol => String,  Show[Symbol]


  println(showString.contramap((s: Symbol) => s"'${s.name}").show('world))

  println(showSymbol.show('hello))



  implicit  val monoidSymbol: Monoid[Symbol] =
    Monoid[String].imap[Symbol](Symbol.apply)(_.name)


  println(Monoid[Symbol].empty |+| 'say |+| 'hello)



}
