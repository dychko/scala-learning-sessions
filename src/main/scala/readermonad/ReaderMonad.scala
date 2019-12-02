package readermonad

import cats.data.Reader
import cats.implicits._

object ReaderMonad extends App {


  case class Cat(name: String, favoriteFood: String)

  // Cat => F[String]: Kleisli
  // Cat => String: Reader
  val catName: Reader[Cat, String] = Reader(cat => cat.name)

  println(catName.run(Cat("Garfield", "lasagne")))

  val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello, $name")

  val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed = for {
    greet <- greetKitty
    feed <- feedKitty
  } yield s"$greet. $feed"

  case class FeedAndGreet(result: String)

                     // Cat => String             // Cat => String   String => String
  val greetAndFeed2 = greetKitty.flatMap(greet => feedKitty.map(feed => FeedAndGreet(s"$greet. $feed")))

  println(greetAndFeed2.run(Cat("Heathcliff", "junk food")))
  println(greetAndFeed2.run(Cat("Garfield", "lasagne")))


  case class Db(usernames: Map[Int, String],
                passwords: Map[String, String])

  type ReaderDb[A] = Reader[Db, A]

  def findUsername(userId: Int): ReaderDb[Option[String]] = Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): ReaderDb[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))


  def checkLogin(userId: Int, password: String): ReaderDb[Boolean] = {
    for {
      usernameOpt <- findUsername(userId)
      passwordOk <- usernameOpt.map(username => checkPassword(username, password))
          .getOrElse(false.pure[ReaderDb])
    } yield passwordOk
  }

  val usernames = Map(1 -> "a", 2 -> "john", 3 -> "david")

  val passwords = Map("a" -> "123", "john" -> "chocolatetree", "david" -> "random")

  val db = Db(usernames, passwords)


  println(checkLogin(1, "123").run(db))
  println(checkLogin(3, "not-correct").run(db))
  println(checkLogin(4, "not-correct").run(db))

  println(catName)

}
