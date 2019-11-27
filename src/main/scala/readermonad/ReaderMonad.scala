package readermonad

import cats.data.Reader
import cats.implicits._

object ReaderMonad extends App {

  case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] = Reader(cat => cat.name)

  catName.run(Cat("Garfield", "lasagne"))

  val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")

  println(greetKitty.run(Cat("Heathcliff", "junk food")))

  val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet, $feed"

  println(greetAndFeed(Cat("Garfield", "lasagne")))
  println(greetAndFeed(Cat("Heathcliff", "junk food")))


  case class Db(
                 usernames: Map[Int, String],
                 passwords: Map[String, String]
               )


  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = {
    Reader(db => db.usernames.get(userId))
  }

  def checkPassword(username: String, password: String): DbReader[Boolean] = {
    Reader(db => db.passwords.get(username).contains(password))
  }

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    for {
      usernameOpt <- findUsername(userId)
      passwordOk <- usernameOpt.map { username =>
        checkPassword(username, password)
      }.getOrElse(false.pure[DbReader])
    } yield passwordOk
  }


  val users = Map(
    1 -> "john",
    2 -> "david",
    3 -> "peter"
  )

  val passwords = Map(
    "john" -> "querty",
    "david" -> "pwd",
    "peter" -> "secure"
  )


  val db = Db(users, passwords)

  println(checkLogin(1, "querty").run(db))

  println(checkLogin(4, "random").run(db))
}
