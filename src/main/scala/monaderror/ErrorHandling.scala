package monaderror

import cats.MonadError
import cats.implicits._
import io.circe.{Json, ParsingFailure}

import scala.concurrent.{Await, Future}
import scala.util.{Success, Try}

sealed trait Error
case class LoginError(user: String) extends Error
case object PlainError extends Error

case class User(name: String, age: Int)

// https://www.codacy.com/blog/monad-error-for-the-rest-of-us/

object ErrorHandling extends App {
  type Result = Either[Error, User]

  def handleError(error: Error) = error match {
    case PlainError    => println("Unknown")
    case LoginError(u) => println(s"Error for $u")
  }

  val e1 = User("cats", 42).asRight[Error]
  val e2 = LoginError("cats").asLeft[User]

  e1.fold(handleError, println)
  e2.fold(handleError, println)

  import scala.concurrent.ExecutionContext.Implicits.global

  def readFile(file: String): Try[String] = Success(file)
  def readFileFuture(file: String): Future[String] = Future.successful(file)
  def readJson(string: String): Either[ParsingFailure, Json] =
    io.circe.parser.parse(string)

  val t = for {
    content <- readFileFuture("""{"a: 2}""")
    //    content <- readFile("""{"a": 2}""")
    //    json <- readJson(content)
    json <- abstactParser[Future](content)
  } yield json

  def abstactParser[F[_]](string: String)(
      implicit monad: MonadError[F, Throwable]): F[Json] =
    io.circe.parser
      .parse(string)
      .fold(l => monad.raiseError(l), r => monad.point(r))

  import scala.concurrent.duration._

  println(Await.result(t, 5.seconds))
}
