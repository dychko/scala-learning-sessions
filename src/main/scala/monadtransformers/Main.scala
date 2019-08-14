package monadtransformers

import java.util.UUID

import cats.data.{Kleisli, OptionT}
import cats.implicits._
import cats.{FlatMap, Functor}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.language.higherKinds

object Main extends App {

  case class User(id: UUID, name: String)

  case class Address(id: UUID, street: String)

  def findUserById(id: UUID): OptionT[Task, User] = OptionT(Task.now(Option(User(UUID.randomUUID(), "John"))))

  def findAddressByUser(user: User): OptionT[Task, Address] = OptionT(Task.now(Option(Address(UUID.randomUUID(), "strasse"))))


  //  def findAddressByUserId(userId: UUID): Task[Option[Address]] = {
  //    for {
  //      user <- userOption <- findUserById(userId)
  //      address <- addressOption <- findAddressByUser(user)
  //    } yield address
  //  }
  //


  //  def findAddressByUserId(userId: UUID): Task[Option[Address]] = {
  //    findUserById(userId).flatMap {
  //      case Some(user) => findAddressByUser(user)
  //      case None => Task.now(None)
  //    }
  //  }


  // Functor: map
  // A[X] -> map -> A[Y]
  // Functor[A]
  // B[X] -> map -> B[Y]
  // A[B[X]] -> *map* -> A[B[Y]]


  val taskOptionF = Functor[Task].compose(Functor[Option])

  val data: Task[Option[Int]] = Task.now(Some(1))

  val result = taskOptionF.map(data)(_ * 100)

  import scala.concurrent.duration._

  println(result.runSyncUnsafe(1.second))


  // Monad: flatMap
  // A[X] -> flatMap(f: X => A[Y]) -> A[Y]
  // A[B[X]] -> flatMap -> XXX

  // Monads don't compose


  case class TaskOpt[A](value: Task[Option[A]]) {

    def map[B](f: A => B): TaskOpt[B] = {
      TaskOpt(value.map(opt => opt.map(f)))
    }

    def flatMap[B](f: A => TaskOpt[B]): TaskOpt[B] = {
      TaskOpt(value.flatMap {
        case Some(v) => f(v).value
        case None => Task.now(None)
      })
    }
  }


//  def findAddressByUserId(userId: UUID): Task[Option[Address]] = {
//    (for {
//      user <- TaskOpt(findUserById(userId))
//      address <- TaskOpt(findAddressByUser(user))
//    } yield address).value
//  }

  case class ListOpt[A](value: List[Option[A]]) {

    def map[B](f: A => B): ListOpt[B] = {
      ListOpt(value.map(_.map(f)))
    }

    def flatMap[B](f: A => ListOpt[B]): ListOpt[B] = {
      ListOpt(value.flatMap {
        case Some(v) => f(v).value
        case None => List(None)
      })
    }
  }

  type TaskOptCats[A] = OptionT[Task, A] // Task[Option[A]]


  def findAddressByUserId(userId: UUID): OptionT[Task, Address] = {
    for {
      user <- findUserById(userId)
      address <- findAddressByUser(user)
    } yield address
  }

  /////////////////////////////////////////////

  // A => B, B => C ==> A => C

  // A => F[B], B => F[C] ==> A => F[C]

  type TaskOption[A] = OptionT[Task, A]

  val userById: Kleisli[TaskOption, UUID, User] = Kleisli(userId => findUserById(userId))

  val addressByUser: Kleisli[TaskOption, User, Address] = Kleisli(user => findAddressByUser(user))

  val addressByUserId: Kleisli[TaskOption, UUID, Address] = addressByUser.compose(userById)

  println(addressByUserId(UUID.randomUUID()).value.runSyncUnsafe(1.second))

  println(findAddressByUserId(UUID.randomUUID()).value.runSyncUnsafe(1.second))

  final case class KleisliV[F[_], A, B](run: A => F[B]) {

    def compose[Z](k: KleisliV[F, Z, A])(implicit f: FlatMap[F]): KleisliV[F, Z, B] = {
      // k.run(z): Z => F[A] => F[B]
      KleisliV(z => k.run(z).flatMap(run))
    }
  }
}
