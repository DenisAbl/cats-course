package part4typeclasses

import cats.data.Validated
import cats.{Applicative, ApplicativeError, Monad, MonadError}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object MyHandlingErrors {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  trait MyMonadErr[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String]

  val success = monadErrorEither.pure(32) //Right(32)
  val failure = monadErrorEither.raiseError[Int]("Error") // Left(Error) if no type specified it will fails with compilation of other methods like handle error

  val recover: ErrorOr[Int] = monadErrorEither.handleError(failure)(x => if (x == "Error") 13 else 789) // Right(13)
  val recoverWith: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure)(x => if (x == "Error") monadErrorEither.pure(1984) else failure) // Right(1984)
  val recoverWith2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure)(x => if (x == "Error2") monadErrorEither.pure(1984) else failure) // Left(Error)
  val filterSuccess: ErrorOr[Int] = monadErrorEither.ensure(success)("Check success, but it's invalid")(_ > 100)

  implicit val ex: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  type ErrorsOr[T] = Validated[List[String], T]
  import cats.instances.list._
  val applErrorVal = ApplicativeError[ErrorsOr, List[String]]

  //extension methods
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // raiseError, handle

  val extendedSuccess: ErrorsOr[Int] = 42.pure[ErrorsOr] //requires implicit ApplicativeError[ErrorsOr, List[String]]
  val extendedError: ErrorsOr[Int] =  List("Errors").raiseError[ErrorsOr, Int] //requires implicit ApplicativeError[ErrorsOr, List[String]]
  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case head :: tail if tail.nonEmpty =>  1
    case Nil => 23
  }

  import cats.syntax.monadError._ //ensure
  val testedSuccess = success.ensure("Bad condition")(_ > 300)


  def main(args: Array[String]): Unit = {
    println(success)
    println(failure)
    println(recover)
    println(recoverWith)
    println(recoverWith2)
  }
}
