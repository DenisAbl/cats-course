package part2abstractMath.mysolutions

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MyMonads {

  // lists
  val numbersList = List(1,2,3)
  val charsList = List('a', 'b', 'c')
  // TODO 1.1: how do you create all combinations of (number, char)?
  val combinationsList: List[(Int, Char)] = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val combinationsListFor = for {
    n <- numbersList
    c <- charsList
  } yield (n, c) // identical

  // options
  val numberOption = Option(2)
  val charOption = None
  // TODO 1.2: how do you create the combination of (number, char)?
  val combinationOption = numberOption.flatMap(n => charOption.map(c => (n, c)))
  val combinationOptionFor = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture = Future(42)
  val charFuture = Future('Z')
  // TODO 1.3: how do you create the combination of (number, char)?
  val combinationFuture = numberFuture.flatMap(n => charFuture.map(c => (n, c)))
  val combinationFutureFor = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  /*
     Pattern
     - wrapping a value into a monadic value
     - the flatMap mechanism

     MONADS
   */
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  // Cats Monad
  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]
  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None) // None

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(4, 5)

  // TODO 2: use a Monad[Future]
  import cats.instances.future._
  val futureMonad = Monad[Future]
  val monFut = futureMonad.pure(224)
  val transMonFut = futureMonad.flatMap(monFut)(value => Future(value / 4))

  import cats.syntax.flatMap._
//  import cats.syntax.applicative._
  import cats.syntax.functor._
def getPairs[M[_],A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] = {

  ma.flatMap(a => mb.map(b => (a, b)))
}


  def main(args: Array[String]): Unit = {
    println(combinationOptionFor)
    println(aTransformedList)
    print(transMonFut)
//    println(getPairsFor(numbersList, charsList))
//    println(getPairsFor(numberOption, charOption))
//    getPairsFor(numberFuture, charFuture).foreach(println)
  }
}
