package part4typeclasses.mysolutions

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MyTraversing {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers: List[String] = List("server.rockthejvm", "server-staging.rockthejvm", "prod.rockthejvm")
  def getBandwidth(server: String): Future[Int] = Future(server.length * 50)

   val bandwidths: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
   val bandwidths2: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))
  val allBandwidthsManual: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accumulator, hostname) =>
    val bandFuture: Future[Int] = getBandwidth(hostname)
    for {
      accBandwidths <- accumulator
      band <- bandFuture
    } yield accBandwidths :+ band
  }

  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import cats.syntax.functor._ //for map
  def listTraverse[A, B, F[_]: Monad](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldRight(List.empty[B].pure[F]){ (elem, acc) =>
      val interResult: F[B] = func(elem)
      for {
        totalRes <- acc
        singleRes <- interResult
      } yield singleRes :: totalRes
    }
  }


import cats.syntax.apply._
  def listTraverseApp[A, B, F[_] : Applicative](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldRight(List.empty[B].pure[F]) { (elem, acc) =>
      val interResult: F[B] = func(elem)
      (acc, interResult).mapN((x, y) => y :: x )

    }

  }

  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = {
    listTraverseApp(list)(x => x)
  }

  val manualTraversed: Option[List[Int]] = listTraverse(servers)(name => Option(name.length * 50))
  val manualTraversedApplicative: Option[List[Int]] = listTraverseApp(servers)(name => Option(name.length * 50))

  // TODO 3 - what's the result of

  import cats.instances.vector._

  val allPairs = listSequence(List(Vector(1, 2), Vector(3, 4))) // Vector[List[Int]] - all the possible 2-pairs
  val allTriples = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) // Vector[List[Int]] - all the possible 3-pairs

  import cats.instances.option._
  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverseApp[Int, Int, Option](list)(n => Some(n).filter(predicate))

  // TODO 4 - what's the result of
  val allTrue = filterAsOption(List(2, 4, 6))(_ % 2 == 0) // Some(List(2,4,6))
  val someFalse = filterAsOption(List(1, 2, 3))(_ % 2 == 0) // None

  




  def main(args: Array[String]): Unit = {
    bandwidths.foreach(list => println(list.mkString(",")))
    bandwidths2.foreach(list => println(list.mkString(",")))
    allBandwidthsManual.foreach(list => println(list.mkString(",")))
    manualTraversed.foreach(list => println(list.mkString(",")))
    manualTraversedApplicative.foreach(list => println(list.mkString(",")))
  }
}
