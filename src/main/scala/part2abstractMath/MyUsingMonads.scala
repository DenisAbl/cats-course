package part2abstractMath

import part2abstractMath.UsingMonads.{AggressiveHttpService, errorOrResponse}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object MyUsingMonads {

  import cats.Monad
  import cats.instances.list._
  import cats.instances.option._
  val monadList = Monad[List] // fetch the implicit Monad[List]
  val aSimpleList = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))
  // applicable to Option, Try, Future

  // either is also a monad
  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]
  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n+1) else Left("Loading some."))

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId, "Ready to ship"))
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))
  // use extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(orderStatus => trackLocation(orderStatus))
  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO: the service layer API of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response
  // DO NOT CHANGE THE CODE

  /*
    Requirements:
    - if the host and port are found in the configuration map, then we'll return a M containing a connection with those values
      otherwise the method will fail, according to the logic of the type M
      (for Try it will return a Failure, for Option it will return None, for Future it will be a failed Future, for Either it will return a Left)
    - the issueRequest method returns a M containing the string: "request (payload) has been accepted", if the payload is less than 20 characters
      otherwise the method will fail, according to the logic of the type M

    TODO: provide a real implementation of HttpService using Try, Option, Future, Either
   */

  object FutureHttpService extends HttpService[Future] {
    override def getConnection(cfg: Map[String, String]): Future[Connection] = (for {
      host <- cfg.get("host")
      port <- cfg.get("port")
    } yield Connection(host, port)) match {
      case Some(value) => Future.successful(value)
      case None => Future.failed(new Exception("Log in failed"))
    }

    override def issueRequest(connection: Connection, payload: String): Future[String] =
      if (payload.size < 20) Future.successful(s"request ($payload) has been accepted") else Future.failed(new Exception("payload malformed"))
  }


  object TryHttpService extends HttpService[Try] {
    override def getConnection(cfg: Map[String, String]): Try[Connection] = (for {
      host <- cfg.get("host")
      port <- cfg.get("port")
    } yield Connection(host, port)) match {
      case Some(value) => Success(value)
      case None => Failure(new Exception("Log in failed"))
    }

    override def issueRequest(connection: Connection, payload: String): Try[String] =
      if (payload.size < 20) Success(s"request ($payload) has been accepted") else Failure(new Exception("payload malformed"))
  }

  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = for {
      host <- cfg.get("host")
      port <- cfg.get("port")
    } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.size < 20) Option(s"request ($payload) has been accepted") else None
  }

  object AggressiveHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] = {
      val errorMonad = Monad[ErrorOr]
      (for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)) match {
        case Some(value) => Right(value)
        case None => Left(new RuntimeException("Connection failed"))
      }
    }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] = {
      if (payload.size < 20) Right(s"Payload $payload has been accepted") else Left(new RuntimeException("payload is corrupted"))
    }
  }

  def main(args: Array[String]): Unit = {
    val testEither = Left("Here we have a string")
//    loadingMonad.flatMap(testEither)(x =>  if (x % 2 == 0) Right(x+1) else Left("Loading some.")) //will not compile


//    println(getResponse(OptionHttpService, "Hello Option"))
//    println(getResponse(AggressiveHttpService, "Hello, ErrorOr"))

    val responseOption  = OptionHttpService.getConnection(config).flatMap(conn => OptionHttpService.issueRequest(conn, "Yohoho"))
    println(responseOption)

    val responseErrorOr = AggressiveHttpService.getConnection(config).flatMap(conn => AggressiveHttpService.issueRequest(conn, "Agressive pay"))
    println(responseErrorOr)

    println(getResponse(AggressiveHttpService, "200 OK"))
    println(getResponse(TryHttpService, "200 OK"))
  }
}
