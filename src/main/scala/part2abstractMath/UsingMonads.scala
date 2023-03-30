package part2abstractMath

object UsingMonads {

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
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if(n % 2 == 0) Right(n + 1) else Left("Loading meaning of life..."))

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if(orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
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

  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] = {
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response
  }

  // DO NOT CHANGE THE CODE

  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = for {
      host <- cfg.get("host")
      port <- cfg.get("port")
    } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] = {
      if (payload.length >= 20) None
      else Some(s"request ($payload) has been accepted")
    }
  }

  val responseOption = OptionHttpService.getConnection(config).flatMap(conn => OptionHttpService.issueRequest(conn, "Hello HTTP Service"))
  val responseOptionFor = for {
    connection <- OptionHttpService.getConnection(config)
    issue <- OptionHttpService.issueRequest(connection, "Hello Http Service")
  } yield issue

  // TODO implement another HttpService with LoadingOr or ErrorOr

  object AggressiveHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] = {
      if (!cfg.contains("host") || !cfg.contains("port")) Left(new RuntimeException("Connection colud not be establisged"))
      else {
        Right(Connection(cfg("host"), cfg("port")))
      }
    }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] = {
      if(payload.length >= 20) Left(new RuntimeException("Nothing"))
      else Right(s"request ($payload) has been accepted")
    }
  }

  val errorOrResponse = for {
    conn <- AggressiveHttpService.getConnection(config)
    response <- AggressiveHttpService.issueRequest(conn, "Hello ErrorOr")
  } yield response

  def main(args: Array[String]): Unit = {
    println(responseOption)
    println(responseOptionFor)
    println(errorOrResponse)
    println(getResponse(OptionHttpService, "HelloOPtion"))
    println(getResponse(AggressiveHttpService, "HelloOption"))
  }

}
