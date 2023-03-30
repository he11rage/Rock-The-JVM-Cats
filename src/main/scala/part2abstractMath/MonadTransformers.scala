package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  // option transformer

  import cats.data.OptionT
  import cats.instances.list._ // fetch an implicit OptionT[List]
  import cats.instances.future._

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2))) // OptionT list of option int
  val listOfCharOption: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOption
    number <- listOfNumberOptions
  } yield (number, char)

  // either transformer

  import cats.data.EitherT

  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(43), Right(2)))
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45)) // wrap over Future(Right(45))

  /*
    TODO exercise
  * */

  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170,
  )
  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT(Future[Either[String, Int]](Left(s"Server $server unreachable")))
    case Some(b) => EitherT(Future[Either[String, Int]](Right(b)))
  }


  // TODO 1
  // call getBandwidth twice, and combine the results

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = {
    for {
      band1 <- getBandwidth(s1)
      band2 <- getBandwidth(s2)
    } yield band1 + band2 > 250
  }
  // return Future[Either[String, Boolean]]

  // TODO 2
  // call canWithStandSurge + transform
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = canWithstandSurge(s1, s2).transform {
    case Left(reason) => Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spoke: $reason")
    case Right(false) => Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spoke: not enough total bandwidth")
    case Right(true) => Right(s"Servers $s1 and $s2 can cope with tne incoming spilke NO PROBLEM!")
  }
  // ^^^^^^^^^^^^^^^^^^^^^                       ^^^^^^^^^^^^^^^^^^^^^^^^^^
  // return Future[Either[String, Boolean]] ---- Future[Either[String, String]]


  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)
    val resultFuture = generateTrafficSpikeReport("server1.rockthejvm.com", "server2.rockthejvm.com").value
    resultFuture.foreach(println)
  }
}
