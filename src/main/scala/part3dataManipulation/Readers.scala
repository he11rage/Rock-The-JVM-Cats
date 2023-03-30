package part3dataManipulation

object Readers {

  /*
    - configuration file => initial data structure
    - a DB layer
    - an HTTP layer
    - a business logic layer
  */

  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched" // select * from the db table an return the status of the orderID

    def getLastOrderId(username: String): Long = 54263 // select max(orderId) from table where username = username
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started") // this would start the actual server

  }

  // bootstrap
  val config = Configuration("daniel", "rocjthejvm1!", "localhost", 1234, 8, "daniel@rockthejvm.com")

  // cats Reader

  import cats.data.Reader

  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConn = dbReader.run(config)

  // Reader[I, O]
  val danielsOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbcon => dbcon.getOrderStatus(55))
  val danielsOrderStatus: String = danielsOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderIdReader: Reader[Configuration, String] = dbReader.map(_.getLastOrderId(username)).flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    val usersOrderFor: Reader[Configuration, String] = for{
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    usersOrderFor.run(config)
  }

  /*
    Паттерн
    1. Вы создаете внутреннюю структуру данных
    2. Вы создаете reader, который как та структура данных будет указана позже
    3. Затем вы можете использовать map & flatMap в reader для получения производной информации
    4. Когда вам нужна окончательная часть информации, вы вызываете run reader с исходной структурой данных
  */


  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"From $emailReplyTo: to $address: $contents"
  }

  // TODO
  def emailUser(username: String, userEmail: String) = {
    // получить статус их последнего заказа
    // напишите им с помощью службы электронной почты: "Ваш заказ имеет статус: (статус)"
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))

    val emailReader: Reader[Configuration, String] = ???

    val usersOrderFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(userEmail, s"Your last order status: $orderStatus")
  }


  def main(args: Array[String]): Unit = {
  println(getLastOrderStatus("daniel"))
  }
}
