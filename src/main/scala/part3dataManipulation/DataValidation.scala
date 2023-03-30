package part3dataManipulation

import cats.kernel.Semigroup

import scala.annotation.tailrec
import scala.util.Try


object DataValidation {

  import cats.data.Validated // like either

  val aValidValue: Validated[String, Int] = Validated.valid(42) // "right" value like either
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "left" value
  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")

  // TODO: use Either
  /*
    - n must be a prime
    - n must be non-negative
    - n <= 100
    - n must be even
  * */

  def testPrime(n: Int): Boolean = {
    @tailrec
    def tailrecPrime(d: Int): Boolean =
      if (d <= 1) true
      else n % d != 0 && tailrecPrime(d - 1)

    if (n == 0 || n == 1 || n == -1) false
    else tailrecPrime(Math.abs(n / 2))
  }

  def testNumber(n: Int): Either[List[String], Int] = {
    val nonPrime = if (!testPrime(n)) List("Non prime") else List()
    val isNegative = if (n < 0) List("Negative") else List()
    val isNmensheSotki = if (n >= 100) List("n >= 100") else List()
    val isNotEven = if (n % 2 != 0) List("Non even") else List()

    if (n % 2 == 0 && n >= 0 && n <= 100 && testPrime(n)) Right(n)
    else Left(nonPrime ++ isNegative ++ isNmensheSotki ++ isNotEven)
  }

  import cats.instances.list._

  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be <= 100")))
      .combine(Validated.cond(testPrime(n), n, List("Number must be prime")))


  // chain
  aValidValue.andThen(v => anInvalidValue)

  // test a valid value
  aValidValue.ensure(List("something went wrong"))(_ % 2 == 0)

  // transform valid value
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)

  // interoperate with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present here"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))

  // backwards
  aValidValue.toOption
  aValidValue.toEither

  // TODO 2 - form validation
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    /*
      fields are
      - name
      - email
      - password

      rules are
      - name, email and password MUST be specified
      - name must not be blank
      - email must have "@"
      - password must have >= 10 characters
    * */



    def validateForm(form: Map[String, String]): FormValidation[String] = {
      val success = "Success"
      Validated.cond(form.contains("email"), success, List("Email must be specified"))
        .combine(Validated.cond(form.contains("name"), success, List("Name must be specified")))
        .combine(Validated.cond(form.contains("password"), success, List("Password must be specified")))
        .combine(Validated.cond(form("name") != "", success, List("Name must not be blank")))
        .combine(Validated.cond(form("email").contains("@"), success, List("Email must have @")))
        .combine(Validated.cond(form("password").length >= 10, success, List("password must have >= 10 characters")))
        .map(_ => "User registration complete")
    }
  }

  def main(args: Array[String]): Unit = {
    val form: Map[String, String] = Map(
      "email" -> "dotcom@gmail.com",
      "name" -> "asda",
      "password" -> "12345asdqwe"
    )

    import cats.syntax.validated._
    val aValidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
    val anError: Validated[String, Int] = "Something went wrong".invalid[Int]

    println(FormValidation.validateForm(form))
    println(form.get("email"))
  }
}
