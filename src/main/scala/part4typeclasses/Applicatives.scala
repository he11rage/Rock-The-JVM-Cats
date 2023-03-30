package part4typeclasses

object Applicatives {

  // Applicatives = Functors + the pure method from monads

  // Applicative wrap a value into a wrap type

  import cats.Applicative
  import cats.instances.list._

  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(2) // List(2)

  import cats.instances.option._ // implicit Applicative[Option]

  val optionApplicative = Applicative[Option]
  val aOption = optionApplicative.pure(2) // Some(2)

  // pure extension method

  import cats.syntax.applicative._

  val aSweetList = 2.pure[List] // List(2)
  val aSweetOption = 2.pure[Option] // Some(2)

  // Monads(pure and flatMap) extend Applicatives
  // Applicatives(pure) extend Functors (map)

  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(43) // "pure"
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1) // map

  val validatedApplicative = Applicative[ErrorsOr]

  // TODO: thought experiment

  // def ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T] = ??? // this already implemented

  def productWithApplicative[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
  }

  // Applicatives have this ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T]
  // Applicatives can implement product from Semigroupal
  // => Applicatives extend Semigroupal

  def main(args: Array[String]): Unit = {
    println(productWithApplicative(List(1, 2), List("a", "b")))
  }
}