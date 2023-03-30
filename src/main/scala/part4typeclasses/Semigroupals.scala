package part4typeclasses

import cats.Monad

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}


object Semigroupals {

  // Semigroup => combining
  // Semigroupal => tupling

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]

  val optionSemigroupal = Semigroupal[Option]
  val aTupleOption = optionSemigroupal.product(Some(123), Some("a String")) // Some((123, "a string"))
  val aNoneTupled = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._ // implicit Semigroupal[Future]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
 // val aTupledFuture = Semigroupal[Future].product(Future("the meaning of life"), Future(42)) // Future("the meaning of life", 42)

  import cats.instances.list._ // Monad[List]
  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b")) // zip lists (1, a), (1, b) ...

  // TODO: implement
  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // MONADS EXTEND SEMIGROUPALS

  trait MyMonad[M[_]] extends MySemigroupal[M] {

    def pure[A](value: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(x => pure(f(x)))

    def product[A, B](fa: M[A], fb: M[B]): M[(A, B)] =
      flatMap(fa)(a => map(fb)(b => (a, b)))

  }

  // example: Validated
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires the implicit Semigroup[List[_]]

  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "something else wrong")),
    Validated.invalid(List("This cant't be right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]

  val eitherCombination = eitherSemigroupal.product( // in terms of map/flatMap
    Left(List("Something wrong", "something else wrong")),
    Left(List("This cant't be right"))
  )

  // Associativity: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

  // TODO 2: define a Semigroupal[List] which does a zip

  // my version (не верно потому что получается tuple zip List((1, a), (1, b)) а необходим просто zip List((1, a)(2, b)) )
  def semigroupalZip[A, B](list: List[A], secondList: List[B]): List[(A, B)] = Semigroupal[List].product(list, secondList)

  // right version
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](listA: List[A], listB: List[B]): List[(A, B)] = listA.zip(listB)
  }

  def main(args: Array[String]): Unit = {
    println(aTupledList)
    println(invalidsCombination)
    println(eitherCombination)
    println(semigroupalZip[Int, String](List(1, 2, 3), List("a", "b", "c")))
    println(zipListSemigroupal.product(List(1, 2), List("a", "b")))
  }
}
