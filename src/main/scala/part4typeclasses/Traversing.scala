package part4typeclasses

import cats.{Applicative, Foldable, Functor, Monad, Traverse}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val servers: List[String] = List("server-ci.rockthejvm.com", "server=staging.rockthejvm.com", "prod.rockthejvm.com")

  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  // another implement

  /*
    we have a List[String]
    String => Future[Int]
    we want a Future[List[Int]]
  * */

  val allBandwidthsSecond: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accumulator, hostname) =>
    val bandFuture: Future[Int] = getBandwidth(hostname)
    for {
      accBandwidths <- accumulator
      band <- bandFuture
    } yield accBandwidths :+ band
  }

  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth) // данный traverse находится в стандартной библиотеке scala
  // и работает следующим образом, мы передаем ему список типа List[String], далее передаем функцию, которую он применяет к этому списку, в данном случае функция возвращает Future[Int]
  // также traverse в результаты мы получаем Future[List[Int]], то есть выполняет операцию "перевертывания" контейнера. То есть, если у нас есть коллекция List[F[A]], то traverse возвращает F[List[A]].

  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(x => getBandwidth(x)))

  // TODO 1

  import cats.syntax.applicative._ // pure
  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // map

  // Объявляем метод с типажем Monad, принимающий список list и функцию func
  def listTraverseWithMonad[F[_] : Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
  // Начинаем свертку с пустого списка, обернутого в монаду F
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) =>
      // Получаем элемент списка в виде монады F
      val wElement: F[B] = func(element)
      // С помощью for-comprehension объединяем монады F списка и элемента
      for {
        acc <- wAccumulator // Получаем текущий аккумулятор
        element <- wElement // Получаем текущий элемент
      } yield acc :+ element // Объединяем текущий аккумулятор и элемент в список
    }

  import cats.syntax.apply._ // mapN

  def listTraverseWithApplicative[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
  // Используем метод `foldLeft` для обхода списка
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) =>
      // Применяем функцию `func` к текущему элементу списка
      val wElement: F[B] = func(element)
      // Используем метод `mapN` для объединения двух `F`-вычислений в кортеже
      // и применения операции конкатенации `:+` к элементам кортежа
      (wAccumulator, wElement).mapN(_ :+ _)
    }

  // TODO 2

  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverseWithApplicative(list)(identity)

  def listSequenceSecond[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] =
    list.foldLeft(List.empty[A].pure[F]) {(wAcc, el) =>
      val wEl: F[A] = list.head
      (wAcc, wEl).mapN(_ :+ _)
    }

  // TODO 3 - whats the result of
  import cats.instances.vector._

  listSequence(List(Vector(1, 2), Vector(3, 4))) // Vector[List[Int]] вернет нам Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4)) - all the possible 2-pairs
  listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) // Vector[List[Int]] - all the possible 3-pairs

  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverseWithApplicative[Option, Int, Int](list)(n => Some(n).filter(predicate))

  // TODO 4 - whats the result
  filterAsOption(List(2, 4, 6))(_ % 2 == 0) // Some(List(2, 4, 6))
  filterAsOption(List(1, 2, 3))(_ % 2 == 0) // None

  import cats.data.Validated
  import cats.instances.list._ // Semigroup[List] => Applicative[ErrorsOr]

  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverseWithApplicative[ErrorsOr, Int, Int](list){ n =>
      if(predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    }

  // TODO 5 - whats the result
  val allTrueValidated = filterAsValidated(List(2, 4, 6))(_ % 2 == 0) // ErrorsOr[List[Int]] Valid(List(2, 4, 6))
  val someFalseValidated = filterAsValidated(List(1, 2, 3))(_ % 2 == 0) // Invalid(List("predicate for 1 failed", "predicate for 3 failed"))

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_] : Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]

    def sequence[F[_] : Applicative, A](container: L[F[A]]): F[L[A]] = traverse(container)(identity)

    // TODO 6
    import cats.Id
    def map[A, B](wa: L[A])(f: A => B): L[B] = traverse[Id, A, B](wa)(f)


  }

  import cats.Traverse
  import cats.instances.future._ // Applicative[Future]

  val allBandwidthsCats = Traverse[List].traverse(servers)(getBandwidth)

  // extension methods
  import cats.syntax.traverse._ // sequence + traverse
  val allBandwidthsCats2 = servers.traverse(getBandwidth)


  def main(args: Array[String]): Unit = {
    println(allTrueValidated)
    println(someFalseValidated)
  }
}
