package part4typeclasses

import cats.Eval
import cats.kernel.Monoid

object Folding {

  // TODO - implement all in terms of foldLeft
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldRight(List.empty[B])((a, currentList) => f(a) :: currentList)

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.foldLeft(List.empty[B])((currentList, a) => currentList.foldRight(f(a))(_ :: _))

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldRight(List.empty[A])((a, currentList) => if (predicate(a)) a :: currentList else currentList)

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)(monoid.combine)

  }

  import cats.Foldable
  import cats.instances.list._ // implicit Foldable[List]

  val sum = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) // 6

  import cats.instances.option._ // implicit Foldable[Option]

  val sumOption = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // 32

  // foldRight безопасен для стека независимо от вашего контейнера
  val sumRight = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num)
  }

  // convenience

  import cats.instances.int._ // Monoid[Int]
  import cats.instances.string._ // Monoid[String]

  val anotherSum = Foldable[List].combineAll(List(1, 2, 3)) // implicit Monoid[Int]
  val mappedConcat = Foldable[List].foldMap(List(1, 2, 3))(_.toString) // implicit Monoid[String] => "123"


  // nesting
  import cats.instances.vector._
  val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested) // List(Vector(1, 2, 3, 4, 5, 6))

  // extension methods
  import cats.syntax.foldable._
  val sum3 = List(1, 2, 3).combineAll // req Foldable[List], Monoid[Int]
  val nappedConcat2 = List(1, 2, 3).foldMap(_.toString)

  def main(args: Array[String]): Unit = {
    import ListExercises._
    val numbers = (1 to 10).toList
    println(map((1 to 10).toList)(_ + 1))

    println(combineAll(numbers))
  }
}
