package part2abstractMath

import scala.util.Try

object Functors {

  val aModifiedList = List(1, 2, 3).map(_ + 1) // List(2, 3, 4)
  val aModifiedOption = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry = Try(42).map(_ + 1) // Success(4)


  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  // Cats Functor
  import cats.Functor
  import cats.instances.list._ // includes Functor[List]
  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1, 2, 3))(_ + 1) // List(2, 3, 4)

  import cats.instances.option._ // includes Functor[Option]
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(3))(_ + 1) // Some(4)

  import cats.instances.try_._ // includes Functor[Try]
  val incrementedTry = Functor[Try].map(Try(5))(_ + 1) // Success(6)

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  // generalize
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  // TODO 1: define your own functor for a binary tree
  // hint: define an object which extends Functor[Tree]
  // define your own map function

  implicit object treeFunctor extends Functor[Tree]{
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Leaf(v) => Leaf(f(v))
        case Branch(v, left, right) => Branch(f(v), map(left)(f), map(right)(f))
      }
  }

  trait Tree[+T]
  // "smart" constructors
  object Tree {
    def leaf[T](value: T) = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  // extension method - map
  import cats.syntax.functor._
  val tree: Tree[Int] =
    Tree.branch(40, Tree.branch(5, Tree.leaf(19), Tree.leaf(40)), Tree.leaf(20))
  val incrementedTree = tree.map(_ + 1)

  // TODO 2: write a shorted do10x method with extension methods
  def do10xShorter[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  def main(args: Array[String]): Unit = {
    println(do10x(Option(10)))
    println(do10x(List(1, 2, 3)))
    println(do10x(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))
    println(do10xShorter(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))

  }

}
