package part1intro

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Essentials extends App{

  // values
  val aBoolean: Boolean = false

  // expressions are EVALUATED to a value
  val anIfExpression = if(2 > 3) "bigger" else "smaller"

  // instructions vs expression
  val theUnit = println("Hello, Scala") // doesn't return values = ()

  // OOP
  class Animal
  class Dog extends Animal
  class Cat extends Animal
  trait Carnivore {
    def eat(animal: Animal): Unit
  } // java interface which can contains an implemented fields

  // inheritance model: extend <= 1 class, but inherit from >= 0 traits
  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("crunch!")
  }

  // singleton
  object MySingleton // singleton pattern in one line

  // companions
  object Carnivore // companion object of the class Carnivore

  // generics
  class MyList[A] // A - means multiple types

  // method notation
  val three = 1 + 2 // simple expressions + синтаксический сахар
  val anotherThree = 1.+(2) // тоже самое что и выше, только не используя синтаксический сахар

  // functional programming - работа с функциями
  val incrementer: Int => Int = x => x + 1
  val incremented = incrementer(45) // 46

  // map, flatMap, filter - работа с коллекциями
  val processedList = List(1, 2, 3).map(incrementer) // List(2, 3, 4)
  val aLongerList = List(1, 2, 3).flatMap(x => List(x, x + 1)) // List(1, 2, 2, 3, 3, 4)

  // for-comprehenion
  val checkboard = List(1, 2, 3).flatMap(n => List('a', 'b', 'c').map(c => (n, c)))

  val anotherCheckerboard = for {
    n <- List(1, 2, 3)
    c <- List('a', 'b', 'c')
  } yield (n, c) // equivalent expression

  // options and try
  val anOption: Option[Int] = Option(/* something that might be null */ 3) // Some(3)
  val doubledOption: Option[Int] = anOption.map(_ * 2) // Some(6)

  val anAttempt = Try(/* something that might throw */ 42) // Success(42) or Failure
  val aModifiedAttempt: Try[Int] = anAttempt.map(_ + 10) // if Success = Success(52) or Failure

  // pattern matching
  val anUnknown: Any = 45
  val ordinal = anUnknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val optionDescription: String = anOption match {
    case Some(value) => s"the option is not empty: $value"
    case None => "the option is empty"
  }

  // Futures
  import scala.concurrent.ExecutionContext.Implicits.global

  val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aFuture: Future[Int] = Future {
    // a bit of code
    42
  }

  // wait for completion (async)
  aFuture.onComplete {
    case Success(value) => println(s"The async $value")
    case Failure(exception) => println(s"$exception")
  }

  // map a Future
  val anotherFuture = aFuture.map(_ + 1) // Future(43) when it completes

  // partial functions
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 43
    case 8 => 56
    case 100 => 999
  }

  // some more advanced stuff
  trait HigherKindedType[F[_]]
  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }
}
