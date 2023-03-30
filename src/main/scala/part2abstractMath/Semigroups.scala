package part2abstractMath

import cats._
import cats.implicits._

object Semigroups {

  // Semigroups COMBINE elements of the same type

  import cats.Semigroup
  import cats.instances.int._
  val naturalIntSemigroup = Semigroup[Int]
  val intCombintation = naturalIntSemigroup.combine(2, 46) // сложение
  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("I love ", "cats") // concatenation

  // specific API
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine) // сложение всех элементов списка
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine) // объединение всех элементов string в единую строку

  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine) // универсальный метод для сложения


  // TODO 1: support a new type
  // hint: use the same pattern we used with Eq
  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] {(e1, e2) =>
    Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount)
  }

  // extension methods from Semigroup - |+|
  import cats.syntax.semigroup._
  val anIntSum = 2 |+| 3 // requires the presence of an implicit Semigroup[Int]
  val anStringConcat = "String " |+| "Stringer"
  val aCombinedExpense = Expense(4, 80) |+| Expense(56, 46)

  // TODO 2: implement reduceThings2 with the |+|
  def reduceThings2[T : Semigroup](list: List[T]): T = list.reduce(_ |+| _)


  def main(args: Array[String]): Unit = {
    println(intCombintation)
    println(stringCombination)

    //specific API
    val numbers = (1 to 10).toList
    println(reduceInts(numbers))
    val strings = List("Im ", "starting ", "to ", "like ")
    println(reduceStrings(strings))

    // general API
    println(reduceThings(numbers), reduceThings(strings)) // compiler injects the implicit needed Semigroup[type]
    import cats.instances.option._
    // compiler will produce an implicit Semigroup[Option[Int]] - combine will produce another option with the summed elements
    // compiler will produce an implicit Semigroup[Option[String]] - combine will produce another option with concatenated elements
    // same for any type with an implicit Semigroup

    val numberOptions = numbers.map(n => Option(n))
    println(reduceThings(numberOptions)) // an Option[Int] containing the sum of all the numbers
    val stringOptions: List[Option[String]] = strings.map(s => Option(s))
    println(reduceThings(stringOptions))


    // test ex 1
    val expenses = List(Expense(1, 23), Expense(2, 43), Expense(43, 12))
    println(reduceThings(expenses))

    println(anIntSum)
    println(anStringConcat)

    // text ex 2
    println(reduceThings2(numbers))
    println(reduceThings2(strings))
    println(reduceThings2(numberOptions))
  }
}
