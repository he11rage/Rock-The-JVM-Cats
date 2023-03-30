package part2abstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // import the |+| extension method

  val numbers = (1 to 1000).toList
  // |+| is always associative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
  /* def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
    list.foldLeft(/* WHAT?! */)(_ |+| _) */

  // MONOIDS
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999) // 1022
  val zero = intMonoid.empty // 0

  import cats.instances.string._ // bring the implicit Monoid[String] in Scope
  val emptyString = Monoid[String].empty // "" - will return empty string
  val combineString = Monoid[String].combine("I understand ", "monoids")

  import cats.instances.option._ // construct an implicit Monoid[Option[Int]]
  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)

  // extension method for Monoids - |+|
  // import cats.syntax.monoid._ // either this one or cats.syntax.semigroup._
  val combinedOptionFancy = Option(3) |+| Option(7)

  // TODO 1: implement a combineFold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T = list.foldLeft(monoid.empty)(_ |+| _)

  // TODO 2: combine a list of phonebooks as Maps[String, Int]
  // hint: dont construct a monoid - use an import
  import cats.instances.map._
  val phonebooks = List(
    Map(
     "Alice" -> 235,
     "Bob" -> 647,
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    ),
    Map(
      "Tine" -> 123
    )
  )

  // TODO 3: shopping cart and online stores with Monoids
  // hint: define your monoid - Monoid.instance
  // hint 2: use combineByFold

  implicit val shoppingMonoid: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart] (
    ShoppingCart(List(), 0.0),
    (sa, sb) => ShoppingCart(sa.items ++ sb.items, sa.total + sb.total)
  )

  case class ShoppingCart(items: List[String], total: Double)
  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(combineFold(numbers))
    println(combineFold(List("I ", "like ", "monoids")))
    println(combineFold(phonebooks))
    println(checkout(List(
     ShoppingCart(List("iphone", "shoes"), 799), ShoppingCart(List("Tv", "saw"), 200)
    )))
  }
}
