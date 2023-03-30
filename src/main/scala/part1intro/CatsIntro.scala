package part1intro

object CatsIntro {

  // Eq
  val aComparison = 2 == "a string" // данное сравнение вызовет предупреждение компилятора, всегда будет возвращать false

  // part 1 - импорт тайп классов

  import cats.Eq
  // part 2 - импорт тайп классов для нужных типов

  import cats.instances.int._

  // part 3 - использование API TC

  val intEquality = Eq[Int] // альтернатива equals
  val aTypeSafeComparison = intEquality.eqv(2, 3) // eqv - альтернатива сравнения через ==, более предпочтительная так как при помощи == мы можем сравнивать
  // два совершенно несвязанных между собой типа без получения ошибки.

  // val anUnsafeComparison = intEquality.eqv(2, "string") -- не скомпилируется

  // part 4 - использование методов расширения (если возможно)

  import cats.syntax.eq._
  val anotherTypeSafeComp = 2 === 3 // безопасное сравнение аналог eqv
  val neqComparison = 2 =!= 3 // true
  // val invalidComparison = 2 === "a string" // не скомпилируется, ждёт необходимый тип
  // расширенные методы видны только при наличи правильного экземпляра тайп класса

  // part 5 - наследование операций тайп классов на составные типы, например списки

  import cats.instances.list._ // мы вызываем Eq[List[Int]] в области видимости
  val aListComparison = List(2) === List(3)  // возвращает false

  // part 6 - создание тайп класса для собственного типа

  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price == car2.price
  }

  val compareTwoToyCars = ToyCar("Ferrari", 29.99) === ToyCar("Lamborghini", 29.99) // valid expression, return true
}


