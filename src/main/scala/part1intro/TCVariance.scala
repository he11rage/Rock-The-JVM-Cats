package part1intro

object TCVariance {

  import cats.Eq // импорт тайп класса
  import cats.instances.int._ // импорт экземпляра тайп класса Eq[Int]
  import cats.instances.option._ // импорт экземпляра тайп класса Eq[Option[Int]]
  import cats.syntax.eq._ // импорт методов расширения

  val aComparison = Option(2) === Option(3)
  // val anInvalidComparison = Some(2) === None // Eq[Some[Int]] не найден, не входит в Eq[Option[Int]]

  // вариантность
  class Animal
  class Cat extends Animal

  // ковариантный тип: подтип распространяется на общий тип
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // контравариантный тип: подтип распространяется ОБРАТНО к универсальному типу
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  // rule of thumb: "HAS a T" = covariant, "ACTS on T" = contravariant
  // вариантность влияет на то, как извлекаются экземпляры тайп классов

  // контравариантный тайп класс
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow") // implementation not important
  makeSound[Animal] // ok - экземпляр тайп класса, определенного выше
  makeSound[Cat] // ok - Экземпляр тайп класса Animal также применим к Cat.

  // rule 1: контравариантные тайп классы могут использовать экземпляры суперкласса, если ничего не доступно строго для этого типа

  // has implications for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]

  // ковариантный тайп класс
  trait AnimalShow[+T] {
    def show: String
  }
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  // rule 2: ковариантные тайп классы всегда будут использовать более конкретный экземпляр TC для этого типа.
  // но может запутать компилятор, если присутствует общий TC

  // rule 3: вы не можете иметь оба преимущества
  // Cats используют инвариантные тайп классы

  Option(2) === Option.empty[Int]


  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat]) // ok - компилятор внедрит CatsShow как неявный
    // println(organizeShow[Animal]) // не компилируется - неоднозначные значения

  }

}
