import cats.Eval

object Playground extends App {
  val meaningOfLife = Eval.later {
    println("Learning Cats: computing asdasdasd...")
  }

  println(meaningOfLife.value)
}
