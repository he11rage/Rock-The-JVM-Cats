package part3dataManipulation

object Evaluation {

  /*
  Cats makes the distinction between
  - evaluating an expression eagerly
  - evaluating lazily and every time you request it
  - evaluating lazily and keeping the value(memoizing)
  Cats делают различие между
     - жадно оценивать выражение
     - оценка лениво и каждый раз, когда вы ее запрашиваете
     - ленивое вычисление и сохранение значения (запоминание)
  */

  import cats.Eval

  val instantEval: Eval[Int] = Eval.now {
    //println("Computing now!") // срабатывает при первом использовании
    64345
  }

  val redoEval = Eval.always {
    println("Computing again!") // срабатывает постоянно
    4234
  }

  val delayedEval = Eval.later {
    println("Computing later!") // срабатывает при первом использовании
    53278
  }

  val composedEvaluation = instantEval.flatMap(value1 => delayedEval.map(value2 => value1 + value2))
  val anotherComposedEvaluation = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2 // identical

  // TODO 1:
  val evalEx1 = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d
  // now, later. again, again, sum, again, again, sum

  // запоминает вычисленное значение
  val dontRecompute = redoEval.memoize // redo сработает только один раз принтлн

  val tutorial = Eval
    .always { println("Step 1..."); "put the guitar on your lap" }
    .map { step1 => println("Step 2..."); s"$step1 then put your left gand on the neck" }
    .memoize // remember the value up to this point
    .map {steps12 => println("Step3, more complicated"); s"$steps12 then with the right hand strike the" +
      s"strings"}

  // TODO 2: implement defer such that(Eval.now) does NOT run the side effects
  def defer[T](eval: => Eval[T]): Eval[T] = {
    Eval.later(()).flatMap(_ => eval)
  }

  // TODO 3: rewrite the method with Evals
  def reverseList[T](list: List[T]): List[T] =
    if(list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] = {
    if (list.isEmpty) Eval.now(list)
    else defer(reverseEval(list.tail).map(_ :+ list.head))
  }

  def main(args: Array[String]): Unit = {
    println(defer(Eval.now{
      println("Now!")
      42
    }).value)


    println(reverseEval((1 to 10000).toList).value)
  }
}
