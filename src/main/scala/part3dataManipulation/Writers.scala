package part3dataManipulation

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Writers {

  import cats.data.Writer

  // 1 - define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)
  // 2 - manipulate them with pure FP
  val anyIncreasedWriter = aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting") // value stays the same, logs change
  val aWriterWithBoth = aWriter.bimap(_ :+ "found something interesting", _ + 1) // both value and logs change
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something interesting", value + 1)
  }

  // flatMap on writers

  import cats.instances.vector._ // imports a Semigroup[Vector]

  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs

  import cats.instances.list._ // an implicit Monoid[List[Int]]

  val anEmptyWriter = aWriter.reset // clear the logs, keep the value

  // 3 - dump either the value or the logs
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run

  // TODO 1: rewriter a function wgich "prints" thigs with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("Starting"), 0)
    else {
      countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))
    }
  }

  // Benefit #1: we work with pure FP

  // TODO 2: rewrite this method with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def naiveWriterSum(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- naiveWriterSum(n - 1)
      _ <- Writer(Vector(s"Computed sum (${n - 1}) = $lowerSum"), n)
    } yield lowerSum + n
  }

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  def main(args: Array[String]): Unit = {
    // ex 2
    Future(naiveWriterSum(10).written.foreach(println))
    Future(naiveWriterSum(10).written.foreach(println))

  }
}
