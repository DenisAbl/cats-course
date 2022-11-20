package part3datamanipulation

import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

object MyWriters {

  import cats.data.Writer
  // 1 - define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)
  // 2 - manipulate them with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting") // value stays the same, logs change
  val aWriterWithBoth = aWriter.bimap(_ :+ "found something interesting", _ + 1) // both value and logs change
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something interesting", value + 1)
  }

  // flatMap
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

  // TODO 1: rewrite a function which "prints" things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {

    @tailrec
    def cal(writer: Writer[Vector[String], Int], counter: Int): Writer[Vector[String], Int]  = {
      if (counter <= n) cal(writer.mapBoth((log, value) => {(log :+ counter.toString, value)}), counter + 1)
      else writer
    }

//    def calFlatMap(writer: Writer[Vector[String], Int], counter: Int): Writer[Vector[String], Int]  = {
//      if (counter <= n) calFlatMap(writer.flatMap(_ => Writer(Vector(counter.toString), n), counter + 1))
//      else writer
//    }

    cal(Writer(Vector("starting"), n), 0)
//    calFlatMap(Writer(Vector("starting"), n), 0)

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

  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {

    @tailrec
    def summing(acc: Writer[Vector[String], Int], elem: Int): Writer[Vector[String], Int] = {
      if (elem <= 0) acc.mapBoth((logs, value) => (logs :+ "Zero passed. Finished counting", value))
      else {
        summing(acc.mapBoth((logs, value) =>
          (logs ++ Vector(s"Now at $elem",s"Computed sum(${elem}) = ${value + elem}" ), value + elem)), elem - 1)
      }
    }

    summing(Writer(Vector(), 0), n)
  }

  // Benefit #2: Writers can keep logs separate on multiple threads

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    // ex 1
    countAndSay(10)
    countAndLog(10).written.foreach(println)
    // ex 2
//    Future(naiveSum(100)).foreach(println)
//    Future(naiveSum(100)).foreach(println)
//
//    val sumFuture1 = Future(sumWithLogs(100))
//    val sumFuture2 = Future(sumWithLogs(100))
//    val logs1 = sumFuture1.map(_.written) // logs from thread 1
//    val logs2 = sumFuture2.map(_.written) // logs from thread 2

     sumWithLogs(100).written.foreach(println)
  }
}
