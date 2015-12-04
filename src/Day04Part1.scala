import java.security.MessageDigest

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by jesse on 12/4/15.
  */
object Day04Part1 {

  // How many hashes each thread should do on an iteration
  val batchSize = 1000

  // Number of concurrent threads hashing
  val numThreads = 5

  def md5Str(str: String): String = {
    MessageDigest.getInstance("MD5").digest(str.getBytes).map("%02X".format(_)).mkString
  }

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day04-input.txt").getLines().next()
    val startTime = System.nanoTime()

    Stream.from(0).takeWhile(iteration => {
      val iterationFutures = (0 until numThreads).map(thread => {
        Future[Option[Int]] {
          val startValue = (iteration * numThreads * batchSize) + thread * batchSize
          val validHashes = (startValue until startValue + batchSize).dropWhile(curVal => {
            val md5Input = s"$input$curVal"
            val md5Output = md5Str(md5Input)
            !md5Output.startsWith("00000")
          })

          validHashes.headOption
        }
      })

      val iterationResults = Await.result(Future.sequence(iterationFutures), Duration.Inf).flatMap({
        case None => List()
        case Some(i) => List(i)
      })

      val runTime = (System.nanoTime() - startTime).toDouble / Math.pow(10, 9)
      val hashesCalculated = (iteration + 1) * numThreads * batchSize
      val hashesPerSecond = hashesCalculated / runTime
      println(s"$hashesCalculated hashes calculated")
      println(s"$hashesPerSecond hashes per second")

      if (iterationResults.nonEmpty) {
        println(s"Found matching hash at: ${iterationResults.sorted.head}")
        false
      } else {
        true
      }
    }).force

    val runTime = (System.nanoTime() - startTime).toDouble / Math.pow(10, 9)
    println(s"Runtime: $runTime sec")

  }

}
