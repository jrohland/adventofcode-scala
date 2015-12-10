import util.control.Breaks._

/**
  * Created by jesse on 12/10/15.
  */
object Day10SpeedTest {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day10-input.txt").getLines().next().toCharArray.toList
    val oneMin = 6e+10
    val startTime = System.nanoTime()

    breakable {
      Stream.from(1).foldLeft(input)((x, y) => {
        if (System.nanoTime() - startTime >= oneMin)
          break
        println(s"Iteration $y")
        runLengthEncode(x)
      })
    }
  }

  def runLengthEncode(str: List[Char]): List[Char] = {
    str.foldLeft(List[(Char, Int)]())((x, y) => {
      x.headOption match {
        case Some(char) if char._1 == y => (y, char._2 + 1) :: x.tail
        case _ => (y, 1) :: x
      }
    }).reverse.flatMap(char => {
      char._2.toString.toCharArray :+ char._1
    })
  }

}
