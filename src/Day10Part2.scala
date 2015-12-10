/**
  * Created by jesse on 12/10/15.
  */
object Day10Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day10-input.txt").getLines().next().toCharArray.toList
    val encoded = (1 to 50).foldLeft(input)((x, y) => {
      println(s"Iteration $y")
      runLengthEncode(x)
    })
    println(encoded.length)
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
