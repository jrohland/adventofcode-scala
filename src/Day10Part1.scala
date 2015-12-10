/**
  * Created by jesse on 12/10/15.
  */
object Day10Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day10-input.txt").getLines().next()
    val encoded = (1 to 40).foldLeft(input)((x, y) => {
      println(s"Iteration $y")
      runLengthEncode(x)
    })
    println(encoded.length)
  }

  def runLengthEncode(str: String): String = {
    str.foldLeft(List[(Char, Int)]())((x, y) => {
      x.headOption match {
        case Some(char) if char._1 == y => (y, char._2 + 1) :: x.tail
        case _ => (y, 1) :: x
      }
    }).reverse.map(char => {
      s"${char._2}${char._1}"
    }).mkString
  }

}
