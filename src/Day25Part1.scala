/**
  * Created by jesse on 12/25/15.
  */
object Day25Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day25-input.txt").getLines()
    val Array(row, col) = input.next().split("row ")(1).split(", column ").map(_.stripSuffix(".")).map(_.toInt)

    val codeIndex = (1 to (row + col - 2)).sum + col

    println(s"Need to find code $codeIndex")

    val code = (2 to codeIndex).foldLeft(20151125l)((previousNumber, i) => {
      val code = (previousNumber * 252533) % 33554393
      println(s"Code $i : $code")
      code
    })

    println(code)
  }

}
