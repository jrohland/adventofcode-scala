/**
  * Created by jesse on 12/3/15.
  */
object Day03Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day03-input.txt").getLines().next().map({
      case '^' => (0, 1)
      case 'v' => (0, -1)
      case '>' => (1, 0)
      case '<' => (-1, 0)
    })

    val delivered = input.scanLeft((0, 0))((x, y) => {
      (x._1 + y._1, x._2 + y._2)
    }).toSet

    println(s"Unique places delivered to: ${delivered.size}")
  }

}
