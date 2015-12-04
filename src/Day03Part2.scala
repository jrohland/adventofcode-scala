/**
  * Created by jesse on 12/3/15.
  */
object Day03Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day03-input.txt").getLines().next().map({
      case '^' => (0, 1)
      case 'v' => (0, -1)
      case '>' => (1, 0)
      case '<' => (-1, 0)
    })

    // Scan with a tuple tracking two locations and a bool for which to update
    val delivered = input.scanLeft((true, 0, 0, 0, 0))((x, y) => {
      if (x._1)
        (false, x._2 + y._1, x._3 + y._2, x._4, x._5)
      else
        (true, x._2, x._3, x._4 + y._1, x._5 + y._2)
    }).flatMap(pos => {
      Array(
        (pos._2, pos._3),
        (pos._4, pos._5)
      )
    }).toSet
    println(s"Unique places delivered to: ${delivered.size}")
  }

}
