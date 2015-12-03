/**
  * Created by jesse on 12/3/15.
  */
object Day03Part1 {

  def main(args: Array[String]): Unit = {
    var xPos = 0
    var yPos = 0
    val input = scala.io.Source.fromFile("input/day03-input.txt").getLines().next()
    val delivered = (Array((0,0)) ++ input.map(direction => {
      direction match {
        case '^' => yPos += 1
        case 'v' => yPos -= 1
        case '>' => xPos += 1
        case '<' => xPos -= 1
      }
      (xPos, yPos)
    })).toSet
    println(s"Unique places delivered to: ${delivered.size}")
  }

}
