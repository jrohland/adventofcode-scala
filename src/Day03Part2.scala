/**
  * Created by jesse on 12/3/15.
  */
object Day03Part2 {

  def main(args: Array[String]): Unit = {
    var xPos1 = 0
    var yPos1 = 0
    var xPos2 = 0
    var yPos2 = 0
    var isSantaMove = false
    val input = scala.io.Source.fromFile("input/day03-input.txt").getLines().next()
    val delivered = (Array((0,0)) ++ input.map(direction => {
      isSantaMove = !isSantaMove
      direction match {
        case '^' => if (isSantaMove) yPos1 += 1 else yPos2 += 1
        case 'v' => if (isSantaMove) yPos1 -= 1 else yPos2 -= 1
        case '>' => if (isSantaMove) xPos1 += 1 else xPos2 += 1
        case '<' => if (isSantaMove) xPos1 -= 1 else xPos2 -= 1
      }
      if (isSantaMove)
        (xPos1, yPos1)
      else
        (xPos2, yPos2)
    })).toSet
    println(s"Unique places delivered to: ${delivered.size}")
  }

}
