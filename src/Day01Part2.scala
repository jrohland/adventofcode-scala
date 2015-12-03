/**
  * Created by jesse on 12/3/15.
  */
object Day01Part2 {

  def main(args: Array[String]): Unit = {
    var floor = 0
    var curStep = 1
    var firstBasement = -1
    val input = scala.io.Source.fromFile("input/day01-input.txt").getLines().next()
    input.foreach(direction => {
      direction match {
        case '(' => floor += 1
        case ')' => floor -= 1
      }
      if (floor == -1 && firstBasement == -1)
        firstBasement = curStep
      curStep += 1
    })
    println(s"Hit basement on step $firstBasement")
  }

}
