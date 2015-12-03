/**
  * Created by jesse on 12/3/15.
  */
object Day01Part1 {

  def main(args: Array[String]): Unit = {
    var floor = 0
    val input = scala.io.Source.fromFile("input/day01-input.txt").getLines().next()
    input.foreach({
      case '(' => floor += 1
      case ')' => floor -= 1
    })
    println(s"Stopped on floor $floor")
  }

}
