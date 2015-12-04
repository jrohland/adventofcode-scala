/**
  * Created by jesse on 12/3/15.
  */
object Day01Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day01-input.txt").getLines().next()
    val floor = input.map({
      case '(' => 1
      case ')' => -1
    }).sum
    println(s"Stopped on floor $floor")
  }

}
