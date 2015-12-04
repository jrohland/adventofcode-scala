/**
  * Created by jesse on 12/3/15.
  */
object Day01Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day01-input.txt").getLines().next().map({
      case '(' => 1
      case ')' => -1
    })

    // Fold a tuple of current floor and floor count
    input.foldLeft((0, 0))((x, y) => {
      if (x._1 + y == -1) {
        println(s"Hit basement on step ${x._2 + 1}")
        return
      }
      (x._1 + y, x._2 + 1)
    })

    println("Never hit basement")

  }

}
