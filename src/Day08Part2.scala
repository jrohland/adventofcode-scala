/**
  * Created by jesse on 12/8/15.
  */
object Day08Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day08-input.txt").getLines()

    val totalNewChars = input.map(str => {
      str.map({
        case '\\' | '"' => 2
        case _ => 1
      }).sum + 2 - str.length
    }).sum

    println(s"Total new chars: $totalNewChars")

  }

}
