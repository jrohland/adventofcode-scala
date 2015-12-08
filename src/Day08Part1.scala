/**
  * Created by jesse on 12/8/15.
  */
object Day08Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day08-input.txt").getLines()

    val totalEscapedCount = input.map(str => {
      val escapedCharCount = (1 to str.length - 2).
        partition(str.charAt(_) == '\\')._1.
        foldLeft((List[Int](), Int.MinValue, false))((x, y) => {
          if (x._2 == y - 1 && x._3) {
            (x._1, y, false)
          } else {
            (x._1 :+ y, y, true)
          }
        })._1.map(index => {
          str.charAt(index + 1) match {
            case 'x' => 3
            case _ => 1
          }
        }).sum + 2

      escapedCharCount
    }).sum

    println(s"Total escaped: $totalEscapedCount")

  }

}
