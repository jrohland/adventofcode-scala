/**
  * Created by jesse on 12/8/15.
  */
object Day08Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day08-input.txt").getLines()

    val totalEscapedCount = input.map(str => {
      val unescapedLength = str.substring(1, str.length - 1).foldLeft((false, 0))((x, y) => {
        if (x._1) {
          y match {
            case 'x' => (false, x._2 - 1)
            case _ => (false, x._2 + 1)
          }
        } else {
          y match {
            case '\\' => (true, x._2)
            case _ => (false, x._2 + 1)
          }
        }
      })._2
      str.length - unescapedLength
    }).sum

    println(s"Total escaped: $totalEscapedCount")

  }

}
