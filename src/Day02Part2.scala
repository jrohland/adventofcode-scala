/**
  * Created by jesse on 12/3/15.
  */
object Day02Part2 {

  def main (args: Array[String]) {
    val input = scala.io.Source.fromFile("input/day02-input.txt").getLines()
    val size = input.map(size => {
      val dims = size.split("x").map(_.toInt).sorted
      2*dims(0)+2*dims(1)+dims(0)*dims(1)*dims(2)
    }).sum
    println(s"Ribbon size: $size")
  }

}
