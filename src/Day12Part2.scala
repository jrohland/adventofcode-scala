import scala.util.parsing.json.JSON

/**
  * Created by jesse on 12/12/15.
  */
object Day12Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day12-input.txt").getLines().next()
    JSON.globalNumberParser = {in => try { in.toInt } catch { case e: Exception => in.toDouble}}
    val parsed = JSON.parseFull(input).get
    val sum = findNumerics(parsed)
    println(s"Total sum: $sum")
  }

  def findNumerics(block: Any): Int = {
    block match {
      case l: List[_] => l.map(findNumerics).sum
      case m: Map[_, _] => if (m.values.toList.contains("red")) 0 else m.values.map(findNumerics).sum
      case i: Int => i
      case _ => 0
    }
  }

}
