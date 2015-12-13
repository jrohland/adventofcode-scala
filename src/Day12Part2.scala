import scala.util.parsing.json.JSON

/**
  * Created by jesse on 12/12/15.
  */
object Day12Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day12-input.txt").getLines().next()
    JSON.globalNumberParser = {in => try { in.toInt } catch { case e: Exception => in.toDouble}}
    val parsed = JSON.parseFull(input).get
    val sum = findNumerics(parsed)._1
    println(s"Total sum: $sum")
  }

  def findNumerics(block: Any): (Int, Boolean) = {
    block match {
      case l: List[_] => (l.map(findNumerics).map(_._1).sum, false)
      case m: Map[_, _] =>
        val values = m.values.map(findNumerics).toList
        if (values.exists(_._2)) (0, false) else (values.map(_._1).sum, false)
      case i: Int => (i, false)
      case "red" => (0, true)
      case _ => (0, false)
    }
  }

}
