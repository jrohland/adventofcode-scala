/**
  * Created by jesse on 12/17/15.
  */
object Day17Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day17-input.txt").getLines().map(_.toInt).toList
    val containers = input.zipWithIndex.toSet
    val combos = containers.subsets.filter(_.toList.map(_._1).sum == 150)
    val combosWithSize = combos.map(combo => combo.size -> combo).toList.groupBy(_._1)
    println(combosWithSize(combosWithSize.keys.toList.sortBy(identity).head).size)
  }

}
