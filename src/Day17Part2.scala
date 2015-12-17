/**
  * Created by jesse on 12/17/15.
  */
object Day17Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day17-input.txt").getLines().toList
    val containers = input.indices.map(i => {
      i -> input(i).toInt
    })
    val combos = (1 to containers.size).map(i => {
      i -> containers.combinations(i).filter(_.map(_._2).sum == 150)
    }).toMap.filter(_._2.nonEmpty)
    println(combos(combos.keys.toList.sortBy(identity).head).size)
  }

}
