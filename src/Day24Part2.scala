/**
  * Created by jesse on 12/24/15.
  */
object Day24Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day24-input.txt").getLines()
    val packageWeights = input.map(_.toInt).toList
    val groupWeight = packageWeights.sum / 4

    println(s"Groups should weigh $groupWeight")

    val minQuantumEntanglement = Stream.from(2).map(i => {
      packageWeights.combinations(i).filter(_.sum == groupWeight)
    }).dropWhile(_.isEmpty).head.map(combo => {
      combo.map(_.toLong).product
    }).toList.sorted.head

    println(minQuantumEntanglement)

  }

}
