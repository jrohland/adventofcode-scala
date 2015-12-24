/**
  * Created by jesse on 12/24/15.
  */
object Day24Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day24-input.txt").getLines()
    val packageWeights = input.map(_.toInt).toList
    val groupWeight = packageWeights.sum / 3

    println(s"Groups should weigh $groupWeight")

    val combosFound = Stream.from(2).dropWhile(i => {
      !packageWeights.combinations(i).exists(_.sum == groupWeight)
    }).head

    println(s"Combos found at $combosFound")

    val products = packageWeights.combinations(combosFound).filter(_.sum == groupWeight).map(combo => {
      combo.map(_.toLong).product
    })

    val minQuantumEntanglement = products.toList.sorted.head

    println(minQuantumEntanglement)

  }

}
