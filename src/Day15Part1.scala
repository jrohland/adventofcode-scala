/**
  * Created by jesse on 12/15/15.
  */
object Day15Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day15-input.txt").getLines()
    val ingredients = input.map(line => {
      val Array(_, _, capacity, _, durability, _, flavor, _, texture, _, calories) =
        line.replace(",", "").replace(":", "").split(" ")
      List(capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
    }).toList

    val score = (for (a <- 0 to 100; b <- 0 to 100; c <- 0 to 100; d <- 0 to 100 if a + b + c + d == 100)
      yield (a, b, c, d)).map(combo => {

      val multipliers = combo.productIterator.toList.map(_.asInstanceOf[Int])
      (0 until 4).map(property => {
        Math.max(multipliers.indices.map(i => {
          ingredients(i)(property) * multipliers(i)
        }).sum, 0)
      }).product

    }).sortBy(-_).head

    println(score)

  }

}
