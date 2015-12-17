/**
  * Created by jesse on 12/17/15.
  */
object Day17Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day17-input.txt").getLines().toList
    val containers = input.indices.map(i => {
      i -> input(i).toInt
    })
    val combos = (1 to containers.size).flatMap(i => {
      containers.combinations(i).filter(_.map(_._2).sum == 150)
    })
    println(combos.size)
  }

}
