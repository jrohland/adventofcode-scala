/**
  * Created by jesse on 12/13/15.
  */
object Day13Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day13-input.txt").getLines()
    val userMap = input.map(line => {
      val split = line.stripSuffix(".").split(" ")
      split(0) -> (split(10), split(2) match {
        case "gain" => split(3).toInt
        case _ => -split(3).toInt
      })
    }).toList.groupBy(_._1).mapValues(values => {
      values.map(_._2).toMap
    })

    val totalHappiness = userMap.keys.toList.permutations.map(order => {
      (0 to order.size - 2).map(i => {
        userMap(order(i))(order(i + 1)) + userMap(order(i + 1))(order(i))
      }).sum + userMap(order.head)(order.last) + userMap(order.last)(order.head)
    }).toList.sorted.last

    println(totalHappiness)

  }

}
