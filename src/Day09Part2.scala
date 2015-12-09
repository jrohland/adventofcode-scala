/**
  * Created by jesse on 12/9/2015.
  */
object Day09Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day09-input.txt").getLines()

    val connections = input.flatMap(line => {
      val Array(locations, distance) = line.split(" = ")
      val Array(loc1, loc2) = locations.split(" to ")
      List(
        (loc1, loc2, distance.toInt),
        (loc2, loc1, distance.toInt)
      )
    }).toList.
      groupBy(_._1).
      mapValues(_.map(endpoints => {endpoints._2 -> endpoints._3}).toMap)

    val distances = connections.keys.toList.permutations.flatMap(path => {
      val distances = path.foldLeft(("", List[Option[Int]]()))((x, y) => {
        if (x._1.isEmpty) {
          (y, x._2)
        } else {
          (y, x._2 :+ connections(x._1).get(y))
        }
      })._2

      if (!distances.exists(_.isEmpty)) {
        List((path.mkString(" -> "), distances.map(_.get).sum))
      } else {
        List()
      }
    }).toList.sortBy(0 - _._2)

    println(distances.head)

  }

}
