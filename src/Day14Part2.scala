/**
  * Created by jesse on 12/14/15.
  */
object Day14Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day14-input.txt").getLines()
    val reindeers = input.map(line => {
      val split = line.split(" ")
      Reindeer(split(0), split(3).toInt, split(6).toInt, split(13).toInt)
    }).toList

    val scores = (0 until 2503).foldLeft(List[Map[String, Int]]())((distances, sec) => {
      distances :+ reindeers.map(reindeer => {
        val curDistance = if (distances.isEmpty) {
          0
        } else {
          distances.last(reindeer.name)
        }
        val newDistance = if (sec % (reindeer.flyingTime + reindeer.restingTime) < reindeer.flyingTime) {
          curDistance + reindeer.flyingSpeed
        } else {
          curDistance
        }
        reindeer.name -> newDistance
      }).toMap
    }).flatMap(secDistances => {
      secDistances.foldLeft((0, List[String]()))((curMax, reindeer) => {
        if (reindeer._2 > curMax._1) {
          (reindeer._2, List(reindeer._1))
        } else if (reindeer._2 == curMax._1) {
          (curMax._1, curMax._2 :+ reindeer._1)
        } else {
          (curMax._1, curMax._2)
        }
      })._2
    }).groupBy(identity).mapValues(_.size).toList.sortBy(-_._2)

    scores.foreach(println)

  }

  case class Reindeer(name: String, flyingSpeed: Int, flyingTime: Int, restingTime: Int)

}
