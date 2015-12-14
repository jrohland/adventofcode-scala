/**
  * Created by jesse on 12/14/15.
  */
object Day14Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day14-input.txt").getLines()
    val flyingDistances = input.map(line => {
      val split = line.split(" ")
      Reindeer(split(0), split(3).toInt, split(6).toInt, split(13).toInt)
    }).map(reindeer => {
      (reindeer.name, (0 until 2503).map(sec => {
        if (sec % (reindeer.flyingTime + reindeer.restingTime) < reindeer.flyingTime) {
          reindeer.flyingSpeed
        } else {
          0
        }
      }).sum)
    }).toList.sortBy(-_._2)
    flyingDistances.foreach(println)
  }

  case class Reindeer(name: String, flyingSpeed: Int, flyingTime: Int, restingTime: Int)

}
