/**
  * Created by jesse on 12/17/15.
  */
object Day16Part1 {

  val conditions = Map("children" -> 3,
    "cats" -> 7,
    "samoyeds" -> 2,
    "pomeranians" -> 3,
    "akitas" -> 0,
    "vizslas" -> 0,
    "goldfish" -> 5,
    "trees" -> 3,
    "cars" -> 2,
    "perfumes" -> 1)

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day16-input.txt").getLines()
    val sues = input.map(line => {
      line.substring(0, line.indexOf(":")) -> line.substring(line.indexOf(":") + 2).split(", ").map(value => {
        value.split(": ")
      }).map(values => {
        values(0) -> values(1).toInt
      }).toMap
    })

    val foundSue = sues.filter(sue => {
      conditions.map(condition => {
        sue._2.get(condition._1) match {
          case None | Some(condition._2) => true
          case _ => false
        }
      }).reduce((a, b) => {
        a && b
      })
    }).toList.head._1

    println(foundSue)
  }

}
