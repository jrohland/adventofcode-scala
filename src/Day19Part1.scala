/**
  * Created by jesse on 12/19/15.
  */
object Day19Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day19-input.txt").getLines()

    val replacements = input.takeWhile(!_.isEmpty).map(replacement => {
      val Array(originalVal, newVal) = replacement.split(" => ")
      originalVal -> newVal
    }).toList.groupBy(_._1).mapValues(_.map(_._2))

    val originalFormula = input.next()

    val newFormulas = replacements.flatMap(replacement => {
      (0 to originalFormula.length - replacement._1.length).flatMap(i => {
        if (originalFormula.substring(i, i + replacement._1.length) == replacement._1)
          List(i)
        else
          List()
      }).flatMap(i => {
        replacement._2.map(newVal => {
          s"${originalFormula.substring(0, i)}$newVal${originalFormula.substring(i + replacement._1.length)}"
        })
      })
    }).toSet

    println(newFormulas.size)
  }

}
