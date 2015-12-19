/**
  * Created by jesse on 12/19/15.
  */
object Day19Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day19-input.txt").getLines()

    val replacements = input.takeWhile(!_.isEmpty).map(replacement => {
      val Array(newVal, originalVal) = replacement.split(" => ")
      originalVal -> newVal
    }).toList.sortBy(-_._1.length)

    val originalFormula = input.next()

    findE(originalFormula)

    def findE(str: String, depth: Int = 1): Unit = {
      val matchingReplacements = replacements.filter(replacement => {
        str.indexOf(replacement._1) >= 0
      })

      if (matchingReplacements.nonEmpty) {
        matchingReplacements.foreach(replacement => {
          val newStr = str.replaceFirst(replacement._1, replacement._2)
          if (newStr == "e") {
            println(s"Found e in $depth replacements")
            sys.exit()
          } else {
            findE(newStr, depth + 1)
          }
        })
      }
    }
  }

}
