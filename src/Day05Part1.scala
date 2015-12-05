/**
  * Created by jesse on 12/5/15.
  */
object Day05Part1 {

  val vowels = Set('a', 'e', 'i', 'o', 'u')
  val restrictedStrings = Set("ab", "cd", "pq", "xy")

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day05-input.txt").getLines()

    val validStrings = input.filter(str => {
      val result = str.foldLeft(new RuleTracker)((x, y) => {
        RuleTracker(
          vowelCount = x.vowelCount + (if (vowels.contains(y)) 1 else 0),
          doubleLetterFound = x.doubleLetterFound || (y == x.previousChar),
          restrictedStringFound = x.restrictedStringFound || restrictedStrings.contains(s"${x.previousChar}$y"),
          previousChar = y
        )
      })

      println(s"$str : $result")

      result.isValid
    })

    println(s"Found ${validStrings.length} valid strings")

  }

  case class RuleTracker(vowelCount: Int = 0,
                         doubleLetterFound: Boolean = false,
                         restrictedStringFound: Boolean = false,
                         previousChar: Char = '!') {

    def isValid: Boolean = vowelCount >= 3 && doubleLetterFound && !restrictedStringFound

  }

}
