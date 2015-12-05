/**
  * Created by jesse on 12/5/15.
  */
object Day05Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day05-input.txt").getLines()

    val validStrings = input.filter(str => {
      val result = str.foldLeft(new RuleTracker)((x, y) => {
        x.compareNextChar(y)
      })

      println(s"$str : ${result.isValid} : $result")
      result.isValid
    })

    println(s"Found ${validStrings.length} valid strings")
  }

  case class RuleTracker(pairs: List[(Char, Char)] = List(),
                        repeatCharFound: Boolean = false,
                        previousChars: (Char, Char, Char) = ('!', '!', '!')) {

    def compareNextChar(char: Char): RuleTracker = {
      RuleTracker(
        pairs = if ((char == previousChars._1 && (char == previousChars._2 && char != previousChars._3))
          || previousChars._1 == '!')
          pairs
        else
          pairs :+ (previousChars._1, char),
        repeatCharFound = repeatCharFound || char == previousChars._2,
        previousChars = (char, previousChars._1, previousChars._2)
      )
    }

    def isValid: Boolean = {
      pairs.groupBy(pair => pair).mapValues(_.size).exists(_._2 > 1) &&
      repeatCharFound
    }

  }

}
