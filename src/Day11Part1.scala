import util.control.Breaks._

/**
  * Created by jesse on 12/10/15.
  */
object Day11Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day11-input.txt").getLines().next()

    breakable {
      Stream.continually().foldLeft(input.toCharArray.toList)((x, y) => {
        val nextStr = addOneToStr(x)
        if (testStr(nextStr)) {
          println(nextStr.mkString)
          break
        }
        nextStr
      })
    }

  }

  def testStr(str: List[Char]): Boolean = {
    // (last 2 chars, pair count, pair found on last char, straight found, forbidden char found
    val result = str.foldLeft((('!', '!'), 0, false, false, false))((x, y) => {
      val pairFound = !x._3 && y == x._1._1
      val straightFound = y == x._1._1 + 1 && y == x._1._2 + 2
      val forbiddenChar = y match {
        case 'i' | 'o' | 'l' => true
        case _ => false
      }

      ((y, x._1._1), if (pairFound) x._2 + 1 else x._2, if (pairFound) true else false, x._4 || straightFound, x._5 || forbiddenChar)
    })

    result._2 >= 2 && result._4 && !result._5
  }

  def addOneToStr(str: List[Char]): List[Char] = {
    val results = str.foldRight((List[Char](), true))((x, y) => {
      if (y._2) {
        if (x == 'z') {
          (y._1 :+ 'a', true)
        } else {
          (y._1 :+ (x + 1).toChar, false)
        }
      } else {
        (y._1 :+ x, false)
      }
    })

    (if (results._2) {
      results._1 :+ 'a'
    } else {
      results._1
    }).reverse
  }

}
