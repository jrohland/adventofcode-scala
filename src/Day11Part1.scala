import util.control.Breaks._

/**
  * Created by jesse on 12/10/15.
  */
object Day11Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day11-input.txt").getLines().next()

    val start = input.map(char => {
      char.toInt - 97
    }).toList

    breakable {
      Stream.continually().foldLeft(start)((x, y) => {
        val nextStr = addOneToStr(x)
        if (testStr(nextStr)) {
          println(arrayToStr(nextStr))
          break
        }
        nextStr
      })
    }

  }

  def testStr(str: List[Int]): Boolean = {
    // (last 2 chars, pair count, pair found on last char, straight found, forbidden char found
    val result = str.foldLeft(((-1, -1), 0, false, false, false))((x, y) => {
      val pairFound = !x._3 && y == x._1._1
      val straightFound = y == x._1._1 + 1 && y == x._1._2 + 2
      val forbiddenChar = y match {
        case 'i' | 'o' | 'l' => true
        case _ => false
      }

      ((y, x._1._1), if (pairFound) x._2 + 1 else x._2, if (pairFound) true else false, (x._4 || straightFound), (x._5 || forbiddenChar))
    })

    result._2 >= 2 && result._4 && !result._5
  }

  def addOneToStr(str: List[Int]): List[Int] = {
    val results = str.foldRight((List[Int](), false, true))((x, y) => {
      if (y._2 || y._3) {
        if (x == 25) {
          (y._1 :+ 0, true, false)
        } else {
          (y._1 :+ x + 1, false, false)
        }
      } else {
        (y._1 :+ x, false, false)
      }
    })

    (if (results._2) {
      results._1 :+ 0
    } else {
      results._1
    }).reverse
  }

  def arrayToStr(arry: List[Int]): String = {
    arry.map(i => {
      (i + 97).toChar
    }).mkString
  }

}
