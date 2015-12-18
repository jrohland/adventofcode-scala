/**
  * Created by jesse on 12/18/15.
  */
object Day18Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day18-input.txt").getLines().toList
    val initialLights = input.indices.map(rowIndex => {
      val row = input(rowIndex)
      (0 until row.length).map(colIndex => {
        if ((rowIndex == 0 && (colIndex == 0 || colIndex == row.length - 1)) ||
          (rowIndex == input.size - 1 && (colIndex == 0 || colIndex == row.length - 1))) {
          true
        } else {
          row.charAt(colIndex) == '#'
        }
      }).toList
    }).toList

    val currentLights = (0 until 100).foldLeft(initialLights)((lights, i) => {
      println(s"$i iterations")
      printLights(lights)
      lights.indices.map(rowIndex => {
        val row = lights(rowIndex)
        row.indices.map(colIndex => {
          val neighbors = litNeighbors(lights, rowIndex, colIndex)
          if ((rowIndex == 0 && (colIndex == 0 || colIndex == row.length - 1)) ||
            (rowIndex == lights.size - 1 && (colIndex == 0 || colIndex == row.length - 1))) {
            true
          } else if (row(colIndex)) {
            if (neighbors == 2 || neighbors == 3) {
              true
            } else {
              false
            }
          } else {
            if (neighbors == 3) {
              true
            } else {
              false
            }
          }
        }).toList
      }).toList
    })

    val numLit = currentLights.map(row => {
      row.map({
        case true => 1
        case _ => 0
      }).sum
    }).sum

    println(numLit)

  }

  def litNeighbors(lights: List[List[Boolean]], row: Int, col: Int): Int = {
    (Math.max(row - 1, 0) to Math.min(row + 1, lights.size - 1)).map(rowIndex => {
      val currentRow = lights(rowIndex)
      (Math.max(col - 1, 0) to Math.min(col + 1, currentRow.size - 1)).map(colIndex => {
        if ((rowIndex == row && colIndex == col) || !currentRow(colIndex)) {
          0
        } else {
          1
        }
      }).sum
    }).sum
  }

  def printLights(lights: List[List[Boolean]]) = {
    lights.foreach(row => {
      row.foreach({
        case true => print('#')
        case _ => print('.')
      })
      println()
    })
  }

}
