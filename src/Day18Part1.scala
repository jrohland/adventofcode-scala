/**
  * Created by jesse on 12/18/15.
  */
object Day18Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day18-input.txt").getLines()
    val initialLights = input.map(row => {
      row.map(_ == '#').toList
    }).toList

    val currentLights = (0 until 100).foldLeft(initialLights)((lights, i) => {
      println(s"$i iterations")
      printLights(lights)
      lights.indices.map(rowIndex => {
        val row = lights(rowIndex)
        row.indices.map(colIndex => {
          val neighbors = litNeighbors(lights, rowIndex, colIndex)
          (row(colIndex), neighbors) match {
            case (true, 2) | (true, 3) | (false, 3) => true
            case _ => false
          }
        }).toList
      }).toList
    })

    val numLit = currentLights.map(row => {
      row.count(identity)
    }).sum

    println(numLit)

  }

  def litNeighbors(lights: List[List[Boolean]], row: Int, col: Int): Int = {
    (for (rowIndex <- Math.max(row - 1, 0) to Math.min(row + 1, lights.size - 1);
       colIndex <- Math.max(col - 1, 0) to Math.min(col + 1, lights.head.size - 1)
       if !(rowIndex == row && colIndex == col)) yield lights(rowIndex)(colIndex)).count(identity)
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
