import scala.collection.mutable.Map

/**
  * Created by jesse on 12/24/15.
  */
object Day23Part2 {

  val registers = Map(
    "a" -> 1l,
    "b" -> 0l
  )

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day23-input.txt").getLines()
    val instructions = input.map(line => {
      line.substring(0, 3) -> line.substring(4)
    }).toList

    var i = 0
    while (i < instructions.length) {
      i = i + runInstruction(instructions(i))
    }

    println(registers("b"))
  }

  def runInstruction(instruction: (String, String)): Int = {
    instruction._1 match {
      case "hlf" =>
        registers(instruction._2) = registers(instruction._2) / 2
        1
      case "tpl" =>
        registers(instruction._2) = registers(instruction._2) * 3
        1
      case "inc" =>
        registers(instruction._2) = registers(instruction._2) + 1
        1
      case "jmp" =>
        instruction._2.toInt
      case "jie" =>
        val Array(register, jump) = instruction._2.split(", ")
        if (registers(register) % 2 == 0)
          jump.toInt
        else
          1
      case "jio" =>
        val Array(register, jump) = instruction._2.split(", ")
        if (registers(register) == 1)
          jump.toInt
        else
          1
    }
  }

}
