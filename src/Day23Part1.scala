/**
  * Created by jesse on 12/24/15.
  */
object Day23Part1 {

  val initialRegisters = Map(
    "a" -> 0l,
    "b" -> 0l
  )

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day23-input.txt").getLines()
    val instructions = input.map(line => {
      line.substring(0, 3) -> line.substring(4)
    }).toList

    val result = runInstructions(instructions, initialRegisters)
    println(result)
  }

  def runInstructions(instructions: List[(String, String)], initialRegisters: Map[String, Long]): Long = {
    Stream.from(0).foldLeft((initialRegisters, 0))((values, instructionsExecuted) => {
      val registers = values._1
      val instructionIndex = values._2

      if (instructionIndex >= instructions.length) {
        return registers("b")
      } else {
        val instruction = instructions(instructionIndex)
        instruction._1 match {
          case "hlf" =>
            (registers+ (instruction._2 -> (registers(instruction._2) / 2)), instructionIndex + 1)
          case "tpl" =>
            (registers + (instruction._2 -> (registers(instruction._2) * 3)), instructionIndex + 1)
          case "inc" =>
            (registers + (instruction._2 -> (registers(instruction._2) + 1)), instructionIndex + 1)
          case "jmp" =>
            (registers, instructionIndex + instruction._2.toInt)
          case "jie" =>
            val Array(register, jump) = instruction._2.split(", ")
            if (registers(register) % 2 == 0)
              (registers, instructionIndex + jump.toInt)
            else
              (registers, instructionIndex + 1)
          case "jio" =>
            val Array(register, jump) = instruction._2.split(", ")
            if (registers(register) == 1)
              (registers, instructionIndex + jump.toInt)
            else
              (registers, instructionIndex + 1)
        }
      }
    })
    -1
  }

}
