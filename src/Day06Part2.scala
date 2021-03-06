/**
  * Created by jesse on 12/6/15.
  */
object Day06Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day06-input.txt").getLines()
    val lights = Array.ofDim[Int](1000, 1000)

    input.map(instruction => {
      val split = instruction.split(" ")
      if (split(0).equals("turn")) {
        LightInstruction(
          action = if (split(1).equals("on")) Action.TurnOn else Action.TurnOff,
          start = split(2).split(",").map(_.toInt) match {
            case Array(x, y) => (x, y)
          },
          end = split(4).split(",").map(_.toInt) match {
            case Array(x, y) => (x, y)
          }
        )
      } else {
        LightInstruction(
          action = Action.Toggle,
          start = split(1).split(",").map(_.toInt) match {
            case Array(x, y) => (x, y)
          },
          end = split(3).split(",").map(_.toInt) match {
            case Array(x, y) => (x, y)
          }
        )
      }
    }).foreach(instruction => {
      for (x <- instruction.start._1 to instruction.end._1) {
        for (y <- instruction.start._2 to instruction.end._2) {
          instruction.action match {
            case Action.TurnOn => lights(x)(y) += 1
            case Action.TurnOff => lights(x)(y) = scala.math.max(0, lights(x)(y) - 1)
            case Action.Toggle => lights(x)(y) += 2
          }
        }
      }
    })

    val brightness = lights.map(_.sum).sum

    println(s"Total brightness: $brightness")

  }

  case class LightInstruction(action: Action.Action, start: (Int, Int), end: (Int, Int))

  object Action extends Enumeration {
    type Action = Value
    val TurnOn, TurnOff, Toggle = Value
  }

}
