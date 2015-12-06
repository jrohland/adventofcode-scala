/**
  * Created by jesse on 12/6/15.
  */
object Day06Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day06-input.txt").getLines()
    val lights = Array.ofDim[Boolean](1000, 1000)

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
            case Action.TurnOn => lights(x)(y) = true
            case Action.TurnOff => lights(x)(y) = false
            case Action.Toggle => lights(x)(y) = !lights(x)(y)
          }
        }
      }
    })

    val lightCount = lights.map(_.count(_ == true)).sum

    println(s"There are $lightCount lights on")

  }

  case class LightInstruction(action: Action.Action, start: (Int, Int), end: (Int, Int))

  object Action extends Enumeration {
    type Action = Value
    val TurnOn, TurnOff, Toggle = Value
  }

}
