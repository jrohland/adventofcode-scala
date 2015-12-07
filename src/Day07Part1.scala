/**
  * Created by jesse on 12/7/15.
  */
object Day07Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day07-input.txt").getLines()

    val instructions = input.map(line => {
      val Array(action, wire) = line.split(" -> ")
      val splitAction = action.split(" ")

      (wire, splitAction.length match {
        case 1 => Instruction(Operation.Assign, splitAction(0), "")
        case 2 => Instruction(Operation.Not, splitAction(1), "")
        case 3 => splitAction(1) match {
          case "AND" => Instruction(Operation.And, splitAction(0), splitAction(2))
          case "OR" => Instruction(Operation.Or, splitAction(0), splitAction(2))
          case "LSHIFT" => Instruction(Operation.LShift, splitAction(0), splitAction(2))
          case "RSHIFT" => Instruction(Operation.RShift, splitAction(0), splitAction(2))
          case _ => throw new Exception(s"Unknown operation: $line")
        }
        case _ => throw new Exception(s"Unknown operation: $line")
      })
    }).toMap

    def calculateWire(wire: String, values: Map[String, Int] = Map()): Map[String, Int] = {
      println(s"Calculating wire: $wire")
      val instruction = instructions(wire)

      val (input1, input1ValueMap) = values.get(instruction.input1) match {
        case Some(value) => (value, values)
        case None =>
          if (isInt(instruction.input1)) {
            (instruction.input1.toInt, values)
          } else {
            val newInput1Values = calculateWire(instruction.input1, values)
            (newInput1Values(instruction.input1), newInput1Values)
          }
      }

      val (input2, input2ValueMap) = input1ValueMap.get(instruction.input2) match {
        case Some(value) => (value, input1ValueMap)
        case None =>
          if (instruction.input2.isEmpty) {
            (0, input1ValueMap)
          } else if (isInt(instruction.input2)) {
            (instruction.input2.toInt, input1ValueMap)
          } else {
            val newInput2Values = calculateWire(instruction.input2, input1ValueMap)
            (newInput2Values(instruction.input2), newInput2Values)
          }
      }

      val wireOutput = instruction.op match {
        case Operation.Assign => input1
        case Operation.And => input1 & input2
        case Operation.Or => input1 | input2
        case Operation.LShift => input1 << input2
        case Operation.RShift => input1 >> input2
        case Operation.Not => ~input1
      }

      input2ValueMap ++ Map(wire -> wireOutput)
    }

    val wireVals = calculateWire("a")
    println(s"A value: ${wireVals("a")}")

  }

  case class Instruction(op: Operation.Operation, input1: String, input2: String)

  object Operation extends Enumeration {
    type Operation = Value
    val Assign, And, Or, LShift, RShift, Not = Value
  }

  def isInt(str: String): Boolean = {
    try {
      str.toInt
      true
    } catch {
      case e: Exception => false
    }
  }

}
