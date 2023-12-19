package me.erikny

import me.erikny.util.Numerics.hexToInt
import me.erikny.util.Polygon
import me.erikny.util.TupleHelpers.{East, North, South, TupleDirection, West}

class Day18 extends Inputs {

  case class Instruction(direction: (Long, Long), steps: Long, color: String)

  object DigReader {
    def apply(lines: Seq[String]): Seq[Instruction] = {
      lines.map(l => {
        val parts = l.split(" ")
        val numericDirection = parts(0).head match {
          case 'R' => East
          case 'L' => West
          case 'U' => North
          case 'D' => South
        }
        Instruction(numericDirection, parts(1).toInt, parts(2).stripPrefix("(#").stripSuffix(")"))
      })
    }
  }

  def part1(input: Seq[String]): Long = {
    val instructions = DigReader(input)
    solve(instructions)
  }

  def part2(input: Seq[String]): Long = {
    val instructions: Seq[Instruction] = DigReader(input)
      .map(instr => {
        val newDirection = instr.color.last match {
          case '0' => East
          case '1' => South
          case '2' => West
          case '3' => North
        }
        val str = instr.color.take(5)
        instr.copy(direction = newDirection, steps = hexToInt(str).toLong)
      })
    solve(instructions)
  }

  def solve(instructions: Seq[Instruction]) = {
    val startingPoint = Seq((0L, 0L))
    val value = instructions.foldLeft(startingPoint)((acc, current) => {
      acc.head.moveDistance(current.direction, current.steps) +: acc
    }).distinct.reverse
    val l = Polygon.shoelace(value)
    val trench = instructions.map(_.steps).sum
    l + trench/2 + 1
  }
}
