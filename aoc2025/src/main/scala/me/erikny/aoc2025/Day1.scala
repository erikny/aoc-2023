package me.erikny.aoc2025

import me.erikny.aoc.common.Inputs

class Day1 extends Inputs {
  sealed trait Direction
  case object L extends Direction
  case object R extends Direction
  case class Move(from: Int, direction: Direction, loops: Int, steps: Int, to: Int) {
    def breaches: Int = {
      loops + ((from,to,steps,direction) match {
        case(_, 0, _, _) => 1
        case(start, _, steps, L) if (start > 0) && (start - steps) < 0 => 1
        case(start, _, steps, R) if (start + steps) > 99 => 1
        case _ => 0
      })

    }
  }

  def part1(input: Seq[String]): Int = {
    val ints = input.foldLeft(Seq(50))((acc, s) => {
      val char = s.charAt(0)
      val value = s.substring(1).toInt
      char match {
        case 'L' => (acc.head - value) % 100 +: acc
        case 'R' => (acc.head + value) % 100 +: acc
      }
    })
    ints.count(p => p == 0)
  }

  def part2(input: Seq[String]): Int = {
    input.foldLeft((0, 50))((acc, s) => {
      val (breaches, previousPos) = acc
      val char = s.charAt(0)
      val totalSteps = s.substring(1).toInt
      val loops = totalSteps / 100
      val actualSteps = totalSteps % 100
      val newPos = char match {
        case 'L' =>
          val next = previousPos - actualSteps
          if(next < 0) (100 - Math.abs(next)) else next
        case 'R' =>
          (previousPos + actualSteps) % 100
      }
      val thisBreach = (newPos, char) match {
        case (0, _) => loops + 1
        case (newPos, 'L') if (previousPos > 0) && (newPos > previousPos) =>
          loops + 1
        case (newPos, 'R') if (newPos < previousPos) =>
          loops + 1
        case _ => loops
      }
      println(s"${s}: from=${previousPos} to=$newPos breaches=${thisBreach}")
      (breaches + thisBreach, newPos)
    })._1

//
//
//    val allsteps = input.foldLeft(Seq(Move(0, R, 0, 0, 50)))((acc, s) => {
//      val value = s.substring(1).toInt
//      val loops = value / 100
//      val steps = value % 100
//      val previousPos = acc.head.to
//      s.charAt(0) match {
//        case 'L' =>
//          val nextPos = previousPos - steps
//          Move(previousPos, L, loops, steps, if (nextPos < 0) (100 - Math.abs(nextPos)) else nextPos) +: acc
//        case 'R' => R
//          val nextPos = (previousPos + value) % 100
//          Move(previousPos, R, loops, steps, nextPos) +: acc
//      }
//    }).init.reverse
//    allsteps.foreach{
//      step => println(s"${step.direction}${step.loops * 100 + step.steps}: from=${step.from} to=${step.to} breaches=${step.breaches}")
//    }
//    allsteps.map(_.breaches).sum
  }
}
