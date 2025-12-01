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
    val allsteps = input.foldLeft(Seq(Move(0, R, 0, 0, 50)))((acc, s) => {
      val value = s.substring(1).toInt
      val loops = value / 100
      val steps = value % 100
      val previousPos = acc.head.to
      s.charAt(0) match {
        case 'L' =>
          val nextPos = previousPos - steps
          Move(previousPos, L, loops, steps, if (nextPos < 0) (100 - Math.abs(nextPos)) else nextPos) +: acc
        case 'R' => R
          val nextPos = (previousPos + value) % 100
          Move(previousPos, R, loops, steps, nextPos) +: acc
      }
    }).init.reverse
    allsteps.foreach{
      step => println(s"${step.direction}${step.loops * 100 + step.steps}: from=${step.from} to=${step.to} breaches=${step.breaches}")
    }
    allsteps.map(_.breaches).sum
//    val ints = input.foldLeft(Seq((0, 50)))((acc, s) => {
//      val char = s.charAt(0)
//      val value = s.substring(1).toInt
//      val hundreds = value / 100
//      char match {
//        case 'L' =>
//          val next = acc.head._2 - value
//          val breaches = hundreds + (if (next < 0 ) 1 else 0)
//          val nextTick = if(next < 0) (100 - Math.abs(next)) else next
//          println(s"$char$value => start=${acc.head._2} next=$next breaches=$breaches nextTick=$nextTick")
//          (breaches, nextTick) +: acc
//        case 'R' =>
//          val next = acc.head._2 + value
//          val breaches = hundreds + (if (next > 99 ) 1 else 0)
//          val nextTick = (next % 100)
//          println(s"$char$value => start=${acc.head._2} next=$next breaches=$breaches nextTick=$nextTick")
//          (breaches, nextTick) +: acc
//      }
//    })
//    ints.map(p => p._1).sum
  }
}
