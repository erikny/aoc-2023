package me.erikny.aoc2025

import me.erikny.aoc.common.Inputs

class Day3 extends Inputs {
  def find(targetCount: Int, input: String): Option[Long] = {
    val content = input.map(_.asDigit).zipWithIndex.sortBy{case (value, offset) => (-value, offset)}

    def findInner(soFar: String, maxValue: Int, offset: Int): Option[Long] = {
      val availableDigits: Seq[(Int, Int)] = content.filter(x => (x._1 <= maxValue && x._2 >= offset))
      availableDigits match {
        case Nil => None
        case (value, _) +: _ if soFar.length == targetCount-1 =>
          Some((soFar + value).toLong)
        case (value, index) +: _ =>
          findInner((soFar + value), 9, index+1) match {
            case x if x.isDefined => x
            case _ => findInner(soFar, value-1, offset)
          }
      }
    }
    findInner(soFar = "", content.head._1, 0)
  }

  def part1(input: Seq[String]): Long = {
    input
      .map(find(2, _))
      .map(_.get).sum
  }

  def part2(input: Seq[String]): Long = {
    input
      .map(find(12, _))
      .map(_.get).sum
  }
}
