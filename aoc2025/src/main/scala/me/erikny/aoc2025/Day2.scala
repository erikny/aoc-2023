package me.erikny.aoc2025

import me.erikny.aoc.common.Inputs

class Day2 extends Inputs {

  private val patternSingle = """^(\d+)\1$""".r
  private val patternMultiple = """^(\d+)\1+$""".r
  private def isRepeating(s: String, multiple: Boolean): Boolean = {
    if (multiple) {
      patternMultiple.matches(s)
    } else {
      patternSingle.matches(s)
    }
  }

  def part1(input: Seq[String]): Long = {
    input.flatMap(splitLine(_, ','))
      .map(splitLine(_, '-'))
      .flatMap(arr => (arr(0).toLong to arr(1).toLong))
      .map(_.toString)
      .filter(isRepeating(_, multiple = false))
      .map(_.toLong)
      .sum
  }

  def part2(input: Seq[String]): Long = {
    input.flatMap(splitLine(_, ','))
      .map(splitLine(_, '-'))
      .flatMap(arr => (arr(0).toLong to arr(1).toLong))
      .map(_.toString)
      .filter(isRepeating(_, multiple = true))
      .map(_.toLong)
      .sum
  }
}
