package me.erikny.aoc2025

import me.erikny.aoc.common.Inputs

class Day2 extends Inputs {

  def part1(input: Seq[String]): Long = {
    input.flatMap(splitLine(_, ','))
      .map(splitLine(_, '-'))
      .flatMap(arr => (arr(0).toLong to arr(1).toLong))
      .map(_.toString)
      .filter(s => {
        val (l, r) = s.splitAt(s.length/2)
        l == r
      })
      .map(_.toLong)
      .sum
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
