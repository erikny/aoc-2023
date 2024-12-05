package me.erikny.aoc2024

import me.erikny.aoc.common.Inputs

class Day3 extends Inputs {

  def part1(input: Seq[String]): Int = {
    val pattern = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r
    input
      .map(pattern.findAllMatchIn(_)).flatMap(matches => {
        matches.map(m => (m.group(1).toInt * (m.group(2)).toInt))
      }).sum
  }

  def part2(input: Seq[String]): Int = {
    val oneliner = input.mkString("")
    val removal = "don't\\(\\).*?do\\(\\)".r
    val cleaned = removal.findAllMatchIn(oneliner)
      .foldLeft(oneliner)((reduced, group) => {
        reduced.replace(group.group(0), "")
      })
    part1(Seq(cleaned))
  }
}
