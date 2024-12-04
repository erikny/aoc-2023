package me.erikny.aoc2024

import me.erikny.aoc.common.Inputs

class Day1 extends Inputs {

  def getNumbers(input: Seq[String]) = {
    input.foldLeft((Seq.empty[Int], Seq.empty[Int]))((agg, line) => {
      val values = toIntArray(line, ' ')
      (agg._1 :+ values(0), agg._2 :+ values(1))
    })

  }

  def part1(input: Seq[String]): Int = {
    val (left, right) = getNumbers(input)
    val ls = left.sorted
    val rs = right.sorted

    ls.zip(rs).foldLeft(0)((agg, next) => {
      agg + Math.abs((next._1 - next._2))
    })
  }

  def part2(input: Seq[String]): Int = {
    val (left, right) = getNumbers(input)
    val valueCount = right.groupBy(identity).view.mapValues(_.size).toMap.withDefaultValue(0)
    left.map(value => {
      value * valueCount(value)
    }).sum
  }
}
