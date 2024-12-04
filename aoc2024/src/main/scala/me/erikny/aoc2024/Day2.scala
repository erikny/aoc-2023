package me.erikny.aoc2024

import me.erikny.aoc.common.Inputs

class Day2 extends Inputs {

  private def isSafe(levels: Array[Int]): Boolean = {
    val changes = levels
      .sliding(2)
      .map(v => v(1) - v(0)).toSeq
    changes.map(Math.abs).max < 4 &&
      (changes.forall(p => p > 0) || changes.forall(p => p < 0))
  }

  def part1(input: Seq[String]): Int = {
    input.count(v => isSafe(toIntArray(v)))
  }

  def part2(input: Seq[String]): Int = {
    input.count(values => {
      val ints = toIntArray(values)
      isSafe(ints) || ints.indices.exists(idx => isSafe(ints.patch(idx, Nil, 1)))
    })
  }
}
