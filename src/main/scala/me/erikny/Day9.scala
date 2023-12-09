package me.erikny

import scala.annotation.tailrec

class Day9 extends Inputs {

  private def extrapolateValue(numbers: Array[Long]): Long = {
    if (numbers.distinct.length == 1)
      return numbers.distinct.head
    val diff = numbers.sliding(2).map(t => t(1) - t(0)).toArray
    numbers(numbers.length-1) + extrapolateValue(diff)
  }

  def part1(input: Seq[String]): Long = {
    val value = input.map(a => toLongArray(a))
    value.map(extrapolateValue).sum
  }

  def part2(input: Seq[String]): Long = {
    val value = input.map(a => toLongArray(a))
    value.map(_.reverse).map(extrapolateValue).sum
  }
}
