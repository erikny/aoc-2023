package me.erikny.aoc2025

import me.erikny.aoc.common.Inputs

class Day6 extends Inputs {

  def part1(input: Seq[String]): Long = {
    input
      .map(_.split(" ").toSeq.filter(_.nonEmpty))
      .transpose
      .map(
        problem =>
          problem.last match {
            case "+" => problem.init.map(_.toLong).sum
            case "*" => problem.init.map(_.toLong).product
          }
      )
      .sum
  }

  def part2(input: Seq[String]): Long = {
    groups[String](input.map(_.toCharArray.toSeq)
      .transpose
      .map(_.toArray.mkString),
      line => line.isBlank)
      .map{
        problem =>
          if(problem.mkString("").contains("+")) {
            problem.map(_.replace("+", "").trim.toLong).sum
          } else {
            problem.map(_.replace("*", "").trim.toLong).product
          }
      }
      .sum
  }
}
