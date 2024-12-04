package me.erikny.aoc2023

import scala.util.matching.Regex
import me.erikny.aoc.common.Inputs

class Day2 extends Inputs {
  val pattern = """([\d]+) (red|blue|green)""".r
  def parseGame(line: String) = {
    val strings : Array[String] = line.split(":")
    val playNumber = strings(0).replace("Game ", "").toInt
    val items: Map[String, Int] = strings(1).split(";").flatMap(subset => {
        val iterator: Iterator[Regex.Match] = pattern.findAllIn(subset).matchData
        iterator.map((rematch: Regex.Match) => (rematch.group(2), rematch.group(1).toInt))
      }).groupBy(_._1)
      .map(entry => entry._2.max)
    (playNumber, items)
  }

  def isAboveThreshold(item: (Int, Map[String, Int])): Boolean = {
    item._2("red") <= 12 &&
      item._2("green") <= 13 &&
      item._2("blue") <= 14
  }

  def part1(input: Seq[String]): Int = {
    input.map(parseGame)
      .filter(isAboveThreshold)
      .map(_._1)
      .sum
  }

  def part2(input: Seq[String]): Int = {
    input.map(parseGame)
      .map(_._2.values.product)
      .sum
  }
}
