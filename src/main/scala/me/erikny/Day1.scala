package me.erikny
import scala.util.matching.Regex

class Day1 extends Inputs {

  def part1(input: Seq[String]): Int = {
    input.map(_.filter(_.isDigit))
      .map(s => s.substring(0,1) + s.reverse.substring(0,1))
      .map(_.toInt)
      .sum
  }

  def convertToNumeric(first: String): String = {
    first match {
      case "one" => "1"
      case "two" => "2"
      case "three" => "3"
      case "four" => "4"
      case "five" => "5"
      case "six" => "6"
      case "seven" => "7"
      case "eight" => "8"
      case "nine" => "9"
      case _ => first
    }
  }

  def part2(input: Seq[String]): Int = {
    val pattern = """(one|two|three|four|five|six|seven|eight|nine|[\d]){1}"""
    val firstOcc: Regex = s"^.*?${pattern}.*".r
    val lastOcc: Regex = s"^.*${pattern}.*$$".r
    input.map(s => {
      val first = s match {
        case firstOcc(first) =>
          convertToNumeric(first)
        case _ =>
          "0"
      }
      val second = s match {
        case lastOcc(first) =>
          convertToNumeric(first)
        case _ =>
          "0"
      }
      (first + second).toInt
    }).sum
  }
}
