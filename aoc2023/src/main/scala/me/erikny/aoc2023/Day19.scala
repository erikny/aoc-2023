package me.erikny.aoc2023

import scala.annotation.tailrec
import scala.util.matching.Regex
import me.erikny.aoc.common.Inputs

class Day19 extends Inputs {
  val workflowPattern: Regex = """(?<part>[xmas])(?<sign>[<>])(?<value>[0-9]+):(?<next>[a-zA-Z]+)""".r
  val partsPattern: Regex = """(?<part>[xmas])=(?<value>[0-9]+)""".r

  case class Workflow(part: Char, comparison: Char, value: Int, goto: String)

  //: (Map[String, (Seq[Workflow], String)], Seq[Seq[Part]])
  def readData(input: Seq[String]) = {
    val value = groups[String](input, (s => s.equals("")))

    val workflows = value.head.map(line => {
      val name = line.takeWhile(_ != '{')
      val default = line.reverse.drop(1).takeWhile(_ != ',').reverse
      val workflows = workflowPattern.findAllMatchIn(line)
        .map(m => {
          val part = m.group("part").charAt(0)
          val sign = m.group("sign").charAt(0)
          val value = m.group("value").toInt
          val next = m.group("next")
          Workflow(part, sign, value, next)
        }).toSeq
      (name, (workflows, default))
    }).toMap

    val parts = value.last.map(line => {
      partsPattern.findAllMatchIn(line).map(m => {
        (m.group("part").charAt(0), m.group("value").toInt)
      }).toMap
    })

    (workflows, parts)
  }

  @tailrec private def processPart(part: Map[Char, Int], workflows: Map[String, (Seq[Workflow], String)], workflowName: String = "in"): Long = {
    if (workflowName.equals("A"))
      part.values.sum
    else if (workflowName.equals("R"))
      0
    else {
      val (flows, default) = workflows(workflowName)
      val nextWorkflow = flows.map(f => {
          f.comparison match {
            case '<' => (part(f.part) < f.value, f.goto)
            case '>' => (part(f.part) > f.value, f.goto)
          }
        })
        .find(_._1)
        .map(_._2)
        .getOrElse(default)
      processPart(part, workflows, nextWorkflow)
    }
  }

  def part1(input: Seq[String]): Long = {
    val (workflows, parts) = readData(input)
    val partsResult = parts.map(processPart(_, workflows))
    partsResult.sum
  }

  private type PartRange = Map[Char, (Int, Int)]

  private def findOptimalRanges(range: PartRange, workflows: Map[String, (Seq[Workflow], String)], workflowName: String = "in"): Seq[PartRange] = {
    if (workflowName.equals("A")) {
      return Seq(range)
    } else if(workflowName.equals("R"))
      return Seq.empty
    val (currentWorkflows, default) = workflows(workflowName)
    val (rangeForDefault, foundRanges) = currentWorkflows.foldLeft((range, Seq.empty[PartRange]))((acc, flow) => {
      val currentRange = acc._1
      val foundRanges = acc._2
      val rangeToSplit = currentRange(flow.part)
      val (modifiedCurrent, recursiveArgs) = flow.comparison match {
        case '<' =>
          val lowerRange = (rangeToSplit._1, flow.value - 1)
          val upperRange = (flow.value, rangeToSplit._2)
          (currentRange.updated(flow.part, upperRange), (currentRange.updated(flow.part, lowerRange), flow.goto))
        case '>' =>
          val lowerRange = (rangeToSplit._1, flow.value)
          val upperRange = (flow.value + 1, rangeToSplit._2)
          (currentRange.updated(flow.part, lowerRange), (currentRange.updated(flow.part, upperRange), flow.goto))
      }
      (modifiedCurrent, foundRanges ++ findOptimalRanges(recursiveArgs._1, workflows, recursiveArgs._2))
    })
    foundRanges ++ findOptimalRanges(rangeForDefault, workflows, default)
  }

  def part2(input: Seq[String]): Long = {
    val (workflows, _) = readData(input)
    val range = (1, 4000)
    val initialRange: PartRange = Map('x' -> range, 'm' -> range, 'a' -> range, 's' -> range)
    findOptimalRanges(initialRange, workflows, "in")
      .map(range => {
      (range, range.values.map(t => ((t._2 - t._1) + 1).toLong).product)
    }).map(_._2).sum
  }

}
