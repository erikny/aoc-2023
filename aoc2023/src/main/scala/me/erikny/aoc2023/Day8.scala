package me.erikny.aoc2023

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.seqIsParallelizable
import me.erikny.aoc.common.Inputs

class Day8 extends Inputs {
  case class Node(id: String, left: String, right: String)
  def getNodes(input: Seq[String]): Map[String, Node] = {
    input.tail
      .filter(_.trim.nonEmpty)
      .map(line => {
        val split = line.replace("(", "")
          .replace(")", "")
          .split("=")
          .filter(_.nonEmpty)
        val nodeId = split(0).trim
        val steps = split(1).split(",").map(_.trim)
        (nodeId -> Node(nodeId, steps(0), steps(1)))
      }).toMap
  }

  @tailrec private final def findZ(i: Int, instructions: String, node: Node, nodes :Map[String, Node]): Long = {
    if(node.id.endsWith("Z"))
      return i
    val instruction = instructions.charAt(i % instructions.length)
    val nextStep = instruction match {
      case 'L' => node.left
      case 'R' => node.right
      case _ => throw new RuntimeException("error")
    }
    findZ(i + 1, instructions, nodes(nextStep), nodes)
  }

  def part1(input: Seq[String]): Long = {
    val instructions = input.head
    val nodes = getNodes(input.tail)
    findZ(0, instructions, nodes("AAA"), nodes)
  }

  def part2(input: Seq[String]): Long = {
    val instructions = input.head
    val nodes: Map[String, Node] = getNodes(input.tail)
    val steps = nodes
      .values
      .filter(_.id.endsWith("A"))
      .toSeq
      .map(n => findZ(0, instructions, n, nodes))

    steps.tail.foldLeft(steps.head)((acc, curr) => {
      lcm(acc, curr)
    })
  }

  @tailrec
  final def gcd(a: Long, b: Long): Long = b match {
    case 0 => a
    case _ => gcd(b, a % b)
  }

  def lcm(a: Long, b: Long): Long = a * (b / gcd(a, b))
}


