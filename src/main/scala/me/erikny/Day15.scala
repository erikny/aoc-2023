package me.erikny

import scala.collection.immutable.ListMap

class Day15 extends Inputs {

  def hash(value: Long, content: List[Char]): Long = {
    content.foldLeft(value)((acc, char) => {
      ((acc + char.toInt) * 17) % 256
    })
  }

  def part1(input: Seq[String]): Long = {
    input
      .flatMap(row => row.split(","))
      .map(_.toCharArray.toList)
      .map(l => hash(0, l))
      .sum
  }

  def part2(input: Seq[String]): Long = {
    input
      .flatMap(row => row.split(","))
      .map(line => {
        val lineParts = if (line.contains("=")) line.split("=") else line.split("-")
        val label = lineParts(0)
        val bucket = hash(0, label.toCharArray.toList)
        val value = if (lineParts.length > 1) Some(lineParts(1).toInt) else None
        (bucket, label, value)
      }).foldLeft(Map.empty[Long, ListMap[String, Long]].withDefaultValue(ListMap.empty[String, Long]))((acc, t) => {
        val bucket = acc(t._1)
        val updatedBucket: ListMap[String, Long] = t._3 match {
          case None => bucket.removed(t._2)
          case Some(value) => bucket.updated(t._2, value)
        }
        val updatedAcc = acc.updated(t._1, updatedBucket)
        updatedAcc
      }).flatMap {
        case (box, content) =>
          content.toSeq.zipWithIndex.map {
            case ((_, focalLength), index) =>
              (box + 1) * (index + 1) * focalLength
          }
      }.sum
  }
}
