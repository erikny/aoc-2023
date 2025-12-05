package me.erikny.aoc2025

import me.erikny.aoc.common.Inputs
import me.erikny.aoc.common.util.TupleHelpers.LongTupleRangeFeatures

class Day5 extends Inputs {

  def part1(input: Seq[String]): Int = {
    val (ranges, freshIds) = input.foldLeft((Seq.empty[(Long, Long)], Seq.empty[(Long)]))((acc, line) => {
      if (line.nonEmpty) {
      val parts = line.split("-")
      if (parts.length == 1) {
        (acc._1, acc._2 :+ parts(0).toLong)
      } else {
        (acc._1 :+ ((parts(0).toLong, parts(1).toLong)), acc._2)
      }}
      else
        acc
    })

    freshIds.count { id =>
      ranges.exists {
        range => range._1 <= id && id <= range._2
      }
    }
  }

  def part2(input: Seq[String]): Long = {
    val ranges = input.takeWhile(_.nonEmpty)
      .map{
        line =>
          val parts = line.split("-")
          (parts(0).toLong, parts(1).toLong)
      }.sortBy(r => (r._2 - r._1)).reverse // sort by largest range first
      ranges.tail.foldLeft(Seq(ranges.head))((acc, range) => {
        if(!acc.exists(_.contains(range))){
          val left = acc.foldLeft(Option(range))((rangeLeft, acceptedRange) => {
            rangeLeft.flatMap(_.diff(acceptedRange))
          })
          acc ++ left
        }
        else acc
      })
        .map(x => x._2-(x._1-1))
        .sum
  }
}