package me.erikny

import java.time.{Duration, LocalDateTime}
import scala.+:
import scala.collection.immutable.NumericRange
import scala.collection.parallel.CollectionConverters.seqIsParallelizable
import scala.collection.parallel.{ForkJoinTaskSupport, ParSeq}

class Day5 extends Inputs {
  case class SeedMap(src: String, dst: String, range: Seq[Range]) {
    def getDestinationValue(value: Long): Long = {
      range.find(r => value >= r.source && value < (r.source + r.length))
        .map(matching => matching.destination + (value - matching.source))
        .getOrElse(value)
    }
  }

  case class Range(destination: Long, source: Long, length: Long)

  def parseSequence(value: Seq[String]): (Array[Long], Seq[SeedMap]) = {
    val seeds = value.head.split(":")(1).split(" ").filter(_.nonEmpty).map(_.trim).map(_.toLong)
    val seedMaps: Seq[SeedMap] = groups[String](value.tail, (p => p.trim.isEmpty)).filter(_.nonEmpty).map(group => {
      val header: Array[String] = group.head.split(" ")(0).split("-")
      val ranges: Seq[Range] = group.tail.map(row => {
        val split: Array[String] = row.split(" ")
        Range(split(0).trim.toLong, split(1).trim.toLong, split(2).trim.toLong)
      })
      SeedMap(header(0), header(2), ranges)
    }
    )
    (seeds, seedMaps)
  }

  def part1(input: Seq[String]): Long = {
    val (seeds, seedMaps) = parseSequence(input)
    findMinLocationForSeeds(seeds, seedMaps)
  }

  def findMinLocationForSeeds(seeds: Seq[Long], seedMaps: Seq[SeedMap]): Long = {
    seeds.foldLeft(Long.MaxValue)((minSoFar, seed) => {
      val locationForSeed = seedMaps.foldLeft(seed)((srcSeed, curr) => {
        curr.getDestinationValue(srcSeed)
      })
      Math.min(locationForSeed, minSoFar)
    })
  }

  def part2(input: Seq[String]): Long = {
    val start: LocalDateTime = LocalDateTime.now()
    val (seeds, seedMaps) = parseSequence(input)
    val allSeeds: Seq[(Long, Long)] = seeds.grouped(2).map {
      case Array(start: Long, length: Long) =>
        (start, (start + (length - 1)))
      case _ => sys.error("uneven size")
    }.toList

    val forkJoinPool = new java.util.concurrent.ForkJoinPool(10)
    val parallelSeeds: ParSeq[(Long, Long)] = allSeeds.par
    parallelSeeds.tasksupport = new ForkJoinTaskSupport(forkJoinPool)

    val value = parallelSeeds.map(seedRanges => {
      val ts = LocalDateTime.now()
      println(s"$ts: Checking range (${seedRanges._1}, ${seedRanges._2})")
      val currentRange = (seedRanges._1 to seedRanges._2)
      println(s"seed range: ${currentRange.length}")
      findMinLocationForSeeds(currentRange, seedMaps)
    }).seq
    val end = LocalDateTime.now()
    val duration: Duration = Duration.between(start, end)
    println(s"Time: ${duration.toMinutes} minutes ${duration.toSecondsPart} seconds")
    value.min
  }


}
