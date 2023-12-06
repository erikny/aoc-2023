package me.erikny

import scala.collection.SetOps
import scala.collection.parallel.ParSetLike
import scala.concurrent.impl.{FutureConvertersImpl, Promise}
import scala.jdk.FunctionWrappers
import scala.runtime.{AbstractFunction1, AbstractPartialFunction}

class Day6 extends Inputs {

  case class Race(time: Long, distance: Long) {
    def getStrategyCount: Long = {
      (0L to time).count(i => {
        (i * (time - i) > distance)
      })
    }
  }

  def readInput(input: Seq[String]) = {
    val times = toLongArray(input.head.split(":")(1))
    val distances = toLongArray(input.tail.head.split(":")(1))
    times.indices.map(i => Race(times(i), distances(i)))
  }

  def part1(input: Seq[String]): Long = {
   readInput(input).map(_.getStrategyCount).product
  }

  def part2(input: Seq[String]): Long = {
    val tuple = readInput(input).foldLeft(("", ""))((acc, race) => {
      (acc._1 + race.time, acc._2 + race.distance)
    })
    val longRace = Race(tuple._1.toLong, tuple._2.toLong)
    longRace.getStrategyCount
  }
}
