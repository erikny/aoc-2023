package me.erikny.aoc2025

import me.erikny.aoc.common.Inputs
import me.erikny.aoc.common.util.TupleHelpers
import me.erikny.aoc.common.util.TupleHelpers.TupleDirection

import scala.annotation.tailrec

class Day4 extends Inputs {

  val directions = TupleHelpers.CompassNavigation ++ Set(
    TupleHelpers.NorthEast,
    TupleHelpers.NorthWest,
    TupleHelpers.SouthEast,
    TupleHelpers.SouthWest,
  )

  def adjacentCount(position: (Long, Long), grid: MapGrid[Char], maxCount: Int): Boolean = {
    directions.toSeq
      .map(x => grid.data.get(position.move(x)))
      .count(x => x.isDefined && x.get == '@') < maxCount
  }

  private def findValid(grid: MapGrid[Char]): Seq[(Long,Long)] = {
    grid.data
      .toIndexedSeq
      .filter(_._2 == '@')
      .filter(x => adjacentCount(x._1, grid, 4))
      .map(_._1)
  }

  def part1(input: Seq[String]): Int = {
    val grid = readGrid(input)
    findValid(grid).size
  }

  @tailrec
  private def work(grid: MapGrid[Char], rolls: Int): Int = {
    findValid(grid) match {
      case Nil => rolls
      case valid =>
        work(MapGrid[Char](grid
          .data
          .toSeq
          .map{
            case (pos, char) if valid.contains(pos) => (pos, '.')
            case x => x
          }.toMap), rolls + valid.size)
    }
  }

  def part2(input: Seq[String]): Int = {
    work(readGrid(input), 0)
  }
}
