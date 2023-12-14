package me.erikny

import scala.annotation.tailrec
import scala.collection.mutable


class Day14 extends Inputs {
  object Direction extends Enumeration {
    type Location = Value
    val North, South, East, West = Value
  }


  def shiftLine(line: List[Char], freeSpace: Int): List[Char] = {
    line match {
      case 'O' :: rest => 'O' +: shiftLine(rest, freeSpace)
      case '#' :: rest => List.fill(freeSpace)('.') ++ Seq('#') ++ shiftLine(rest, 0)
      case '.' :: rest => shiftLine(rest, freeSpace + 1)
      case Nil => List.fill(freeSpace)('.')
      case _ => throw new RuntimeException(s"unrecognized cell content: $line")
    }
  }

  def tiltGrid(grid: Grid): Grid = {
      Grid(grid.content.map(line => shiftLine(line.toList, 0).toVector))
  }

  def spin(grid: Grid): Grid = {
    Seq(Direction.North, Direction.West, Direction.South, Direction.East).foldLeft(grid)((tmp, direction) => {
      tilt(tmp, direction)
    })
  }

  def tilt(grid: Grid, direction: Direction.Value): Grid = {
      direction match {
        case Direction.North =>
          tiltGrid(grid.transpose).transpose
        case Direction.South =>
          tiltGrid(grid.reverseRow.transpose).transpose.reverseRow
        case Direction.West =>
          tiltGrid(grid)
        case Direction.East =>
          tiltGrid(grid.reverseColumn).reverseColumn
      }
  }

  case class Grid(content: Vector[Vector[Char]]) {

    def reverseRow : Grid = {
      Grid(content.reverse)
    }

    def reverseColumn: Grid = {
      Grid(content.map(_.reverse))
    }

    def transpose: Grid = {
      Grid(content.transpose)
    }

    def print(): Unit = {
      content.foreach(line => println(s"${line.mkString}"))
    }

    def score: Long = {
      content
        .map(line => line.count(c => c == 'O'))
        .reverse
        .zipWithIndex
        .map {
          case (count, multiplier) => count * (multiplier + 1)
        }.sum
    }
  }

  object ReadGrid {
    def apply(lines: Seq[String]): Grid = {
      val value = lines.map(line => line.toCharArray.toVector).toVector
      Grid(value)
    }
  }

  def part1(input: Seq[String]): Long = {
    val grid = ReadGrid(input)
    val grid1 = tilt(grid, Direction.North)
    grid1.print
    tilt(grid, Direction.North).score
  }

  private def findCycleScore(grid: Grid): Long = {
    val gridIterations = mutable.HashMap[Grid, Long]()
    val spinScore = mutable.HashMap[Long, Long]()
    val maxSpins = 1000000000
    @tailrec  def  makeIteration(grid: Grid, currentSpin: Long): Long = {
      if(currentSpin == maxSpins)
        return grid.score
      val spunned = spin(grid)
      gridIterations.get(spunned) match {
        case None =>
          gridIterations.put(spunned, currentSpin)
          spinScore.put(currentSpin, spunned.score)
        case Some(count) =>
          // We've seen this before. Pattern starts at <previous> then repeats X times then run a few iteration more before
          // it would have hit the maxSpin threshold.
          val previous = gridIterations(spunned)
          val cycleLength = currentSpin - previous
          val repeatedPatterns = (maxSpins - previous) / cycleLength
          val additionalCycles = (maxSpins - previous) - (repeatedPatterns*cycleLength)
          val correctSpin = previous + additionalCycles
          println(s"Found cycle on iteration ${cycleLength}, correct grid is created after $correctSpin spins")
          return spinScore(correctSpin)
      }
      makeIteration(spunned, currentSpin + 1)
    }
    makeIteration(grid, 1)
  }

  def part2(input: Seq[String]): Long = {
    val grid = ReadGrid(input)
    findCycleScore(grid)
  }
}
