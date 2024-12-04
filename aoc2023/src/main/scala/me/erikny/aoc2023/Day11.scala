package me.erikny.aoc2023

import scala.:+
import scala.collection.immutable.Seq
import me.erikny.aoc.common.Inputs

class Day11 extends Inputs {

  case class SpaceMap(space: Seq[Space]) {
    val rows: Long = space.map(_.col).max
    val columns: Long = space.map(_.col).max

    def expand(size: Long = 2): SpaceMap = {
      val rows = emptyRows
      val columns = emptyColumns
      SpaceMap(space.map(s => {
        val emptyRowsBefore = rows.filter(i => i < s.row).map(_ => 1L).sum
        val emptyColumnsBefore = columns.filter(i => i < s.col).map(_ => 1L).sum
        val a = (emptyRowsBefore * size) - emptyRowsBefore * 1
        val b = (emptyColumnsBefore * size) - emptyColumnsBefore * 1
        s.copy(row = s.row + a, col = s.col + b)
      }))
    }

    private def emptyRows = {
      (0L to rows).foldLeft(Seq[Long]())((acc, index) => {
        val currentRow = space.filter(_.row == index)
        if (currentRow.exists(_.galaxy == true))
          acc
        else
          acc :+ index
      })
    }

    private def emptyColumns = {
      (0L to columns).foldLeft(Seq[Long]())((acc, index) => {
        val currentColumn = space.filter(_.col == index)
        if (currentColumn.exists(_.galaxy == true))
          acc
        else
          acc :+ index
      })
    }

    def getDistance: Long = {
      val allGalaxies = space.filter(_.galaxy == true)
      val uniquePairs = for {
        (x, idxX) <- allGalaxies.zipWithIndex
        (y, idxY) <- allGalaxies.zipWithIndex
        if idxX < idxY
      } yield (x, y)
      uniquePairs.map(t => {
        val distance = Math.abs(t._2.row - t._1.row) + Math.abs(t._2.col - t._1.col)
        (t, distance)
      }).map(_._2).sum
    }
  }

  case class Space(row: Long, col: Long, galaxy: Boolean)

  object SpaceMapInput {
    def apply(lines: Seq[String]): SpaceMap = {
      val d = lines.zipWithIndex.flatMap {
        case (line, row) =>
          line.zipWithIndex.map {
            case (char, col) => char match {
              case '.' => Space(row, col, false)
              case _ => Space(row, col, true)
            }
          }
      }
      SpaceMap(d)
    }
  }

  def part1(input: Seq[String]): Long = {
    SpaceMapInput(input)
      .expand()
      .getDistance
  }

  def part2(input: Seq[String], size: Int): Long = {
    SpaceMapInput(input)
      .expand(size)
      .getDistance
  }
}
