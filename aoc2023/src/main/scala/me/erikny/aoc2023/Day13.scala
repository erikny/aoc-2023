package me.erikny.aoc2023

import me.erikny.aoc.common.Inputs
class Day13 extends Inputs {

  case class Grid(row: Long, col: Long, sign: Char) {
    def invert: Char = {
      sign match {
        case '.' => '#'
        case '#' => '.'
      }
    }
  }

  object Axis extends Enumeration {
    type Location = Value
    val Row, Column = Value
  }

  case class PerfectReflection(axis: Axis.Value, index: Long) {
    def getScore: Long = {
      axis match {
        case Axis.Row => index * 100
        case Axis.Column => index
      }
    }
  }

  case class Pattern(grid: Seq[Grid]) {
    val rows = grid.map(_.row).max
    val columns = grid.map(_.col).max

    private def groupByAxis(groupBy: Grid => Long) = {
      grid.groupBy(groupBy).map(d => {
          (d._1, d._2.map(_.sign).mkString)
        }).toSeq
        .groupBy(_._2)
        .map(t => (t._1, t._2.map(_._1)))
    }

    def uniquePairs(data: Seq[Long]): Seq[(Long, Long)] = {
      for {
        (x, idxX) <- data.zipWithIndex
        (y, idxY) <- data.zipWithIndex
        if idxX < idxY
      } yield (x, y)
    }

    def findPerfectionReflection2: Seq[PerfectReflection] = {
      Seq(Axis.Row, Axis.Column).flatMap(axis => {
        findPerfectPairOnAxis(axis)
          .map(p => {
            PerfectReflection(axis, index = Math.min(p._1, p._2) + 1)
          })
      })
    }

    def findPerfectPairOnAxis(axis: Axis.Value): Seq[(Long, Long)] = {
      val groups = if (axis == Axis.Row) groupByAxis(_.row) else groupByAxis(_.col)
      val closePairs = groups
        .values
        .filter(_.size > 1)
        .flatMap(uniquePairs)
        .map(p => (p, Math.abs(p._2 - p._1)))
        .toSeq.filter(p => p._2 == 1)
      val closePairsArePerfect = closePairs.map {
        case ((minColumn, maxColumn), distance) => {
          val patternWidth = groups
            .values
            .flatten
            .max
          val minDistanceToEdge = Math.min(minColumn, patternWidth - maxColumn)
          val value = (0L to minDistanceToEdge).foldLeft(Seq.empty[Boolean])((acc, index) => {
            val min = minColumn - index
            val max = maxColumn + index
            val bool = groups.values.exists(p => p.contains(min) && p.contains(max))
            acc :+ bool
          })
          ((minColumn, maxColumn), value.reduce((a, b) => a && b))
        }
      }
      closePairsArePerfect.filter(p => p._2).map(t => t._1)
    }

    def part1: PerfectReflection = {
      findPerfectionReflection2.head
    }

    def part2 = {
      val originalScore = part1
      (0L to rows).flatMap(i => {
        (0L to columns).map(j => {
          val patched = grid.map(gridEntry => {
            if (gridEntry.row == i && gridEntry.col == j) gridEntry.copy(sign = gridEntry.invert) else gridEntry
          })
          Pattern(patched).findPerfectionReflection2.filterNot(p => p.equals(originalScore))
        }).filter(_.nonEmpty)
      }).filter(_.nonEmpty).flatten.distinct.head
    }
  }

  object PatternInput {
    def apply(input: Seq[String]): Seq[Pattern] = {
      groups[String](input, b => b.equals("")).map(group => {
        val gridContent = group.zipWithIndex.flatMap {
          case (line, row) =>
            line.zipWithIndex.map {
              case (char, column) => Grid(row, column, char)
            }
        }
        Pattern(gridContent)
      })
    }
  }


  def part1(input: Seq[String]): Long = {
    PatternInput(input)
      .map(_.part1)
      .map(_.getScore)
      .sum
  }

  def part2(input: Seq[String]): Long = {
    PatternInput(input)
      .map(_.part2)
      .map(_.getScore)
      .sum
  }
}
