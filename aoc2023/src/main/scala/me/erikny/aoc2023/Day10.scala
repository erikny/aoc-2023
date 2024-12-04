package me.erikny.aoc2023

import scala.annotation.tailrec
import me.erikny.aoc.common.Inputs
class Day10 extends Inputs {

  case class Grid(tiles: Seq[Tile]) {
    val rows: Long = tiles.map(_.row).max
    val columns: Long = tiles.map(_.column).max

    def getStartTile: (Tile, Seq[Tile]) = {
      val head = tiles.filter(_.sign == 'S').head
      val neighbours = head
        .neighbours
      val t = neighbours
        .flatMap(t => tiles.filter(p => p.row == t._1 && p.column == t._2))
      val tra = t
        .filter(p => {
          val transitions = p.validTransitions
          transitions.exists(p => p._1 == head.row && p._2 == head.column)
        })
      (head, tra)
    }

    def getStartTileSign: Char = {
      val (start, movements) = getStartTile
      movements.toSet match {
        case x if getTiles(start.copy(sign = 'F').validTransitions).toSet.equals(x) => 'F'
        case x if getTiles(start.copy(sign = 'J').validTransitions).toSet.equals(x) => 'J'
        case x if getTiles(start.copy(sign = 'L').validTransitions).toSet.equals(x) => 'L'
        case x if getTiles(start.copy(sign = '7').validTransitions).toSet.equals(x) => '7'
      }
    }

    def getTiles(positions: Seq[(Long, Long)]) = {
      positions.flatMap {
        case (row, column) => tiles.filter(t => t.row == row && t.column == column)
      }
    }

    @tailrec final def walkTrail(steps: Seq[Tile], current: Tile): Seq[Tile] = {
      if (current.sign == 'S') {
        // completed loop
        return steps
      }
      val next = getTiles(current.validTransitions).filterNot(p => p.equals(steps.head)).head
      walkTrail(current +: steps , next)
    }
  }

  case class Tile(row: Long, column: Long, sign: Char) {

    private def north: (Long, Long) = (row - 1, column)
    private def south: (Long, Long) = (row + 1, column)
    private def west: (Long, Long) = (row, column - 1)
    private def east: (Long, Long) = (row, column + 1)

    def neighbours: Seq[(Long, Long)] = Seq(
      (row - 1, column),
      (row + 1, column),
      (row, column - 1),
      (row, column + 1)
    )

    def validTransitions: Seq[(Long, Long)] = {
      sign match {
        case '|' => Seq(north, south)
        case '-' => Seq(west, east)
        case 'L' => Seq(north, east)
        case 'J' => Seq(north, west)
        case '7' => Seq(west, south)
        case 'F' => Seq(south, east)
        case _ => Seq.empty
      }
    }
  }

  object TileReader {
    def apply(lines: Seq[String]): Grid = {
      Grid(lines.zipWithIndex.flatMap {
        case (line, i) =>
          line.zipWithIndex.map({
            case (char, j) => Tile(i, j, char)
          })
      })
    }
  }

  def part1(input: Seq[String]): Int = {
    val grid = TileReader(input)
    val tile = grid.getStartTile
    val steps = grid.walkTrail(Seq(tile._1), tile._2.head)
    steps.size/2
  }

  def part2(input: Seq[String]): Int = {
    val grid = TileReader(input)
    val tile = grid.getStartTile
    val trail = grid
      .walkTrail(Seq(tile._1), tile._2.head)
      .map(p => if(p.sign == 'S') p.copy(sign = grid.getStartTileSign) else p)
    val rayCrossing = grid.tiles
      .filterNot(_.sign == 'S')
      .diff(trail).map(notTrail => {
      if(notTrail.row == 0 || notTrail.row == grid.rows - 1 || notTrail.column == 0 || notTrail.column == grid.columns - 1)
        (notTrail, 0)
      else { // Find if this intersects with trail
        val trailToRight = trail
          .filter(t => t.row == notTrail.row && t.column > notTrail.column).sortBy(_.column).map(_.sign).mkString
        // Replace trails that include corner and sideway transformation but is really just a pipe
        val pipeString = trailToRight
          .replaceAll("[F][-]*?[J]", "|")
          .replaceAll("[L][-]*?[7]", "|")
        val pipes = pipeString.count(p => p == '|')
        val counts = pipes
        (notTrail, counts)
      }
    })

    val inside = rayCrossing.filter(_._2 % 2 != 0)
    inside.size
  }
}
