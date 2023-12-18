package me.erikny

import me.erikny.util.Graph
import me.erikny.util.TupleHelpers.{CompassNavigation, East, LongTupleFeatures, North, South, TupleDirection, West}

class Day17 extends Inputs {


  case class Crucible(position: (Long, Long), direction: (Long, Long), consecutiveMoves: Int) {
    def neighbours: Set[Crucible] = {
      CompassNavigation.map {
        case dir if dir == direction => Crucible(position.move(dir), dir, consecutiveMoves + 1)
        case e => Crucible(position.move(e), e, 1)
      }.filterNot(e => e.direction == direction && e.consecutiveMoves > 3 )
        .filterNot(e => e.direction == direction.reverse)
    }
  }

  case class UltraCrucible(position: (Long, Long), direction: (Long, Long), consecutiveMoves: Int) {
    def neighbours: Set[UltraCrucible] = {
      CompassNavigation.map {
        case dir if dir == direction => UltraCrucible(position.move(dir), dir, consecutiveMoves + 1)
        case e => UltraCrucible(position.move(e), e, 1)
      }.filterNot(e => e.direction != direction && consecutiveMoves < 4 )
       .filterNot(e => e.direction == direction && e.consecutiveMoves > 10 )
        .filterNot(e => e.direction == direction.reverse)
    }
  }

  def part1(input: Seq[String]): Long = {
    val grid = readGridNumeric(input)
    val startPositions = Set(Crucible((0L,0L), East, 0), Crucible((0L,0L), South, 0))
    val endPosition = (grid.rows, grid.columns)
    val tuple = Graph.aStar[Crucible](
      startPositions,
      p => p.position == endPosition,
      p => grid(p.position),
      p => p.neighbours.filterNot(p => p.position._1 < 0 || p.position._1 > grid.rows || p.position._2 < 0 || p.position._2 > grid.columns),
      p => p.position manhattan endPosition
    )
    tuple._2
  }

  def part2(input: Seq[String]): Long = {
    val grid = readGridNumeric(input)
    val startPositions = Set(UltraCrucible((0L,0L), East, 0), UltraCrucible((0L,0L), South, 0))
    val endPosition = (grid.rows, grid.columns)
    val tuple = Graph.aStar[UltraCrucible](
      startPositions,
      p => p.position == endPosition,
      p => grid(p.position),
      p => p.neighbours.filterNot(p => p.position._1 < 0 || p.position._1 > grid.rows || p.position._2 < 0 || p.position._2 > grid.columns),
      p => p.position manhattan endPosition
    )
    tuple._2
  }
}
