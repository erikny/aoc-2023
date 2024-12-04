package me.erikny.aoc.common.util

object TupleHelpers {

  val North: (Long, Long) = (-1L, 0L)
  val South: (Long, Long) = (1L, 0L)
  val East: (Long, Long) = (0L, 1L)
  val West: (Long, Long) = (0L, -1L)
  val CompassNavigation: Set[(Long, Long)] = Set(North, West, South, East)

  implicit class LongTupleFeatures(val t: (Long, Long)) extends AnyVal {
    def +(p: (Long, Long)): (Long, Long) = (p._1 + t._1, p._2 + t._2)
    def -(p: (Long, Long)): (Long, Long) = (t._1 - p._1, t._2 - p._2)
    def *(p: (Long, Long)): (Long, Long) = (t._1 * p._1, t._2 * p._2)
    def manhattan(p: (Long, Long)): Long = (p._1 - t._1).abs + (p._2 - t._2).abs
  }

  implicit class TupleDirection(val t: (Long, Long)) extends AnyVal {
    def move(direction: (Long, Long)): (Long, Long) = t + direction
    def moveDistance(direction: (Long, Long), distance: Long): (Long, Long) =
      t + (direction * (distance, distance))
    def makeTrail(direction: (Long, Long), steps: Long) = {
      val trail = Seq(t)
      (0L to steps).foldLeft(trail)((acc, step) => acc.head.move(direction) +: acc)
    }

    def reverse: (Long, Long) = (t._1 * -1, t._2 * -1)
  }
  
}