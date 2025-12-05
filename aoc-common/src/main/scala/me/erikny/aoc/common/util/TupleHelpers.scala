package me.erikny.aoc.common.util

object TupleHelpers {

  val North: (Long, Long) = (-1L, 0L)
  val South: (Long, Long) = (1L, 0L)
  val East: (Long, Long) = (0L, 1L)
  val West: (Long, Long) = (0L, -1L)
  val NorthEast: (Long, Long) = (-1L, -1L)
  val NorthWest: (Long, Long) = (-1L, 1L)
  val SouthEast: (Long, Long) = (1L, -1L)
  val SouthWest: (Long, Long) = (1L, 1L)
  val CompassNavigation: Set[(Long, Long)] = Set(North, West, South, East)

  implicit class LongTupleFeatures(val t: (Long, Long)) extends AnyVal {
    def +(p: (Long, Long)): (Long, Long) = (p._1 + t._1, p._2 + t._2)
    def -(p: (Long, Long)): (Long, Long) = (t._1 - p._1, t._2 - p._2)
    def *(p: (Long, Long)): (Long, Long) = (t._1 * p._1, t._2 * p._2)
    def manhattan(p: (Long, Long)): Long = (p._1 - t._1).abs + (p._2 - t._2).abs
  }


  implicit class LongTupleRangeFeatures(val t: (Long, Long)) extends AnyVal {
    def contains(other: (Long, Long)): Boolean = {
      checkConstraint(other)
      t._1 <= other._1 && other._2 <= t._2
    }

    def contains(value: Long): Boolean = t._1 <= value && value <= t._2
    private def checkConstraint(tuple: (Long, Long)): Unit =
      if(tuple._2 < tuple._1){
        throw new IllegalArgumentException(s"Invalid tuple: $tuple")
      }
    def intersect(other: (Long,Long)): Option[(Long, Long)] = {
      checkConstraint(t)
      checkConstraint(other)
      if (t.contains(other)) {
        Some(other)
      }else if (other.contains(t)) {
          Some(t)
      } else {
        other match {
          case outside if outside._2 < t._1 => None
          case outside if outside._1 > t._2 => None

          /**
           * (5, 15) - (7, 20) => (7,15)
           * (5, 15) - (0, 7) => (5,7)
           *
           */
          case lowerInside if t.contains(lowerInside._1) && !t.contains(lowerInside._2) => Some(lowerInside._1, t._2)
          case higherInside if !t.contains(higherInside._1) && t.contains(higherInside._2) => Some(t._1, higherInside._2)
          case unsupported =>
            throw new IllegalArgumentException(s"Unmatched case: t = $t, other = $unsupported")
        }
      }
    }

    /**
     * Get the values from <this> that does not exist in <other>.
     * @param other other tuple
     * @return new tuple with the values that exist in <this> but not <other>,
     *         if <this> is contained within <other> then None
     */
    def diff(other: (Long, Long)): Option[(Long, Long)] = {
      checkConstraint(t)
      checkConstraint(other)
      if (other.contains(t)) {
        None
      } else {
        (t.contains(other._1), t.contains(other._2)) match {
          case (false, false) => Some(t)
          case (true, false) => Some((t._1, other._1-1))
          case (false, true) => Some((other._2+1, t._2))
        }
      }
    }

    def extendWith(other: (Long, Long)): Option[(Long,Long)] = {
      checkConstraint(t)
      checkConstraint(other)
      /**
       * (5, 10) extendWith (0,5) => (0,10)
       * (5,10) extendWith (10, 15) => (5,15)
       * (5,10) extendWith (11, 15) => (5,15)
       * (5,10) extendWith (8, 15) => (5,15)
       * (5,10) extendWith (0, 4) => (0,10)
       */
      if(t.contains(other))
        None
      else {
        (t.contains(other._1), t.contains(other._2)) match {
          case (true, false) => Some(t._1, other._2)
          case (false, true) => Some(other._1, t._2)
          case (false, false) =>
            other match {
              case (_, upper) if upper == (t._1-1) => Some(other._1, t._2)
              case (lower, _) if lower == (t._2+1) => Some(t._1, other._2)
              case _ => None
            }
          case (true, true) =>
            throw new IllegalArgumentException(s"Unmatched case: t = $t, other = $other")
        }
      }
    }

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