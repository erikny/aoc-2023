package me.erikny.util

import scala.collection.mutable

object Graph {
  def aStar[A](
              start: Set[A],
              stopCondition: A => Boolean,
              cost: A => Long,
              neighbours: A => Set[A],
              heuristic: A => Long
              ): (Map[A, A], Long) = {

    case class QueueState(payload: A, priority: Long, cost: Long)
    val queue = mutable.PriorityQueue[QueueState]()(Ordering.by(v => -(v.priority + v.cost)))
    start.foreach(a => queue.addOne(QueueState(a, 0, 0)))
    val cameFrom = mutable.Map[A, A]()
    while(queue.nonEmpty) {
      val current = queue.dequeue()
      val currentNode = current.payload
      if(stopCondition(currentNode)) {
        // TODO: extract the path
        return (cameFrom.toMap, current.cost)
      }

      val n = neighbours(currentNode)
      n.map(next => {
        val newCost = current.cost + cost(next)
        if(!cameFrom.contains(next)) {
          cameFrom.put(next, currentNode)
          val priority = newCost + heuristic(next)
          queue.addOne(QueueState(next, priority, newCost))
        }
      })
    }
    (Map.empty, 0L)
  }
}
