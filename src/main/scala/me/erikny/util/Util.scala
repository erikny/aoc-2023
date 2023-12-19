package me.erikny.util

object Util {

  def uniquePairs[A](data: Seq[A]): Seq[(A, A)] = {
    for {
      (x, idxX) <- data.zipWithIndex
      (y, idxY) <- data.zipWithIndex
      if idxX < idxY
    } yield (x, y)
  }

  def allPairs[A](data: Seq[A]): Seq[(A, A)] = {
    for {
      x <- data
      y <- data
    } yield (x, y)
  }
}
