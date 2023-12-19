package me.erikny.util

object Polygon {
  /*
  shoelace formula https://en.wikipedia.org/wiki/Shoelace_formula
   */
  def shoelace(vertices: Seq[(Long, Long)]): Long = {
    val tuple = (vertices.sliding(2) ++ Iterator(Seq(vertices.last, vertices.head)))
      .map(entry => (entry.head._1 * entry.last._2, entry.last._1 * entry.head._2))
      .foldLeft((0L, 0L))((acc, c) => (acc._1 + c._1, acc._2 + c._2))
    (tuple._1 - tuple._2).abs / 2
  }
}
