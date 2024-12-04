package me.erikny.aoc.common.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class PolygonSpec extends AnyFlatSpec with Matchers {

  it should "calculate correct polygon area" in {
    val vertices = Seq(
      (2L, 7L),
      (10L, 1L),
      (8L, 6L),
      (11L, 7L),
      (7L, 10L),
    )
    val area = Polygon.shoelace(vertices)
    area shouldBe 32
  }
}
