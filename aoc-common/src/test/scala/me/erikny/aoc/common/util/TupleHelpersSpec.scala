package me.erikny.aoc.common.util

import me.erikny.aoc.common.util.TupleHelpers.{East, LongTupleFeatures, North, South, TupleDirection, West}
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks


class TupleHelpersSpec extends AnyFeatureSpec with Matchers with TableDrivenPropertyChecks {

  Feature("LongTupleFeatures") {
    Scenario("it should add two tuples") {
      val a = (1L, 1L)
      val b = (10L, 20L)
      val result = (a + b)
      result shouldBe(11L, 21L)
    }

    Scenario("it should subtract two tuples") {
      val a = (1L, 1L)
      val b = (10L, 20L)
      val result = (b - a)
      result shouldBe(9L, 19L)
    }

    Scenario("it should manhattan distance between two tuples") {
      val a = (1L, 1L)
      val b = (10L, 20L)
      val result = (a manhattan b)
      result shouldBe 9L + 19L
    }
  }

  Feature("Navigation") {
    val startingPoint = (10L, 10L)
    val navigations = {
      Table(
        ("text", "direction", "expectedResult"),
        ("North", North, (9L, 10L)),
        ("South", South, (11L, 10L)),
        ("West", West, (10L, 9L)),
        ("East", East, (10L, 11L)),
      )
    }

    Scenario("Should move in all directions") {
      forAll(navigations) { (text, direction, expected) =>
        startingPoint.move(direction) shouldBe expected
      }
    }
  }
}
