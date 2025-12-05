package me.erikny.aoc.common.util

import me.erikny.aoc.common.util.TupleHelpers.{East, LongTupleFeatures, LongTupleRangeFeatures, North, South, TupleDirection, West}
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

  Feature("Range tuple helpers") {
    Scenario("Contain sub range") {
      val range = (0L, 10L)
      val subRange = (3L,6L)
      range.contains(subRange) shouldBe true
    }

    Scenario("Contain full") {
      val range = (0L, 10L)
      range.contains(range) shouldBe true
    }

    Scenario("Not contain") {
      val range = (0L, 10L)
      val outside = (9L, 11L)
      range.contains(outside) shouldBe false
    }

    Scenario("Intersect full range") {
      val range = (0L, 10L)
      val other = (7L, 10L)
      range.intersect(other) shouldBe Some(other)
    }

    Scenario("Intersect full range other way") {
      val range = (0L, 10L)
      val other = (7L, 10L)
      other.intersect(range) shouldBe Some(other)
    }

    Scenario("Intersect outside higher") {
      val range = (0L, 10L)
      val other = (20L, 30L)
      range.intersect(other) shouldBe None
    }

    Scenario("Intersect outside lower") {
      val range = (0L, 10L)
      val other = (-10L, -5L)
      range.intersect(other) shouldBe None
    }

    Scenario("Intersect outside lower reversed") {
      val range = (0L, 10L)
      val other = (-10L, -5L)
      other.intersect(range) shouldBe None
    }

    Scenario("Intersect lower end") {
      val range = (0L, 10L)
      val other = (5L, 30L)
      range.intersect(other) shouldBe Some(5L, 10L)
    }

    Scenario("Intersect upper end") {
      val range = (0L, 10L)
      val other = (8L, 100L)
      range.intersect(other) shouldBe Some(8L, 10L)
    }

    Scenario("Diff same objects is empty") {
      val range = (0L, 10L)
      range.diff(range) shouldBe None
    }

    Scenario("Diff subrange") {
      val range = (90L, 100L)
      val other = (0L, 100L)
      range.diff(other) shouldBe None
    }

    Scenario("Diff lower end") {
      val range = (5L, 15L)
      val other = (10L, 20L)
      range.diff(other) shouldBe Some(5, 9)
    }

    Scenario("Diff upper end") {
      val range = (5L, 15L)
      val other = (0L, 7L)
      range.diff(other) shouldBe Some(8L, 15L)
    }

    Scenario("Diff outside") {
      val range = (5L, 15L)
      val other = (20L, 30L)
      range.diff(other) shouldBe Some(5L, 15L)
    }

    Scenario("extend with itself") {
      val range = (5L, 15L)
      range.extendWith(range) shouldBe None
    }

    Scenario("extend upper range overlapping element") {
      val range = (5L, 10L)
      val other = (8L, 15L)
      range.extendWith(other) shouldBe Some(5,15)
    }

    Scenario("extend upper range just outside") {
      val range = (5L, 10L)
      val other = (11L, 15L)
      range.extendWith(other) shouldBe Some(5,15)
    }

    Scenario("extend lower range overlapping") {
      val range = (5L, 10L)
      val other = (0L, 6L)
      range.extendWith(other) shouldBe Some(0,10)
    }

    Scenario("extend lower range just outside edge") {
      val range = (5L, 10L)
      val other = (0L, 4L)
      range.extendWith(other) shouldBe Some(0,10)
    }

    Scenario("extend lower range same edge value") {
      val range = (5L, 10L)
      val other = (0L, 5L)
      range.extendWith(other) shouldBe Some(0,10)
    }

    Scenario("extend with gap returns none") {
      val range = (5L, 10L)
      val other = (0L, 3L)
      range.extendWith(other) shouldBe None
    }
  }
}
