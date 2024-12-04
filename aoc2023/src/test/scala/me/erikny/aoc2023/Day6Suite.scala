package me.erikny.aoc2023

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day6Suite extends AnyFlatSpec with Matchers {
  val day = new Day6

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 4 * 8 * 9
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 3317888
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 71503
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 24655068
  }
}
