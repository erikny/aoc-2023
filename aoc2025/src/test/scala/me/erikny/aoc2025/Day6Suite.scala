package me.erikny.aoc2025

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day6Suite extends AnyFlatSpec with Matchers {
  val day = new Day6

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 4277556
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 5335495999141L
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 3263827L
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 10142723156431L
  }
}
