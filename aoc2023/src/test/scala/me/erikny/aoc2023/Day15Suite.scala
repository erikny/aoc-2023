package me.erikny.aoc2023

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day15Suite extends AnyFlatSpec with Matchers {
  val day = new Day15

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 1320
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 514281
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 145
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 244199
  }
}
