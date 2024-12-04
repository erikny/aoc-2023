package me.erikny.aoc2024

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day2Suite extends AnyFlatSpec with Matchers {
  val day = new Day2

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 2
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 526
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 4
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 566
  }
}
