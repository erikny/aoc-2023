package me.erikny.aoc2023

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day4Suite extends AnyFlatSpec with Matchers {
  val day = new Day4

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 13
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 17803
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 30
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 5554894
  }
}
