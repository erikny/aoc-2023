package me.erikny.aoc2023

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day7Suite extends AnyFlatSpec with Matchers {
  val day = new Day7

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 6440
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 251287184
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 5905
  }
  it should "part2" in {
    // 250766783 to high
    // 250759205 to high
    // 250757288 CORRECT
    // 250499744 fel
    // 251002509 fel

    day.part2(day.getInput) shouldBe 250757288
  }
}
