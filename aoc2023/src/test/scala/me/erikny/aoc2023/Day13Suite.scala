package me.erikny.aoc2023

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day13Suite extends AnyFlatSpec with Matchers {
  val day = new Day13

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 405
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 37025
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 400
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 32854
  }
}
