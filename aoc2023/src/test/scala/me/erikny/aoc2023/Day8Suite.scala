package me.erikny.aoc2023

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day8Suite extends AnyFlatSpec with Matchers {
  val day = new Day8

  it should "part1Test" in {
    day.part1(day.getThisInput("day8test_part1.txt")) shouldBe 6
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 18727
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 6
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 18024643846273L
  }
}
