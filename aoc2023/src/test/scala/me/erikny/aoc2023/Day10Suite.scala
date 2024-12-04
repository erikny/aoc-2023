package me.erikny.aoc2023

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day10Suite extends AnyFlatSpec with Matchers {
  val day = new Day10

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 4
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 6931
  }
  it should "part2Test-simple" in {
    day.part2(day.getThisInput("day10test-part2-easy.txt")) shouldBe 4
  }
  it should "part2Test-medium" in {
    day.part2(day.getThisInput("day10test-part2-medium.txt")) shouldBe 8
  }
  it should "part2Test-heard" in {
    day.part2(day.getThisInput("day10test-part2-hard.txt")) shouldBe 10
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 357
  }
}
