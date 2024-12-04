package me.erikny.aoc2023

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day1Suite extends AnyFlatSpec with Matchers {
  val day = new Day1

  it should "part1Test" in {
    day.part1(day.getThisInput("day1test1.txt")) shouldBe 142
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 56506
  }
  it should "part2Test" in {
    day.part2(day.getThisInput("day1test2.txt")) shouldBe 303
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 56017
  }
}
