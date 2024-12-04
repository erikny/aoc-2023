package me.erikny.aoc2023

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day19Suite extends AnyFlatSpec with Matchers {
  val day = new Day19

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 19114
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 353046
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 167409079868000L
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 125355665599537L
  }
}
//18sec891