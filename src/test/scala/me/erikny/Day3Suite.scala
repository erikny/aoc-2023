package me.erikny

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day3Suite extends AnyFlatSpec with Matchers {
  val day = new Day3

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 4361
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 546312
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 467835
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 87449461
  }
}
