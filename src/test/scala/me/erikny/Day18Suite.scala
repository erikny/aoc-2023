package me.erikny

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day18Suite extends AnyFlatSpec with Matchers {
  val day = new Day18

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 62
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 40761
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 952408144115L
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 106920098354636L
  }
}
