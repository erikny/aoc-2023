package me.erikny.aoc2023

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day11Suite extends AnyFlatSpec with Matchers {
  val day = new Day11

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 374
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 10494813
  }

  it should "part2Test_10" in {
    day.part2(day.getTestInput, 10) shouldBe 1030 // 10 => 1030 // 100 => 8410
  }

  it should "part2Test_100" in {
    day.part2(day.getTestInput, 100) shouldBe 8410 // 10 => 1030 // 100 => 8410
  }

  it should "part2" in {
    day.part2(day.getInput, 1000000) shouldBe 840988812853L
  }
}
