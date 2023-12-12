package me.erikny

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day12Suite extends AnyFlatSpec with Matchers {
  val day = new Day12

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 21
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 7204
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 525152
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 1672318386674L
  }
}
