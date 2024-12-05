package me.erikny.aoc2024

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day3Suite extends AnyFlatSpec with Matchers {
  val day = new Day3

  it should "match regex" in {
    val pattern = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r
    val input = "mul(12,3)"
    val matches = pattern.findAllMatchIn(input).toSeq
    matches.isEmpty shouldBe false
    matches.foreach(m => {
      println(m.group(1))
    })
  }

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe (2*4 + 5*5 + 11*8 + 8*5)
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 153469856
  }
  it should "part2Test" in {
    day.part2(day.getThisInput("day3test2.txt")) shouldBe (2*4 + 8*5)
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 77055967
  }
}
