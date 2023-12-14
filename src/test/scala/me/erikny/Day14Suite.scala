package me.erikny

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day14Suite extends AnyFlatSpec with Matchers {
  val day = new Day14

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 136
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 110677
  }
  it should "part2Test" in {
    // running in brute force way took 2 hours 31 minutes...
    // look for cycles in real data instead..
    day.part2(day.getTestInput) shouldBe 64
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 90551
  }



  it should "shift signs correct" in {
    val input = ".O.#..O..O".toCharArray.toList
    val expected = "O..#OO....".toCharArray.toList
    day.shiftLine(input, 0) shouldBe(expected)
  }

  it should "transpose correctly" in {
    val input = Vector(
      Vector('1','2','3'),
      Vector('4','5','6'),
      Vector('7','8','9'),
      Vector('a','b','c'),
      Vector('d','e','f')
      )

    val expected = Vector(
      Vector('1', '4', '7', 'a', 'd'),
      Vector('2', '5', '8', 'b', 'e'),
      Vector('3', '6', '9', 'c', 'f')
    )
    val transpose = day.Grid(input).transpose
    transpose shouldBe(day.Grid(expected))
  }
}
