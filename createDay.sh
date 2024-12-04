cat << EOF > aoc2024/src/main/scala/me/erikny/aoc2024/Day$1.scala
package me.erikny.aoc2024

import me.erikny.aoc.common.Inputs

class Day$1 extends Inputs {

  def part1(input: Seq[String]): Int = {
    -1
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
EOF
mkdir -p src/test/resources
cat << EOF > aoc2024/src/test/scala/me/erikny/aoc2024/Day$1Suite.scala
package me.erikny.aoc2024

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day$1Suite extends AnyFlatSpec with Matchers {
  val day = new Day$1

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe -1
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe -1
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe -1
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe -1
  }
}
EOF

curl "https://adventofcode.com/2024/day/$1/input" -H "cookie: session=$AOC_SESSION_ID" -o aoc2024/src/test/resources/day$1.txt
touch aoc2024/src/test/resources/day$1test.txt
