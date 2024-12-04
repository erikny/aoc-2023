package me.erikny.aoc2023

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.seqIsParallelizable
import me.erikny.aoc.common.Inputs

class Day12 extends Inputs {

  def smartReplacement(input: List[Char], numbers: List[Int]): Long = {
    val cache: mutable.Map[(List[Char], List[Int]), Long] = mutable.HashMap[(List[Char], List[Int]), Long]()

    def process(line: List[Char], groups: List[Int]): Long = {
      cache.getOrElseUpdate((line, groups), {
        (line, groups) match {
          /*
          Possible combinations:
          no more groups =>
             0 point if line contains # (we have more hashtags in input than expected)
             1 otherwise
          no more data:
             0 points if we have groups left (we haven't found everything)
             1 otherwise
          length of input is less than the sum of groups
             0 points
          next char = ?
             split => (process rest, replace ? with # and process *this*)
          next char = #
             if rest of the string is less than groups(0) => not enough characters to match the expected number => 0 points
             if we have more than groups(0) # in a row => 0 points as it does not match the pattern
             if we have groups(0) #:
                 if next char is a ? => we must deduce this to a point,
                   hence we can ignore this character and process the rest of the string, without the ?, with the next expected groups
                 else => process the rest of the string with the next expected groups
           */
          case (Nil, groups) if groups.nonEmpty => return 0 // No more characters but we haven't found all groups yet
          case (chars, Nil) if chars.contains('#') => return 0 // At least one # left but we have found all groups, so input does not match
          case (_, Nil) => return 1 // No more groups
          //          case (x, b) if x.length < b.sum => return 0 // less characters left than expected blocks in 'groups'
          case ('.' :: rest, groups) => process(rest, groups)
          case ('?' :: rest, groups) => process(rest, groups) + process('#' :: rest, groups)
          case ('#' :: rest, thisGroup :: _) if rest.length < thisGroup - 1 => return 0
          case ('#' :: rest, thisGroup :: nextGroups) =>
            // We should have enough for the first group
            val charsForThisGroup = line.take(thisGroup)
            if (charsForThisGroup.contains('.')) return 0 // We do not have <thisGroup> # signs.
            val next = line.drop(thisGroup)
            next match {
              case Nil => process(next, nextGroups)
              case '#' :: _ => 0 // If the first in the next part is a # we have more # than expected
              case '?' :: e => process(e, nextGroups) // First char in next group must be a . so we can just drop it here
              case _ => process(next, nextGroups)
            }
          case _ => throw new RuntimeException(s"Match failed for line $line and groups $groups")
        }
      }
      )
    }

    process(input, numbers)
  }

  case class Springs(line: String, groups: Seq[Int])

  object DRead {
    def apply(line: String): Springs = {
      val strings = line.split(" ").filter(_.nonEmpty)
      val numbers = toIntArray(strings(1), separator = ',').toSeq
      Springs(strings(0), numbers)
    }
  }

  def part1(input: Seq[String]): Long = {
    input.map(DRead(_))
      .map(d => smartReplacement(d.line.toCharArray.toList, d.groups.toList))
      .sum
  }

  def part2(input: Seq[String]): Long = {
    val springs = input.map(DRead(_)).par.map(spring => {
      (0 to 3).foldLeft(spring)((acc, _) => {
        Springs(acc.line + "?" + spring.line, acc.groups ++ spring.groups)
      })
    })
    springs.map(d => smartReplacement(d.line.toCharArray.toList, d.groups.toList))
      .sum
  }
}
