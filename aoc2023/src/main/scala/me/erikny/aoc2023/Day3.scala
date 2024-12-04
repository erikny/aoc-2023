package me.erikny.aoc2023

import scala.util.matching.Regex
import me.erikny.aoc.common.Inputs

class Day3 extends Inputs {
  class Entry(value: Int, start: Int, stop: Int)
  private val characterPattern: Regex = """([^\.\d])""".r
  private val numberPattern: Regex = """(\d+)""".r
  private val star: Regex = """\*""".r
  def getCharacterIndexesOnRow(row: String): Seq[Int] = {
    characterPattern.findAllMatchIn(row)
      .map(regmatch => regmatch.start).toSeq
  }

  def getNumberMatches(row: String): Seq[(Int, Seq[Int])] = {
    numberPattern.findAllMatchIn(row)
      .map(m => {
        val start: Int = Math.max(0, m.start -1)
        val end: Int = m.end
        val value: Int = m.group(1).toInt
        val indexes: Range.Inclusive = (start to end)
        (value, indexes)
      }).toSeq
  }

  def part1(input: Seq[String]): Int = {
    val indexedRow: Seq[(Int, String)] = input
      .zipWithIndex.map(e => (e._2, e._1))
    val charactersOnRow: Map[Int, Seq[Int]] = indexedRow
      .map(e => (e._1, getCharacterIndexesOnRow(e._2)))
      .toMap

    indexedRow.flatMap(row => {
      val rowNumber = row._1
      numberPattern.findAllMatchIn(row._2)
        .flatMap((regexmatch: Regex.Match) => {
          val value: Int = regexmatch.group(1).toInt
          val start: Int = regexmatch.start - 1
          val end: Int = regexmatch.end
          val validIndexes: Seq[Int] = (start to end).toList
          val hasNeighbour: Boolean = validIndexes.exists(index =>
            charactersOnRow.getOrElse(rowNumber - 1, Seq.empty).contains(index) ||
            charactersOnRow.getOrElse(rowNumber, Seq.empty).contains(index) ||
            charactersOnRow.getOrElse(rowNumber + 1, Seq.empty).contains(index)
          )
          if (hasNeighbour)
            Seq(regexmatch.group(1).toInt)
          else
            Seq.empty
        })
    }).sum
  }

  def getValueOnRowCloseToIndex(rowNumber: Int, index: Int, numbersOnRows: Map[Int, Seq[(Int, Seq[Int])]]): Seq[Int] = {
    val rowData: Seq[(Int, Seq[Int])] = numbersOnRows
      .getOrElse(rowNumber, Seq.empty)
    rowData.filter(_._2.contains(index))
      .map(_._1)
  }

  def getAdjacents(rowNumber: Int, index: Int, numbersOnRows: Map[Int, Seq[(Int, Seq[Int])]]): Seq[Int] = {
    val adjacentRow = numbersOnRows
      .getOrElse(rowNumber, Seq.empty)
      .filter(_._2.contains(index))
      .map(e => e._1)
    val previous = getValueOnRowCloseToIndex(rowNumber-1, index, numbersOnRows)
    val next = getValueOnRowCloseToIndex(rowNumber+1, index, numbersOnRows)
    adjacentRow ++ previous ++ next
  }

  def part2(input: Seq[String]): Int = {
    val indexedRow: Seq[(Int, String)] = input
      .zipWithIndex.map(e => (e._2, e._1))

    val numbersOnRows: Map[Int, Seq[(Int, Seq[Int])]] = indexedRow.map(row => (row._1, getNumberMatches(row._2))).toMap
    indexedRow.flatMap(row => {
      val rowNumber: Int = row._1
      val starIndexes: Seq[Int] = star.findAllMatchIn(row._2).map(_.start).toSeq
      starIndexes.map(index => {
        val adjacent = getAdjacents(rowNumber, index, numbersOnRows)
        adjacent match {
          case s: Seq[Int] if s.length == 2 => s.product
          case _ => 0
        }})
    }).sum
  }
}
