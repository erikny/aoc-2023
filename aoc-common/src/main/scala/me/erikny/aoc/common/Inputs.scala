package me.erikny.aoc.common

import scala.io.Source

trait Inputs {
  val fileName = s"${this.getClass.getTypeName.toLowerCase.replace("$", "").split("\\.").last}.txt"
  val testFileName = s"${this.getClass.getTypeName.toLowerCase.replace("$", "").split("\\.").last}test.txt"


  def getThisInput(fileName: String) = read(fileName)
  def getInput: Seq[String] = read(fileName)
  def getRaw: String = raw(fileName)
  def getTestInput: Seq[String] = read(testFileName)
  def getTestRaw: String = raw(testFileName)

  def getNonSeparatedIntegers: Seq[Int] = getInput.head.split("\\B").map(_.toInt).toSeq

  def raw(path: String): String = {
    val source = Source.fromResource(path)
    try {
      source.mkString
    } finally {
      source.close()
    }
  }

  def read(path: String): Seq[String] = {
    val source = Source.fromResource(path)
    try {
      source.getLines().toSeq
    } finally {
      source.close()
    }
  }

  def toIntArray(input: String, separator: Char = ' '): Array[Int] = {
    splitLine(input, separator)
      .filterNot(_.isEmpty)
      .map(_.toInt)
  }

  def toLongArray(input: String, separator: Char = ' '): Array[Long] = {
      splitLine(input, separator)
        .filterNot(_.isEmpty)
        .map(_.toLong)
  }

  def splitLine(input: String, separator: Char = ' ', trim: Boolean = true): Array[String] = {
    input.trim
      .split(separator)
      .map(_.trim)
  }

  def groups[B](strings: Seq[B], predicate: B=>Boolean): Seq[Seq[B]] = {
    val reduction = strings.foldLeft((Seq():Seq[Seq[B]], Seq[B]())) {
      case ((reduction, remainder), item) =>
        if (predicate.apply(item))
          (reduction :+ remainder, Seq())
        else
          (reduction, remainder :+ item)
    }
    reduction._1 :+ reduction._2
  }

  case class MapGrid[A](data: Map[(Long, Long), A]){
    val rows: Long = data.keys.map(_._1).max
    val columns: Long = data.keys.map(_._2).max
    def apply(position: (Long, Long)): A = data(position)
  }

  def readGrid(lines: Seq[String]): MapGrid[Char] = {
    MapGrid(lines.zipWithIndex.flatMap {
      case (line, row) => line.zipWithIndex.map {
        case (char, column) => ((row.toLong, column.toLong), char)
      }
    }.toMap)
  }
  def readGridNumeric(lines: Seq[String]): MapGrid[Int] = {
    MapGrid(readGrid(lines).data.map(t => (t._1, t._2.asDigit)))
  }

}