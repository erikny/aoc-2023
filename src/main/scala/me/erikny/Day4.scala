package me.erikny

class Day4 extends Inputs {
  val mainPattern = """^Card[\s]+(\d+):([\s\d]+) \| ([\s\d]+)$""".r
  val number = """(\d+)""".r
  case class Card(number: Int, winning: Seq[Int], player: Seq[Int]) {
    def intersection = winning.intersect(player)
    def getScore: Int = {
      val intersect: Seq[Int] = intersection
      if (intersect.isEmpty) {
        0
      } else {
        val i: Int = intersect
          .drop(1).foldLeft(1)((l, _) => l * 2)
        i
      }
    }

    def scratchCards: Seq[Int] = {
      if (intersection.isEmpty) {
        Seq.empty
      } else {
        ((number+1) to (number+intersection.length))
      }
    }
  }

  def getNumberSeq(input: String): Seq[Int] = {
    number.findAllMatchIn(input).map(re => re.group(1).trim.toInt).toSeq
  }

  def splitInput(input: Seq[String]): Seq[Card] = {
    input.map(mainPattern.findAllMatchIn)
      .flatMap(it => it.map(re => {
        val winning = re.group(2)
        val player = re.group(3)
        Card(re.group(1).toInt, getNumberSeq(winning), getNumberSeq(player))
      }).toSeq)
  }

  def part1(input: Seq[String]): Int = {
    splitInput(input)
      .map(card => card.getScore)
      .sum
  }

  def part2(input: Seq[String]): Int = {
    splitInput(input).foldLeft(Map.empty[Int, Int].withDefaultValue(0))((acc, curr) => {
      val copies = acc(curr.number) + 1
      curr.scratchCards.foldLeft(acc)((subacc, cardNumber) => {
        subacc.updated(cardNumber, subacc(cardNumber) + copies)
      }).updated(curr.number, acc(curr.number) + 1)
    }).values.sum
  }
}
