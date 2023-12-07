package me.erikny

import scala.language.{existentials, implicitConversions}

class Day7 extends Inputs {

  object Card extends Enumeration {
    type Card = Value
    val A, K, Q, J, T, Nine, Eight, Seven, Six, Five, Four, Three, Two = Value
  }

  object HandType extends Enumeration {
    type HandType = Value
    val FullHand, FourOfAKind, FullHouse, ThreeOfAkind, TwoPair, OnePair, HighCard = Value
  }

  trait Hand {
    def cards: Seq[Card.Value]
    def getHandType: HandType.Value
    def bid: Long
  }

  case class VanillaHand(cards: Seq[Card.Value], bid: Long) extends Hand {
    def getHandType = {
      val groups = cards.groupBy(identity).mapValues(_.size).toSeq.sortBy(x => (x._2)).reverse
      groups.size match {
        case 1 => HandType.FullHand
        case 2 =>
          if (groups.exists(_._2 == 2)) HandType.FullHouse else HandType.FourOfAKind
        case 3 =>
          if (groups.exists(_._2 == 3)) HandType.ThreeOfAkind else HandType.TwoPair
        case 4 => HandType.OnePair
        case 5 => HandType.HighCard
      }
    }
  }

  case class JokerHand(hand: VanillaHand) extends Hand {
    def getHandType: HandType.Value = {
      val jokers = hand.cards.count(c => c == Card.J)
      if (jokers == 0)
        return hand.getHandType
      hand.getHandType match {
        case HandType.FullHand => HandType.FullHand
        case HandType.FourOfAKind => HandType.FullHand
        case HandType.FullHouse => HandType.FullHand
        case HandType.ThreeOfAkind => HandType.FourOfAKind
        case HandType.TwoPair =>
          if (jokers == 2) HandType.FourOfAKind else HandType.FullHouse
        case HandType.OnePair => HandType.ThreeOfAkind
        case HandType.HighCard => HandType.OnePair
      }
    }

    override def cards: Seq[Card.Value] = hand.cards
    override def bid: Long = hand.bid
  }

  object Hand {
    def apply(input: String): Hand = {
      val strings = input.split((" ")).filter(_.nonEmpty)
      val bid = strings(1).toLong
      val cards: IndexedSeq[Card.Value] = strings(0).map {
        case 'A' => Card.A
        case 'K' => Card.K
        case 'Q' => Card.Q
        case 'J' => Card.J
        case 'T' => Card.T
        case '9' => Card.Nine
        case '8' => Card.Eight
        case '7' => Card.Seven
        case '6' => Card.Six
        case '5' => Card.Five
        case '4' => Card.Four
        case '3' => Card.Three
        case '2' => Card.Two
      }
      VanillaHand(cards, bid)
    }
  }

  object VanillaHandSort extends Ordering[Hand] {
    def compare(a: Hand, b: Hand): Int = {
      a.cards.zip(b.cards).map {
        case (a, b) => a.compareTo(b)
      }.find(x => x != 0).getOrElse(0)
    }
  }

  object JokerSort extends Ordering[Hand] {
    def compare(a: Hand, b: Hand): Int = {
      a.cards.zip(b.cards).map {
        case (Card.J, Card.J) => 0
        case (Card.J, _) => 1
        case (_, Card.J) => -1
        case (c1, c2) =>
          c1.compareTo(c2)
      }.find(x => x != 0).getOrElse(0)
    }
  }

  def part1(input: Seq[String]): Long = {
    val hands = input.map(Hand(_))
    calculateHands(hands)(VanillaHandSort)
  }

  def part2(input: Seq[String]): Long = {
    val hands = input.map(Hand(_)).map(h => JokerHand(h.asInstanceOf[VanillaHand]))
    calculateHands(hands)(JokerSort)
  }

  def calculateHands(hands: Seq[Hand])(ordering: Ordering[Hand]): Long = {
    hands
      .groupBy(_.getHandType).toSeq.sortBy(x => x._1)
      .map(s => (s._1, s._2.sorted(ordering)))
      .flatMap(s => s._2)
      .reverse
      .zipWithIndex
      .map(t => t._1.bid * (t._2 + 1))
      .sum
  }
}
