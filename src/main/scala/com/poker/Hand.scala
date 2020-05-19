package com.poker

case class Hand(setOfCards: Set[Card])

object SortedHand{
  def apply(hand: Hand): SortedHand = {
    SortedHand(hand.setOfCards.toList.sortBy(_.rank.value))
  }
}
case class SortedHand(cards: List[Card])

object SequentialCards{
  def apply(sortedHand: SortedHand): Seq[(Card, Int)] = {
    val cards = sortedHand.cards
    val initialRankValue = cards.head.rank.value
    cards.zipWithIndex.takeWhile{ tup =>
      val (card, index) = tup
      card.rank.value == initialRankValue + index
    }
  }
}

class HandRank(value: Int)
case object RoyalFlush extends HandRank(10)
case object StraightFlush extends HandRank(9)
case object FourOfAKind extends HandRank(8)
case object FullHouse extends HandRank(7)
case object Flush extends HandRank(6)
case object Straight extends HandRank(5)
case object ThreeOfAKind extends HandRank(4)
case object TwoPair extends HandRank(3)
case object Pair extends HandRank(2)
case object HighCard extends HandRank(1)

object HandRank{
  def apply(hand: Hand): HandRank = {

    val sortedHand = SortedHand(hand)
    val isSameSuit = sortedHand.cards.map(_.suit).toSet.size == 1
    val isSequential = SequentialCards(sortedHand).size == sortedHand.cards.size

    sortedHand.cards match {
      case Card(_, Ten) :: Card(_, Jack) :: Card(_, Queen) :: Card(_, King) :: Card(_, Ace) :: Nil if isSameSuit => RoyalFlush
      case Card(_, Two) :: Card(_, Three) :: Card(_, Four) :: Card(_, Five) :: Card(_, Ace) :: Nil if isSameSuit => StraightFlush
      case Card(_, Two) :: Card(_, Three) :: Card(_, Four) :: Card(_, Five) :: Card(_, Ace) :: Nil => Straight
      case _ if isSequential && isSameSuit => StraightFlush
      case _ if isSameSuit => Flush
      case _ if isSequential => Straight
      case _ =>
        val pairs = sortedHand.cards.groupBy(_.rank).filter(_._2.size > 1)
        val pairCount = pairs.map(_._2.size).toList.sorted
        pairCount match {
          case 4 :: Nil => FourOfAKind
          case 2 :: 3 :: Nil => FullHouse
          case 3 :: Nil => ThreeOfAKind
          case 2 :: 2 :: Nil => TwoPair
          case 2 :: Nil => Pair
          case _ => HighCard
        }
    }
  }
}
