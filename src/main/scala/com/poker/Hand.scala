package com.poker
import scala.collection.immutable

case class Hand(setOfCards: Set[Card])

object SortedHand{
  def apply(hand: Hand): SortedHand = {
    SortedHand(hand.setOfCards.toList.sortBy(_.rank.value))
  }
}

case class SortedHand(cards: List[Card]){
  lazy val hasSingleSuit: Boolean = cards.map(_.suit).toSet.size == 1
  lazy val isSequential: Boolean = {
    val initialRankValue = cards.head.rank.value
    cards
      .zipWithIndex
      .takeWhile{ tup =>
        val (card, index) = tup
        card.rank.value == initialRankValue + index
      }
      .size == cards.size
  }
  lazy val pairs: Map[Rank, List[Card]] = cards.groupBy(_.rank).filter(_._2.size > 1)
  lazy val pairCount: List[Int] = pairs.map(_._2.size).toList.sorted
  val highestRankingCard: Card = cards.last
}

class HandRank(val value: Int)

case object RoyalFlush extends HandRank(10){
//  override val value: Int = 10

  def unapply(sortedHand: SortedHand): Option[HandRank] = {
    sortedHand.cards match {
      case Card(_, Ten) :: Card(_, Jack) :: Card(_, Queen) :: Card(_, King) :: Card(_, Ace) :: Nil if sortedHand.hasSingleSuit => Some(RoyalFlush)
      case _ => None
    }
  }
}
object StraightFlush extends HandRank(9){
//  override val value: Int = 9
  def unapply(sortedHand: SortedHand): Option[HandRank] = {
    sortedHand.cards match {
      case Card(_, Two) :: Card(_, Three) :: Card(_, Four) :: Card(_, Five) :: Card(_, Ace) :: Nil if sortedHand.hasSingleSuit => Some(StraightFlush)
      case _ if sortedHand.hasSingleSuit && sortedHand.isSequential => Some(StraightFlush)
      case _ => None
    }
  }
}
object FourOfAKind extends HandRank(8){
//  override val value: Int = 8
  def unapply(sortedHand: SortedHand): Option[HandRank] = {
    sortedHand.pairCount match {
      case 4 :: Nil => Some(FourOfAKind)
      case _ => None
    }
  }
}
case object FullHouse extends HandRank(7){
  def unapply(sortedHand: SortedHand): Option[this.type] = {
    sortedHand.pairCount match {
      case 2 :: 3 :: Nil => Some(FullHouse)
      case _ => None
    }
  }
}
case object Flush extends HandRank(6){
  def unapply(sortedHand: SortedHand): Option[this.type] = {
    if(sortedHand.hasSingleSuit && !sortedHand.isSequential) Some(Flush) else None
  }
}
case object Straight extends HandRank(5){
  def unapply(sortedHand: SortedHand): Option[this.type] = {
    sortedHand.cards match {
      case Card(_, Two) :: Card(_, Three) :: Card(_, Four) :: Card(_, Five) :: Card(_, Ace) :: Nil if !sortedHand.hasSingleSuit=> Some(Straight)
      case _ if sortedHand.isSequential && !sortedHand.hasSingleSuit=> Some(Straight)
      case _ => None
    }
  }
}
case object ThreeOfAKind extends HandRank(4){
  def unapply(sortedHand: SortedHand): Option[this.type] = {
    sortedHand.pairCount match {
      case 3 :: Nil => Some(ThreeOfAKind)
      case _ => None
    }
  }
}
case object TwoPair extends HandRank(3){
  def unapply(sortedHand: SortedHand): Option[this.type] = {
    sortedHand.pairCount match {
      case 2 :: 2 :: Nil => Some(TwoPair)
      case _ => None
    }
  }
}
case object Pair extends HandRank(2){
  def unapply(sortedHand: SortedHand): Option[this.type] = {
    sortedHand.pairCount match {
      case 2 :: Nil => Some(Pair)
      case _ => None
    }
  }
}
case object HighCard extends HandRank(1){
  def unapply(sortedHand: SortedHand): Option[this.type] = {
    if(sortedHand.pairs.isEmpty && !sortedHand.isSequential && !sortedHand.hasSingleSuit)
      Some(HighCard)
    else
      None
  }
}

object HandRank{
  def apply(hand: Hand): HandRank = {
    val sortedHand = SortedHand(hand)
    sortedHand match {
      case RoyalFlush(rf) => rf
      case StraightFlush(sf) => sf

    }

//    sortedHand.cards match {
//      case Card(_, Ten) :: Card(_, Jack) :: Card(_, Queen) :: Card(_, King) :: Card(_, Ace) :: Nil if sortedHand.isSameSuit => RoyalFlush
//      case Card(_, Two) :: Card(_, Three) :: Card(_, Four) :: Card(_, Five) :: Card(_, Ace) :: Nil if isSameSuit => StraightFlush
//      case Card(_, Two) :: Card(_, Three) :: Card(_, Four) :: Card(_, Five) :: Card(_, Ace) :: Nil => Straight
//      case _ if isSequential && isSameSuit => StraightFlush
//      case _ if isSameSuit => Flush
//      case _ if isSequential => Straight
//      case _ =>
//        val pairCount = cards
//          .groupBy(_.rank)
//          .collect{
//            case (_, matchingRanks) if matchingRanks.size > 1 => matchingRanks.size
//          }
//          .toList.sorted
//
//        pairCount match {
//          case 4 :: Nil => FourOfAKind
//          case 2 :: 3 :: Nil => FullHouse
//          case 3 :: Nil => ThreeOfAKind
//          case 2 :: 2 :: Nil => TwoPair
//          case 2 :: Nil => Pair
//          case _ => HighCard
//        }
//    }
  }
}
