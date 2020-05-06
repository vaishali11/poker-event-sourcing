package com.poker



case class Hand(setOfCards: Set[Card])


class HandRank(value:Int, fn: Set[Card] => Boolean)


object HandRank {

  def sameSuits(hand: Hand):Boolean = {
    val suits = hand.setOfCards.map(_.suit)
    suits.size == 1
  }

  def isRoyalFlush(hand: Hand): Boolean  = {
    val isSameSuit = sameSuits(hand)
    val sortedCards = hand.setOfCards.toList.sortBy(_.rank)
    sortedCards match {
      case Card(_, Ace) :: Card(_, Ten) ::  Card(_, Jack) ::  Card(_, Queen) ::  Card(_, King) :: Nil => true
      case _ => false
    }
  }
}




case class RoyalFlush() extends HandRank(1, )

