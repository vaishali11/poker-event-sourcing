package com.poker
import com.poker.Deck.Deck

import scala.util


object Deck {

  type Deck = List[Card]

  val SUITS : List[Suit] = Club :: Spade :: Heart :: Diamond :: Nil
  val RANKS : List[Rank] = Ace :: Two :: Three :: Four :: Five :: Six :: Seven :: Eight :: Nine :: Ten :: Jack :: Queen :: King :: Nil

  def apply(): Set[Card] ={
      SUITS.flatMap{ suit =>
      RANKS.map{ rank =>
         Card(suit, rank)
      }
    }.toSet

  }

  def shuffle(): Deck = {
    scala.util.Random.shuffle(Deck().toList)
  }

  def draw(listOfCards: Deck): (Card,Deck) = {
    (listOfCards.head, listOfCards.tail)
  }
}


case class ShuffledDeck(listOfCards: Deck)