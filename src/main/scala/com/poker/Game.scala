package com.poker

trait Game {

  val players: List[Player]
  val dealer: Dealer
  val deck: ShuffledDeck

  //TODO Getting bidding later
  val bets: List[(Player, Bet)]

}
