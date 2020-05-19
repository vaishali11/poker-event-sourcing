package com.poker

case class Player(id: Int, name: String, cards: List[Card])
case class Dealer(player: Player)

//TODO: Implement betting later
trait Bet
case object BigBlind extends Bet
case object SmallBlind extends Bet
