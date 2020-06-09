package com.poker

trait Game {

  val players: List[Player]
  val dealer: Dealer
  val deck: ShuffledDeck

  //TODO Getting bidding later
  val bets: List[(Player, Bet)]

}

trait Command
case class AddPlayerToGame( gameId: Int, player: Player) extends Command
case class PlayerBet(amount: Int) extends Command
case object DealCards extends Command
case object Check extends Command


trait Event
case class PlayerAddedToGame(gameId: Int, player: Player) extends Event
case class PlayerBetted(player: Player, amount: Float) extends Event
case object CardsDealt extends Event
case object Checked extends Event

trait State
case object WaitingForPlayers extends State
case class StartedGame(dealer: Dealer, player: List[Player], deck: ShuffledDeck) extends State