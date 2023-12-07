package day7

final case class Game(hand: Hand, bid: Int)

object Game {
  def apply(input: String, withJoker: Boolean): Game = {
    val parts = input.split(" ").filter(_.nonEmpty).toList
    val cards = parts(0)
      .map(c => Card(c.toString))
      .toList
    val bid = parts(1).toInt
    if (withJoker)
      Game(Hand.applyWithJoker(cards), bid)
    else
      Game(Hand(cards), bid)
  }
}
