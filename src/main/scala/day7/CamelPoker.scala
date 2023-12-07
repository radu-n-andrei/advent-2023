package day7

import scala.io.Source

object CamelPoker {

  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day7.input")
    val input = file.getLines().toList
    val games = input.map(i => Game(i, withJoker = false))
    val jokerGames = input.map(i => Game(i, withJoker = true))
    println(s"SOL1: ${totalBid(games, Hand.ordering)}")
    println(
      s"SOL2: ${totalBid(jokerGames, Hand.gameOrdering(Card.jokerRanking)(_, _))}"
    )

    file.close()
  }

  private def totalBid(games: List[Game], ordering: Ordering[Hand]): Int =
    games
      .sortBy(_.hand)(ordering)
      .map(_.bid)
      .zipWithIndex
      .map { case (bid, rank) =>
        bid * (rank + 1)
      }
      .sum
}
