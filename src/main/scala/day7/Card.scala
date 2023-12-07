package day7

final case class Card(cardType: String)

object Card {
  val cardRanking: Map[String, Int] = Map(
    "2" -> 1,
    "3" -> 2,
    "4" -> 3,
    "5" -> 4,
    "6" -> 5,
    "7" -> 6,
    "8" -> 7,
    "9" -> 8,
    "T" -> 9,
    "J" -> 10,
    "Q" -> 11,
    "K" -> 12,
    "A" -> 13
  )

  val jokerRanking: Map[String, Int] = cardRanking.updated("J", 0)
}
