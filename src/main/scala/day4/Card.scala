package day4

final case class Card(
    id: Int,
    winningNumbers: List[Int],
    gameNumbers: List[Int]
) {

  val matchingNumbers: Int = gameNumbers.intersect(winningNumbers).size 
  val score: Int = 
    Math.pow(2, matchingNumbers - 1).toInt
}

object Card {

  val empty = Card(0, List.empty, List.empty)

  def apply(input: String): Card = {
    val template = s"Card\\s+(\\d+): (.*)\\|(.*)".r
    input match {
      case template(id, winning, mine) =>
        Card(
          id.toInt,
          winning.trim().split(" ").filter(_.nonEmpty).map(_.toInt).toList,
          mine.trim().split(" ").filter(_.nonEmpty).map(_.toInt).toList
        )
      case _ => 
        empty
    }
  }
}
