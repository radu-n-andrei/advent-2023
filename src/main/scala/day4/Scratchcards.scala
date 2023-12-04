package day4

import scala.io.Source
import scala.annotation.tailrec

object Scratchcards {

  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day4.input")
    val input = file.getLines().toList
    val cards = input.map(Card(_))
    val resultMap = cards.map(c => c.id -> BigInt(1)).toMap
    println(s"SOL 1: ${cards.map(_.score).sum}")
    println(s"SOL 2: ${processCards(cards, resultMap)}")
    file.close()
  }

  @tailrec
  def processCards(cards: List[Card], resultMap: Map[Int, BigInt]): BigInt =
    cards match {
      case Nil => resultMap.values.sum
      case x :: xs =>
        val repeatedFor = resultMap(x.id)
        val range =
          (x.id + 1 to Math.min(resultMap.size, x.id + x.matchingNumbers))
        val newMap = range.foldLeft(resultMap)((acc, id) =>
          acc.updated(id, acc(id) + repeatedFor)
        )
        processCards(xs, newMap)
    }
}
