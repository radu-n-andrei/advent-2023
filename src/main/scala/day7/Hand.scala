package day7

sealed abstract class Hand(val cards: List[Card], val rank: Int)

case class FiveOfAKind(override val cards: List[Card]) extends Hand(cards, 7)
case class FourOfAKind(override val cards: List[Card]) extends Hand(cards, 6)
case class FullHouse(override val cards: List[Card]) extends Hand(cards, 5)
case class ThreeOfAKind(override val cards: List[Card]) extends Hand(cards, 4)
case class TwoPairs(override val cards: List[Card]) extends Hand(cards, 3)
case class OnePair(override val cards: List[Card]) extends Hand(cards, 2)
case class HighCard(override val cards: List[Card]) extends Hand(cards, 1)

object Hand {

  implicit val ordering: Ordering[Hand] = gameOrdering(Card.cardRanking)(_, _)

  def gameOrdering(cardRanking: Map[String, Int]): (Hand, Hand) => Int =
    (card1, card2) => {
      val handRank = card1.rank - card2.rank
      if (handRank == 0) {
        card1.cards
          .zip(card2.cards)
          .map { case (c1, c2) =>
            cardRanking(c1.cardType) - cardRanking(c2.cardType)
          }
          .filterNot(_ == 0)
          .headOption
          .getOrElse(0)
      } else handRank
    }

  def apply(cards: List[Card]): Hand = {
    val cardMap = cards.groupBy(c => c.cardType).view.mapValues(_.size).toMap
    cardMap match {
      case c if c.keySet.size == 1                       => FiveOfAKind(cards)
      case c if c.values.exists(_ == 4)                  => FourOfAKind(cards)
      case c if c.keySet.size == 2                       => FullHouse(cards)
      case c if c.values.exists(_ == 3)                  => ThreeOfAKind(cards)
      case c if c.values.filter(_ == 2).toList.size == 2 => TwoPairs(cards)
      case c if c.values.filter(_ == 2).toList.size == 1 => OnePair(cards)
      case _                                             => HighCard(cards)
    }
  }

  def applyWithJoker(cards: List[Card]): Hand = {
    val cardMap = cards.groupBy(c => c.cardType).view.mapValues(_.size).toMap
    val jokers = cardMap.getOrElse("J", 0)
    val standardHand = apply(cards)
    jokers match {
      case 5 | 4 => FiveOfAKind(cards) // 5 or 4 Jokers will form a 5 of a kind
      case 3 =>
        standardHand match {
          case FullHouse(_) =>
            FiveOfAKind(cards) // full house JJJXX => 5 of a kind
          case _ => FourOfAKind(cards) // otherwise JJJXY => 4 of a kind
        }
      case 2 =>
        standardHand match {
          case FullHouse(_) => FiveOfAKind(cards) // JJXXX => 5 of a kind
          case TwoPairs(_)  => FourOfAKind(cards) // JJXXY => 4 of a kind
          case _            => ThreeOfAKind(cards) // JJXYZ => 3 of a kind
        }
      case 1 =>
        standardHand match {
          case FourOfAKind(_)  => FiveOfAKind(cards) // JXXXX => 5 of a kid
          case ThreeOfAKind(_) => FourOfAKind(cards) // JXXXY => 4 of a kind
          case TwoPairs(_)     => FullHouse(cards) // JXXYY => full house
          case OnePair(_)      => ThreeOfAKind(cards) // JXXYZ => 3 of a kind
          case _               => OnePair(cards) // JXYZQ => 1 pair
        }
      case 0 => standardHand
    }
  }
}
