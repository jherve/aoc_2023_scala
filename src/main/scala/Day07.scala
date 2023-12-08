package day07
enum CardLabel:
  case A
  case K
  case Q
  case J
  case T
  case N9
  case N8
  case N7
  case N6
  case N5
  case N4
  case N3
  case N2

  def value = this match {
    case N2 => 1
    case N3 => 2
    case N4 => 3
    case N5 => 4
    case N6 => 5
    case N7 => 6
    case N8 => 7
    case N9 => 8
    case T  => 9
    case J  => 10
    case Q  => 11
    case K  => 12
    case A  => 13
  }

object CardLabel:
  def fromChar(char: Char) =
    char match {
      case 'A' => CardLabel.A
      case 'K' => CardLabel.K
      case 'Q' => CardLabel.Q
      case 'J' => CardLabel.J
      case 'T' => CardLabel.T
      case '9' => CardLabel.N9
      case '8' => CardLabel.N8
      case '7' => CardLabel.N7
      case '6' => CardLabel.N6
      case '5' => CardLabel.N5
      case '4' => CardLabel.N4
      case '3' => CardLabel.N3
      case '2' => CardLabel.N2
    }

case class Card(label: CardLabel)

enum HandType extends Ordered[HandType]:
  case FIVE_OF_A_KIND(card: CardLabel)
  case FOUR_OF_A_KIND(card: CardLabel)
  case FULL_HOUSE(highest: CardLabel, lowest: CardLabel)
  case THREE_OF_A_KIND(card: CardLabel)
  case TWO_PAIRS(highest: CardLabel, lowest: CardLabel)
  case ONE_PAIR(card: CardLabel)
  case HIGH_CARD(card: CardLabel)

  def compare(that: HandType) = {
    val strenghtComp = this.strength.compare(that.strength)
    if strenghtComp != 0 then {
      strenghtComp
    } else {
      val highestCompare = this.highestCard.compare(that.highestCard)
      if highestCard != 0 then {
        highestCompare
      } else {
        (this, that) match {
          case (TWO_PAIRS(_, s1), TWO_PAIRS(_, s2)) =>
            s1.value.compare(s2.value)
          case (FULL_HOUSE(_, s1), FULL_HOUSE(_, s2)) =>
            s1.value.compare(s2.value)
        }
      }
    }
  }

  def strength = this match {
    case HIGH_CARD(_)       => 1
    case ONE_PAIR(_)        => 2
    case TWO_PAIRS(_, _)    => 3
    case THREE_OF_A_KIND(_) => 4
    case FULL_HOUSE(_, _)   => 5
    case FOUR_OF_A_KIND(_)  => 6
    case FIVE_OF_A_KIND(_)  => 7
  }
  def highestCard = this match {
    case HIGH_CARD(h)       => h.value
    case ONE_PAIR(h)        => h.value
    case TWO_PAIRS(h, _)    => h.value
    case THREE_OF_A_KIND(h) => h.value
    case FULL_HOUSE(h, _)   => h.value
    case FOUR_OF_A_KIND(h)  => h.value
    case FIVE_OF_A_KIND(h)  => h.value
  }
  def secondHighest = this match {
    case TWO_PAIRS(_, s)  => s.value
    case FULL_HOUSE(_, s) => s.value
    case _                => 0
  }

object HandType:
  implicit val orderingByStrength: Ordering[HandType] =
    Ordering.by(t => (t.strength, t.highestCard, t.secondHighest))

case class Hand(cards: List[Card]) extends Ordered[Hand]:
  def sorted =
    cards
      .groupBy(_.label)
      .map((label, list) => (label, list.size))
      .toList
      .sortBy(_._2)
      .reverse

  def type_ = {
    sorted match
      case (highest, 5) :: Nil => HandType.FIVE_OF_A_KIND(highest)
      case (highest, 4) :: _   => HandType.FOUR_OF_A_KIND(highest)
      case (highest, 3) :: (second, 2) :: Nil =>
        HandType.FULL_HOUSE(highest, second)
      case (highest, 3) :: _ => HandType.THREE_OF_A_KIND(highest)
      case (oneCard, 2) :: (anotherCard, 2) :: _ =>
        val List(strongest, weakest) =
          List(oneCard, anotherCard).sortBy(_.value).reverse
        HandType.TWO_PAIRS(strongest, weakest)
      case (oneCard, 2) :: _ => HandType.ONE_PAIR(oneCard)
      case (oneCard, 1) :: _ =>
        val highest = sorted.map(_._1).sortBy(_.value).reverse.head
        HandType.HIGH_CARD(highest)
  }

  def compare(that: Hand) = this.type_.compare(that.type_)

object Hand:
  def fromString(string: String) =
    val cards = string.toCharArray().map(CardLabel.fromChar).map(Card(_)).toList
    Hand(cards)
case class HandBid(hand: Hand, bid: Int)

object HandBid:
  def fromString(string: String) =
    string.split("\\ ").toList match
      case handStr :: bidStr :: Nil =>
        Some(HandBid(Hand.fromString(handStr), bidStr.toInt))
      case _ => None

case class FullGame(handBids: List[HandBid]):
  def winnings =
    handBids
      .sortBy(_.hand)
      .zipWithIndex
      .map((hand, index) => hand.bid * (index + 1))
      .sum

object FullGame:
  def fromString(lines: Iterator[String]) =
    FullGame(
      lines.map(HandBid.fromString(_)).toList.map(_.get)
    )

object Day7 {
  def getSum(lines: Iterator[String]) = {
    FullGame.fromString(lines).winnings
  }
}
