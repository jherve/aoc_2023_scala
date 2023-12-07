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

enum HandType:
  case FIVE_OF_A_KIND(card: CardLabel)
  case FOUR_OF_A_KIND(card: CardLabel)
  case FULL_HOUSE(highest: CardLabel, lowest: CardLabel)
  case THREE_OF_A_KIND(card: CardLabel)
  case TWO_PAIRS(highest: CardLabel, lowest: CardLabel)
  case ONE_PAIR(card: CardLabel)
  case HIGH_CARD(card: CardLabel)

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

case class Hand(cards: List[Card]):
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

object Hand:
  def fromString(string: String) =
    val cards = string.toCharArray().map(CardLabel.fromChar).map(Card(_)).toList
    Hand(cards)

object Day7 {
  def getSum(lines: Iterator[String]) = {
    lines
  }
}

CardLabel.A

val handStr = "32T3K"

val hand = Hand.fromString(handStr)
hand.cards.map(_.label).forall(_ == CardLabel.A)
hand.cards
  .groupBy(_.label)
  .map((label, list) => (label, list.size))
  .toList
  .sortBy(_._2)
  .reverse

List("AAAAA", "AAAA9", "AAA99", "AAK99", "AAK89", "AQK89")
  .map(Hand.fromString)
  .map(_.sorted)

Hand.fromString("AAAAA").type_
Hand.fromString("AAAA9").type_
Hand.fromString("AAA99").type_
Hand.fromString("99888").type_
Hand.fromString("AAK99").type_
Hand.fromString("99KAA").type_
Hand.fromString("AAK89").type_
Hand.fromString("AQK89").type_
Hand.fromString("TQK89").type_

HandType.orderingByStrength.compare(
  Hand.fromString("TQK99").type_,
  Hand.fromString("TTK89").type_
)

Hand.fromString("TQK89").type_ == Hand.fromString("TQA89").type_
