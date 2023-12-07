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

case class Card(label: CardLabel):
  def value = label match {
    case CardLabel.N2 => 1
    case CardLabel.N3 => 2
    case CardLabel.N4 => 3
    case CardLabel.N5 => 4
    case CardLabel.N6 => 5
    case CardLabel.N7 => 6
    case CardLabel.N8 => 7
    case CardLabel.N9 => 8
    case CardLabel.T  => 9
    case CardLabel.J  => 10
    case CardLabel.Q  => 11
    case CardLabel.K  => 12
    case CardLabel.A  => 13
  }

enum HandType:
  case FIVE_OF_A_KIND(card: CardLabel)
  case FOUR_OF_A_KIND(card: CardLabel)
  case FULL_HOUSE(highest: CardLabel, lowest: CardLabel)
  case THREE_OF_A_KIND(card: CardLabel)
  case TWO_PAIRS(highest: CardLabel, lowest: CardLabel)
  case ONE_PAIR(card: CardLabel)
  case HIGH_CARD(card: CardLabel)

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
        HandType.TWO_PAIRS(oneCard, anotherCard)
      case (oneCard, 2) :: _ => HandType.ONE_PAIR(oneCard)
      case (oneCard, 1) :: _ => HandType.HIGH_CARD(oneCard)
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

List("AAAAA","AAAA9","AAA99","AAK99","AAK89","AQK89").map(Hand.fromString).map(_.sorted)

Hand.fromString("AAAAA").type_
Hand.fromString("AAAA9").type_
Hand.fromString("AAA99").type_
Hand.fromString("AAK99").type_
Hand.fromString("AAK89").type_
Hand.fromString("AQK89").type_
