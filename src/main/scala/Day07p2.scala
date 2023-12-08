package day07p2
enum CardLabel extends Ordered[CardLabel]:
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
    case J  => 0
    case Q  => 11
    case K  => 12
    case A  => 13
  }

  def compare(that: CardLabel) =
    this.value.compare(that.value)

object CardLabel:
  def parse(char: Char) =
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

case class Card(label: CardLabel) extends Ordered[Card]:
  def compare(that: Card) =
    this.label.compare(that.label)

enum SortedHand:
  case FIVE_OF_A_KIND(main: CardLabel)
  case FOUR_OF_A_KIND(main: CardLabel)
  case FULL_HOUSE(main: CardLabel)
  case THREE_OF_A_KIND(main: CardLabel)
  case TWO_PAIRS(main: CardLabel)
  case ONE_PAIR(main: CardLabel)
  case HIGH_CARD

  def intrinsicStrength = this match {
    case HIGH_CARD          => 1
    case ONE_PAIR(_)        => 2
    case TWO_PAIRS(_)       => 3
    case THREE_OF_A_KIND(_) => 4
    case FULL_HOUSE(_)      => 5
    case FOUR_OF_A_KIND(_)  => 6
    case FIVE_OF_A_KIND(_)  => 7
  }

object SortedHand:
  def sorted(cards: List[Card]) =
    cards
      .groupBy(_.label)
      .map((label, list) => (label, list.size))
      .toList
      .sortBy(el => (-el._2, -el._1.value))

  def fromCards(cards: List[Card]) =
    sorted(cards) match
      case (main, 5) :: Nil         => Some(SortedHand.FIVE_OF_A_KIND(main))
      case (main, 4) :: _           => Some(SortedHand.FOUR_OF_A_KIND(main))
      case (main, 3) :: (_, 2) :: _ => Some(SortedHand.FULL_HOUSE(main))
      case (main, 3) :: _           => Some(SortedHand.THREE_OF_A_KIND(main))
      case (main, 2) :: (_, 2) :: _ => Some(SortedHand.TWO_PAIRS(main))
      case (main, 2) :: _           => Some(SortedHand.ONE_PAIR(main))
      case (_, 1) :: _              => Some(SortedHand.HIGH_CARD)
      case _                        => None

  def parse(string: String) =
    val cards = string.toCharArray().map(CardLabel.parse).map(Card(_)).toList
    fromCards(cards)

case class Hand(cards: List[Card]) extends Ordered[Hand]:
  def asSorted =
    SortedHand.fromCards(cards).get

  def compare(that: Hand) =
    val strengthComp =
      this.asSorted.intrinsicStrength.compare(that.asSorted.intrinsicStrength)
    if strengthComp == 0
    then
      val compare0 = this.compareNth(that, 0)
      val compare1 = this.compareNth(that, 1)
      val compare2 = this.compareNth(that, 2)
      val compare3 = this.compareNth(that, 3)
      val compare4 = this.compareNth(that, 4)
      if compare0 == 0 then
        if compare1 == 0 then
          if compare2 == 0 then
            if compare3 == 0 then compare4
            else compare3
          else compare2
        else compare1
      else compare0
    else strengthComp

  def compareNth(that: Hand, n: Int) =
    this.cards(n).compare(that.cards(n))

  def nJokers =
    cards.count(_.label == CardLabel.J)

  def improvedHand =
    (this.asSorted, nJokers) match
      case (hand, 0) => hand
      case (SortedHand.FOUR_OF_A_KIND(main), 1) =>
        SortedHand.FIVE_OF_A_KIND(main)
      case (SortedHand.FULL_HOUSE(main), 2) => SortedHand.FIVE_OF_A_KIND(main)
      case (SortedHand.FULL_HOUSE(main), 1) => SortedHand.FOUR_OF_A_KIND(main)
      case (SortedHand.THREE_OF_A_KIND(main), 2) =>
        SortedHand.FIVE_OF_A_KIND(main)
      case (SortedHand.THREE_OF_A_KIND(main), 1) =>
        SortedHand.FOUR_OF_A_KIND(main)
      case (SortedHand.TWO_PAIRS(main), 2) => SortedHand.FOUR_OF_A_KIND(main)
      case (SortedHand.TWO_PAIRS(main), 1) => SortedHand.THREE_OF_A_KIND(main)
      case (SortedHand.ONE_PAIR(main), 1)  => SortedHand.THREE_OF_A_KIND(main)
      case (SortedHand.HIGH_CARD, 1)       => SortedHand.ONE_PAIR(CardLabel.J)

object Hand:
  def parse(string: String) =
    val cards = string.toCharArray().map(CardLabel.parse).map(Card(_)).toList
    Hand(cards)

case class HandBid(hand: Hand, bid: Int)

object HandBid:
  def parse(string: String) =
    string.split("\\ ").toList match
      case handStr :: bidStr :: Nil =>
        Some(HandBid(Hand.parse(handStr), bidStr.toInt))
      case _ => None

case class FullGame(handBids: List[HandBid]):
  def sorted =
    handBids
      .sortBy(_.hand)

  def winnings =
    sorted.zipWithIndex
      .map((hand, index) => hand.bid * (index + 1))
      .sum

object FullGame:
  def parse(lines: Iterator[String]) =
    FullGame(
      lines.map(HandBid.parse(_)).toList.map(_.get)
    )

object Day7Part2 {
  def getSum(lines: Iterator[String]) = {
    FullGame.parse(lines).winnings
  }
}
