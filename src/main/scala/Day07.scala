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

enum SortedHand extends Ordered[SortedHand]:
  case FIVE_OF_A_KIND(main: CardLabel)
  case FOUR_OF_A_KIND(main: CardLabel, last: CardLabel)
  case FULL_HOUSE(main: CardLabel, lowest: CardLabel)
  case THREE_OF_A_KIND(main: CardLabel, second: CardLabel, third: CardLabel)
  case TWO_PAIRS(main: CardLabel, lowest: CardLabel, last: CardLabel)
  case ONE_PAIR(
      main: CardLabel,
      second: CardLabel,
      third: CardLabel,
      fourth: CardLabel
  )
  case HIGH_CARD(
      card: CardLabel,
      second: CardLabel,
      third: CardLabel,
      fourth: CardLabel,
      fifth: CardLabel
  )

  def intrinsicStrength = this match {
    case HIGH_CARD(_, _, _, _, _) => 1
    case ONE_PAIR(_, _, _, _)     => 2
    case TWO_PAIRS(_, _, _)       => 3
    case THREE_OF_A_KIND(_, _, _) => 4
    case FULL_HOUSE(_, _)         => 5
    case FOUR_OF_A_KIND(_, _)     => 6
    case FIVE_OF_A_KIND(_)        => 7
  }

  def compare(that: SortedHand) = {
    (this, that) match {
      case (FIVE_OF_A_KIND(c1), FIVE_OF_A_KIND(c2)) =>
        c1.value.compare(c2.value)

      case (FOUR_OF_A_KIND(c11, c12), FOUR_OF_A_KIND(c21, c22)) =>
        val comp1 = c11.value.compare(c21.value)
        val comp2 = c12.value.compare(c22.value)
        if comp1 == 0 then comp2 else comp1

      case (FULL_HOUSE(c11, c12), FULL_HOUSE(c21, c22)) =>
        val comp1 = c11.value.compare(c21.value)
        val comp2 = c12.value.compare(c22.value)
        if comp1 == 0 then comp2 else comp1

      case (THREE_OF_A_KIND(c11, c12, c13), THREE_OF_A_KIND(c21, c22, c23)) =>
        val comp1 = c11.value.compare(c21.value)
        val comp2 = c12.value.compare(c22.value)
        val comp3 = c13.value.compare(c23.value)
        if comp1 == 0 then
          if comp2 == 0 then comp3
          else comp2
        else comp1

      case (TWO_PAIRS(c11, c12, c13), TWO_PAIRS(c21, c22, c23)) =>
        val comp1 = c11.value.compare(c21.value)
        val comp2 = c12.value.compare(c22.value)
        val comp3 = c13.value.compare(c23.value)
        if comp1 == 0 then
          if comp2 == 0 then comp3
          else comp2
        else comp1

      case (ONE_PAIR(c11, c12, c13, c14), ONE_PAIR(c21, c22, c23, c24)) =>
        val comp1 = c11.value.compare(c21.value)
        val comp2 = c12.value.compare(c22.value)
        val comp3 = c13.value.compare(c23.value)
        val comp4 = c14.value.compare(c24.value)
        if comp1 == 0 then
          if comp2 == 0 then
            if comp3 == 0 then comp4
            else comp3
          else comp2
        else comp1

      case (
            HIGH_CARD(c11, c12, c13, c14, c15),
            HIGH_CARD(c21, c22, c23, c24, c25)
          ) =>
        val comp1 = c11.value.compare(c21.value)
        val comp2 = c12.value.compare(c22.value)
        val comp3 = c13.value.compare(c23.value)
        val comp4 = c14.value.compare(c24.value)
        val comp5 = c15.value.compare(c25.value)
        if comp1 == 0 then
          if comp2 == 0 then
            if comp3 == 0 then
              if comp4 == 0 then comp5
              else comp4
            else comp3
          else comp2
        else comp1

      case (h1, h2) =>
        h1.intrinsicStrength.compare(h2.intrinsicStrength)
    }
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
      case (highest, 5) :: Nil => SortedHand.FIVE_OF_A_KIND(highest)
      case (highest, 4) :: (lowest, 1) :: Nil =>
        SortedHand.FOUR_OF_A_KIND(highest, lowest)
      case (highest, 3) :: (second, 2) :: Nil =>
        SortedHand.FULL_HOUSE(highest, second)
      case (highest, 3) :: (second, 1) :: (third, 1) :: Nil =>
        SortedHand.THREE_OF_A_KIND(highest, second, third)
      case (strongest, 2) :: (weakest, 2) :: (last, 1) :: Nil =>
        SortedHand.TWO_PAIRS(strongest, weakest, last)
      case (oneCard, 2) :: (second, 1) :: (third, 1) :: (fourth, 1) :: Nil =>
        SortedHand.ONE_PAIR(oneCard, second, third, fourth)
      case (oneCard, 1) :: (second, 1) :: (third, 1) :: (fourth, 1) :: (
            fifth,
            1
          ) :: Nil =>
        SortedHand.HIGH_CARD(oneCard, second, third, fourth, fifth)

  def fromString(string: String) =
    val cards = string.toCharArray().map(CardLabel.fromChar).map(Card(_)).toList
    fromCards(cards)

case class Hand(cards: List[Card]) extends Ordered[Hand]:
  def asSorted =
    SortedHand.fromCards(cards)

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
    this.cards(n).label.value.compare(that.cards(n).label.value)

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
  def sorted =
    handBids
      .sortBy(_.hand)

  def winnings =
    sorted.zipWithIndex
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
