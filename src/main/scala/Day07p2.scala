package day07p2
/* See the problem here : https://adventofcode.com/2023/day/7

  It basically consists of parsing such inputs as "32T3K 765" into
  a "hand" (of a 3, a 2, a 10, a 3 and a Kind) and a "bid" (of value 765).

  Hands can be sorted depending on the cards they contain, with a poker-style
  order (five of a kind, four of a kind, full house, pair, ...). In case of a
  tie, hands H1 and H2 are sorted by comparing the 1st card of H1 against the
  1st card of H2, then the 2nd card of H1 against the 2nd card of H2, and so on.

  The value of a list of (hand, bid) is determined by sorting all the hands
  according to their value.
 */

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
  case Joker

  def value = this match {
    case Joker => 0
    case N2    => 1
    case N3    => 2
    case N4    => 3
    case N5    => 4
    case N6    => 5
    case N7    => 6
    case N8    => 7
    case N9    => 8
    case T     => 9
    case J     => 10
    case Q     => 11
    case K     => 12
    case A     => 13
  }

  def compare(that: CardLabel) =
    this.value.compare(that.value)

object CardLabel:
  def parse(char: Char, jokerRule: Boolean) =
    char match {
      case 'A' => CardLabel.A
      case 'K' => CardLabel.K
      case 'Q' => CardLabel.Q
      case 'J' => if jokerRule then CardLabel.Joker else CardLabel.J
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

enum SortedHand extends Ordered[SortedHand]:
  case FIVE_OF_A_KIND(main: CardLabel)
  case FOUR_OF_A_KIND(main: CardLabel)
  case FULL_HOUSE(main: CardLabel)
  case THREE_OF_A_KIND(main: CardLabel)
  case TWO_PAIRS(main: CardLabel)
  case ONE_PAIR(main: CardLabel)
  case HIGH_CARD(main: CardLabel)
  case FIVE_JOKERS

  def intrinsicStrength = this match {
    case FIVE_JOKERS        => 0
    case HIGH_CARD(_)       => 1
    case ONE_PAIR(_)        => 2
    case TWO_PAIRS(_)       => 3
    case THREE_OF_A_KIND(_) => 4
    case FULL_HOUSE(_)      => 5
    case FOUR_OF_A_KIND(_)  => 6
    case FIVE_OF_A_KIND(_)  => 7
  }

  def compare(that: SortedHand) =
    this.intrinsicStrength.compare(that.intrinsicStrength)

object SortedHand:
  def sorted(cards: List[Card]) =
    cards
      .groupBy(_.label)
      .map((label, list) => (label, list.size))
      .toList
      .sortBy(el => (-el._2, -el._1.value))

  def fromCards(cards: List[Card]) =
    sorted(cards.filter(_.label != CardLabel.Joker)) match
      case (main, 5) :: Nil         => Some(SortedHand.FIVE_OF_A_KIND(main))
      case (main, 4) :: _           => Some(SortedHand.FOUR_OF_A_KIND(main))
      case (main, 3) :: (_, 2) :: _ => Some(SortedHand.FULL_HOUSE(main))
      case (main, 3) :: _           => Some(SortedHand.THREE_OF_A_KIND(main))
      case (main, 2) :: (_, 2) :: _ => Some(SortedHand.TWO_PAIRS(main))
      case (main, 2) :: _           => Some(SortedHand.ONE_PAIR(main))
      case (main, 1) :: _           => Some(SortedHand.HIGH_CARD(main))
      case Nil                      => Some(SortedHand.FIVE_JOKERS)
      case _                        => None

  def parse(string: String, jokerRule: Boolean) =
    val cards = string
      .toCharArray()
      .map(CardLabel.parse(_, jokerRule))
      .map(Card(_))
      .toList
    fromCards(cards)

case class Hand(cards: List[Card]) extends Ordered[Hand]:
  def asSorted =
    SortedHand.fromCards(cards).get

  def compare(that: Hand) =
    val strengthCompare =
      this.improvedHand.get.intrinsicStrength
        .compare(that.improvedHand.get.intrinsicStrength)
    val nthCompare = List(0, 1, 2, 3, 4).map(this.compareNth(that, _))

    (strengthCompare, nthCompare) match
      case (0, 0 :: 0 :: 0 :: 0 :: c :: Nil) => c
      case (0, 0 :: 0 :: 0 :: c :: _)        => c
      case (0, 0 :: 0 :: c :: _)             => c
      case (0, 0 :: c :: _)                  => c
      case (0, c :: _)                       => c
      case (c, _)                            => c

  def compareNth(that: Hand, n: Int) =
    this.cards(n).compare(that.cards(n))

  def nJokers =
    cards.count(_.label == CardLabel.Joker)

  def improvedHand =
    (this.asSorted, nJokers) match
      case (hand, 0) => Some(hand)

      case (SortedHand.FOUR_OF_A_KIND(main), 1) =>
        Some(SortedHand.FIVE_OF_A_KIND(main))

      case (SortedHand.FULL_HOUSE(main), 2) =>
        Some(SortedHand.FIVE_OF_A_KIND(main))
      case (SortedHand.FULL_HOUSE(main), 1) =>
        Some(SortedHand.FOUR_OF_A_KIND(main))

      case (SortedHand.THREE_OF_A_KIND(main), 2) =>
        Some(SortedHand.FIVE_OF_A_KIND(main))

      case (SortedHand.THREE_OF_A_KIND(main), 1) =>
        Some(SortedHand.FOUR_OF_A_KIND(main))

      case (SortedHand.TWO_PAIRS(main), 1) => Some(SortedHand.FULL_HOUSE(main))
      case (SortedHand.ONE_PAIR(main), 1) =>
        Some(SortedHand.THREE_OF_A_KIND(main))
      case (SortedHand.ONE_PAIR(main), 2) =>
        Some(SortedHand.FOUR_OF_A_KIND(main))
      case (SortedHand.ONE_PAIR(main), 3) =>
        Some(SortedHand.FIVE_OF_A_KIND(main))
      case (SortedHand.HIGH_CARD(main), 1) => Some(SortedHand.ONE_PAIR(main))
      case (SortedHand.HIGH_CARD(main), 2) =>
        Some(SortedHand.THREE_OF_A_KIND(main))
      case (SortedHand.HIGH_CARD(main), 3) =>
        Some(SortedHand.FOUR_OF_A_KIND(main))
      case (SortedHand.HIGH_CARD(main), 4) =>
        Some(SortedHand.FIVE_OF_A_KIND(main))
      case (SortedHand.FIVE_JOKERS, 5) =>
        Some(SortedHand.FIVE_OF_A_KIND(CardLabel.A))
      case _ => None

object Hand:
  def parse(string: String, jokerRule: Boolean) =
    val cards = string
      .toCharArray()
      .map(CardLabel.parse(_, jokerRule: Boolean))
      .map(Card(_))
      .toList
    Hand(cards)

case class HandBid(hand: Hand, bid: Int)

object HandBid:
  def parse(string: String, jokerRule: Boolean) =
    string.split("\\ ").toList match
      case handStr :: bidStr :: Nil =>
        Some(HandBid(Hand.parse(handStr, jokerRule: Boolean), bidStr.toInt))
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
  def parse(lines: Iterator[String], jokerRule: Boolean) =
    FullGame(
      lines.map(HandBid.parse(_, jokerRule: Boolean)).toList.map(_.get)
    )

object Day7Part2 {
  def getSum(lines: Iterator[String], jokerRule: Boolean) = {
    val game = FullGame.parse(lines, jokerRule: Boolean)

    game.winnings
  }
}
