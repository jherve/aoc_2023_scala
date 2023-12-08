import day07._


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
Hand.fromString("TTK89").type_ > Hand.fromString("TQA89").type_

val input = """32T3K 765
                |T55J5 684
                |KK677 28
                |KTJJT 220
                            |QQQJA 483""".stripMargin

input.linesIterator.toList
val game = FullGame.fromString(input.linesIterator)

game.handBids
  .sortBy(_.hand.type_)
  .zipWithIndex
  .map((hand, index) => hand.bid * (index + 1))
  .sum

import scala.io.Source
val lines = Source.fromFile("inputs/day_07.txt").getLines()

val fullGame = FullGame.fromString(lines)

val combinations=  for { 
  x <- fullGame.handBids 
  y <- fullGame.handBids 
  if x != y
  if x.hand.type_ == y.hand.type_
} yield (x, y)
combinations.size
combinations(0)
val h1 = Hand(List(Card(CardLabel.N4), Card(CardLabel.K), Card(CardLabel.N8), Card(CardLabel.J), Card(CardLabel.N9))) 
val h2 = Hand(List(Card(CardLabel.N9), Card(CardLabel.N8), Card(CardLabel.N2), Card(CardLabel.K), Card(CardLabel.T)))

h1.type_
h2.type_
