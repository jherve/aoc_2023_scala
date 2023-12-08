import day07._

val hands = for 
  str <- List("32T3K", "32T4K", "KKKK2", "KKKKK", "KKK99", "KKA99", "K2A99", "23456")
  hand = SortedHand.fromString(str)
yield
  hand.compare(hand)

val hand = SortedHand.fromString("KKKK2")

hand.compare(hand)

val comp = (hand, hand) match
    case (SortedHand.FOUR_OF_A_KIND(c11, c12), SortedHand.FOUR_OF_A_KIND(c21, c22)) =>
      val comp1 = c11.value.compare(c12.value)
      val comp2 = c12.value.compare(c22.value)
      if comp1 == 0 
      then (c12, c22) 
      else (c11, c21) 
comp

Hand.fromString("AAAAA")
Hand.fromString("AAAA9")
Hand.fromString("AAA99")
Hand.fromString("99888")
Hand.fromString("AAK99")
Hand.fromString("99KAA")
Hand.fromString("AAK89")
Hand.fromString("AQK89")
Hand.fromString("TQK89")

Hand.fromString("TQK89") == Hand.fromString("TQA89")
Hand.fromString("TTK89") > Hand.fromString("TQA89")

val input = """32T3K 765
                |T55J5 684
                |KK677 28
                |KTJJT 220
                            |QQQJA 483""".stripMargin

input.linesIterator.toList
val game = FullGame.fromString(input.linesIterator)

game.handBids
  .sortBy(_.hand)
  .zipWithIndex
  .map((hand, index) => hand.bid * (index + 1))
  .sum

import scala.io.Source
val lines = Source.fromFile("inputs/day_07.txt").getLines()

val fullGame = FullGame.fromString(lines)

fullGame.sorted
fullGame.handBids.length
