package day04

import scala.math.pow

case class Card(id: String, winning: Set[Int], mine: Set[Int])

object Day4 {
  def intoCard(string: String) = {
    string.split(": ").toList match {
      case cardId :: rest :: Nil =>
        rest.split(" \\| ").toList match {
          case winningStr :: mineStr :: Nil =>
            val winning = winningStr.trim().split("\\s+").toSet.map(_.toInt)
            val mine = mineStr.trim().split("\\s+").toSet.map(_.toInt)
            Some(Card(cardId, winning, mine))

          case _ => None
        }

      case _ => None
    }
  }

  def getWinning(card: Card) = {
    card.winning.intersect(card.mine)
  }

  def getSum(lines: Iterator[String]) = {
    lines
      .map(intoCard)
      .flatten
      .map(getWinning)
      .filterNot(_.isEmpty)
      .map(set => pow(2, set.size - 1).intValue)
      .sum
  }
}
