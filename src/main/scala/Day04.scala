package day04

case class Card (id: String, winning: Set[Int], mine: Set[Int])

def intoCard(string: String) = {
    string.split(": ").toList match {
        case cardId :: rest :: Nil => 
            rest.split(" \\| ").toList match {
                case winningStr :: mineStr :: Nil => 
                    val winning = winningStr.trim().split("\\s+").toSet.map(_.toInt)
                    val mine = mineStr.trim().split("\\s+").toSet.map(_.toInt)
                    Card(cardId, winning, mine)
            }
    }
}

def getWinning(card: Card) = {
    card.winning.intersect(card.mine)
}

def getSumForDay04(lines: Iterator[String]) = {
    lines.
    map(intoCard)
    .map(getWinning)
    .toList
}
