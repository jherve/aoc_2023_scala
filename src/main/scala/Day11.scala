package day11
import scala.math._

case class Position(x: Int, y: Int)

case class EmptySpace(position: Position)
case class Galaxy(position: Position):
  def positionAfterExpansion(image: Image) =
    val emptyRowsBefore = image.emptyRows.filter(_ < position.y).size
    val emptyColsBefore = image.emptyCols.filter(_ < position.x).size
    Position(position.x + emptyColsBefore, position.y + emptyRowsBefore)

  def distanceTo(that: Galaxy) =
    abs(this.position.y - that.position.y) + abs(
      this.position.x - that.position.x
    )

case class Image(width: Int, height: Int, galaxies: List[Galaxy]):
  def emptyRows =
    Range(0, height).toSet.diff(galaxies.map(_.position.y).toSet)

  def emptyCols =
    Range(0, width).toSet.diff(galaxies.map(_.position.x).toSet)

  def intoExpanded =
    Image(
      width + emptyCols.size,
      height + emptyRows.size,
      galaxies.map(g => Galaxy(g.positionAfterExpansion(this)))
    )

  def shortestPaths =
    val allUniquePairs = for
      (g1, idx1) <- galaxies.zipWithIndex
      (g2, idx2) <- galaxies.zipWithIndex
      if idx1 < idx2
    yield (g1, g2)

    allUniquePairs.map((g1, g2) => g1.distanceTo(g2))

  private def toLine(y: Int): String =
    Range(0, width)
      .map(x =>
        if galaxies.exists(_.position == Position(x, y)) then '#' else '.'
      )
      .mkString

  override def toString(): String =
    Range(0, height).map(toLine).mkString("\n")

object Image:
  def parse(lines: Iterator[String]) =
    val linesList = lines.toList
    val height = linesList.size
    val width = linesList(0).size
    val chars = linesList.map(_.toCharArray().zipWithIndex).zipWithIndex
    val posValue = for
      (charList, y) <- chars
      (char, x) <- charList
    yield (x, y, char)

    val galaxies =
      posValue.filter(_._3 == '#').map((x, y, _) => Galaxy(Position(x, y)))
    Image(width, height, galaxies)

object Day11 {
  def sumFromImage(lines: Iterator[String]) = {
    Image.parse(lines).intoExpanded.shortestPaths.sum
  }
}
