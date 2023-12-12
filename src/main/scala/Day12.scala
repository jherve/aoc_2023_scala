package day12
/* See the problem here : https://adventofcode.com/2023/day/12 */

enum SpringCondition:
  case Broken
  case Operational
  case Unknown

object SpringCondition:
  def parse(c: Char) =
    c match
      case '#' => Some(Broken)
      case '.' => Some(Operational)
      case '?' => Some(Unknown)
      case _   => None

type ListOfDamagedRecords = List[Int]

case class ConditionRecord(
    springs: List[SpringCondition],
    damagedRecords: ListOfDamagedRecords
)

object ConditionRecord:
  def parse(str: String) =
    str.strip.split("\\ ").toList match
      case conditions :: damaged :: Nil =>
        Some(
          ConditionRecord(
            conditions.toCharArray.map(SpringCondition.parse(_).get).toList,
            damaged.split(",").toList.map(_.toInt)
          )
        )
      case _ => None

case class PuzzleInput(records: List[ConditionRecord])

object PuzzleInput:
  def parse(lines: Iterator[String]) =
    lines.map(ConditionRecord.parse).toList

object Day12 {
  def sumOfArrangements(lines: Iterator[String]) =
    PuzzleInput.parse(lines)
}
