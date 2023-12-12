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

case class SpringConfiguration(spring: List[SpringCondition]):
  def toDamaged =
    spring.foldLeft((Some(List()): Option[List[Int]], 0))((acc, condition) =>
      val (damaged, currentBroken) = acc

      (condition, damaged, currentBroken) match
        case (_, None, nb)                    => (None, nb)
        case (SpringCondition.Unknown, _, nb) => (None, nb)
        case (SpringCondition.Broken, Some(damaged), nb) =>
          (Some(damaged), nb + 1)
        case (_, damaged, 0)        => (damaged, 0)
        case (_, Some(damaged), nb) => (Some(damaged ++ List(nb)), 0)
    ) match
      case (None, _)        => None
      case (list, 0)        => list
      case (Some(list), nb) => Some(list ++ List(nb))

  def alternates =
    spring.foldLeft(List(): List[SpringConfiguration])((acc, condition) =>
      val alternates = acc
      (alternates, condition) match
        case (Nil, SpringCondition.Unknown) =>
          List(
            SpringConfiguration(List(SpringCondition.Broken)),
            SpringConfiguration(List(SpringCondition.Operational))
          )
        case (Nil, cond) => List(SpringConfiguration(List(cond)))
        case (alternates, SpringCondition.Unknown) =>
          alternates.flatMap(conf =>
            List(
              SpringConfiguration(conf.spring ++ List(SpringCondition.Broken)),
              SpringConfiguration(
                conf.spring ++ List(SpringCondition.Operational)
              )
            )
          )
        case (alternates, cond) =>
          alternates.map(conf => SpringConfiguration(conf.spring ++ List(cond)))
    )

object SpringConfiguration:
  def parse(str: String) =
    SpringConfiguration(
      str.toCharArray.map(SpringCondition.parse(_).get).toList
    )

case class ConditionRecord(
    springs: SpringConfiguration,
    damagedRecords: ListOfDamagedRecords
):
  def alternatesThatMatchDamagedRecord =
    springs.alternates.filter(_.toDamaged.get == damagedRecords)

object ConditionRecord:
  def parse(str: String) =
    str.strip.split("\\ ").toList match
      case conditions :: damaged :: Nil =>
        Some(
          ConditionRecord(
            SpringConfiguration.parse(conditions),
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
    PuzzleInput
      .parse(lines)
      .map(_.get.alternatesThatMatchDamagedRecord.size)
      .sum
}
