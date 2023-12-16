import scala.io.Source
import utils._
import day01._
import day04._
import day07.Day7
import day11.Day11
import day12.Day12
import day15.Day15
import day16.Day16

@main def hello(day: Int, part: Int, inputFile: String): Unit =
  val lines = Utils.lines(inputFile)

  val result = (day, part) match
    case (1, 1)  => getSum(lines)
    case (4, _)  => Day4.getSum(lines)
    case (7, 1)  => Day7.getSum(lines, false)
    case (7, 2)  => Day7.getSum(lines, true)
    case (11, 1) => Day11.sumFromImage(lines, 2)
    case (11, 2) => Day11.sumFromImage(lines, 1000000)
    case (12, _) => Day12.sumOfArrangements(lines)
    case (15, 1) => Day15.sumOfHashes(Utils.commaSeparatedValues(inputFile))
    case (15, 2) => Day15.focusingPower(Utils.commaSeparatedValues(inputFile))
    case (16, 1) => Day16.getEnergisedTiles(lines)

  println(result)
