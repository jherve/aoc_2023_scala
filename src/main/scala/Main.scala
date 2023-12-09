import scala.io.Source
import day01._
import day04._
import day07.Day7
import day07p2.Day7Part2

@main def hello(day: Int, part: Int, inputFile: String): Unit =
  val lines = Source.fromFile(inputFile).getLines()

  val result = (day, part) match
    case (1, 1) => getSum(lines)
    case (4, _) => Day4.getSum(lines)
    case (7, 1) => Day7Part2.getSum(lines, false)
    case (7, 2) => Day7Part2.getSum(lines, true)

  println(result)
