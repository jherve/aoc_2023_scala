import scala.io.Source
import day01._
import day04._

@main def hello(day: Int, part: Int, inputFile: String): Unit =
  val lines = Source.fromFile(inputFile).getLines()

  val result = (day, part) match
    case (1, 1) => getSum(lines)
    case (4, _) => getSumForDay04(lines)

  println(result)
