import scala.io.Source

def day_01(lines: Iterator[String]) = {
  lines
    .map(_.filter(_.isDigit).map(_.toString.toInt))
    .map(l => (l.head, l.last))
    .foldLeft(0)((acc, value) => acc + value._1 * 10 + value._2)
}

@main def hello(day: Int, part: Int, inputFile: String): Unit =
  val lines = Source.fromFile(inputFile).getLines()

  val result = (day, part) match
    case (1, 1) => day_01(lines)

  println(result)
