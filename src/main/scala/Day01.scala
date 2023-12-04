package day01

def getFirstLastDigit(line: String) = {
    val ints = line.filter(_.isDigit).map(_.toString.toInt)
    (ints.head, ints.last)
}

def getSum(lines: Iterator[String]): Int = {
  lines
    .map(getFirstLastDigit)
    .map(value => value._1 * 10 + value._2)
    .sum
}

