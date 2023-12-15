package day15
/* See the problem here : https://adventofcode.com/2023/day/15 */

object Hash:
  def getHASHValue(str: String): Int =
    str.foldLeft(0)((hash, char) => {
      var value = char.toInt + hash
      value *= 17
      value %= 256
      value
    })

object Day15 {
  def sumOfHashes(values: Iterator[String]) =
    values.map(Hash.getHASHValue(_)).sum
}
