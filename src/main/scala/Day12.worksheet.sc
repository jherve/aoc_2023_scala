import scala.io.Source
import day12._

val input = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1""".stripMargin

input.linesIterator.toList(0).size
input.linesIterator.toList(1).size

input.linesIterator.map(_.strip.split("\\ ").toList).toList
PuzzleInput.parse(input.linesIterator)
