import day11._

val input = """...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."""

val expandedInput = """....#........
.........#...
#............
.............
.............
........#....
.#...........
............#
.............
.............
.........#...
#....#......."""

class Day11Suite extends munit.FunSuite {
  test("string representation matches") {
    val image = Image.parse(input.linesIterator)
    assertEquals(input, image.toString())
  }

  test("expansion matches") {
    val image = Image.parse(input.linesIterator)
    val expandedImage = Image.parse(expandedInput.linesIterator)
    assertEquals(image.intoExpanded(2), expandedImage)
  }

  test("expansion matches for larger factors") {
    val image = Image.parse(input.linesIterator)
    assertEquals(image.intoExpanded(10).shortestPaths.sum, 1030)
    assertEquals(image.intoExpanded(100).shortestPaths.sum, 8410)
  }
}
