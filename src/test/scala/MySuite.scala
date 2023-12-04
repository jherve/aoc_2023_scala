import day01._

class Day01Suite extends munit.FunSuite {
  test("detect first and last digit in strings with no digit written out in letters") {
    assertEquals(day01.getFirstLastDigit("1abc2"), (1,2))
    assertEquals(day01.getFirstLastDigit("pqr3stu8vwx"), (3,8))
    assertEquals(day01.getFirstLastDigit("a1b2c3d4e5f"), (1,5))
    assertEquals(day01.getFirstLastDigit("treb7uchet"), (7,7))
  }
}
