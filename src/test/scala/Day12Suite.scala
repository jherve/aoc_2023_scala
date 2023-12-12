import day12._

class Day12Suite extends munit.FunSuite {
  val inputs = List(
    "#.#.### 1,1,3",
    ".#...#....###. 1,1,3",
    ".#.###.#.###### 1,3,1,6",
    "####.#...#... 4,1,1",
    "#....######..#####. 1,6,5",
    ".###.##....# 3,2,1"
  )

  inputs.foreach(input =>
    test(s"check groups match conditions for ${input}") {
      val record = ConditionRecord.parse(input).get
      assertEquals(record.springs.toDamaged, Some(record.damagedRecords))
    }
  )
}
