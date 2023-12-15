import day15._

class Day15Suite extends munit.FunSuite {
  val hashes = List(
    ("rn=1", 30),
    ("cm-", 253),
    ("qp=3", 97),
    ("cm=2", 47),
    ("qp-", 14),
    ("pc=4", 180),
    ("ot=9", 9),
    ("ab=5", 197),
    ("pc-", 48),
    ("pc=6", 214),
    ("ot=7", 231)
  )

  test("produces the expected hash") {
    assertEquals(Hash.getHASHValue("HASH"), 52)
  }

  hashes.foreach((string, hash) =>
    test(s"produces the expected hash for ${string}") {
      assertEquals(Hash.getHASHValue(string), hash)
    }
  )
}
