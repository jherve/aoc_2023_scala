import day16._

val input = """.|...\....
            ||.-.\.....
            |.....|-...
            |........|.
            |..........
            |.........\
            |..../.\\..
            |.-.-/..|..
            |.|....-|.\
            |..//.|....""".stripMargin

input.linesIterator.toList
GridOccupation.parse('/')
Contraption.parse(input.linesIterator)
