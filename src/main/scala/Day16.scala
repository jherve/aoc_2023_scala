package day16
/* See the problem here : https://adventofcode.com/2023/day/16 */

case class Size(width: Int, height: Int)

case class Position(x: Int, y: Int):
  def next(direction: Direction) =
    direction match
      case Direction.Rightward => Position(x + 1, y)
      case Direction.Leftward  => Position(x - 1, y)
      case Direction.Upward    => Position(x, y - 1)
      case Direction.Downward  => Position(x, y + 1)

  def isWithin(gridSize: Size) =
    (0 <= x) && (x <= gridSize.width) && (0 <= y) && (y <= gridSize.height)

enum SplitterOrientation:
  case |
  case -

enum MirrorOrientation:
  case \
  case /

enum GridOccupation:
  case EmptySpace
  case Mirror(orientation: MirrorOrientation)
  case Splitter(orientation: SplitterOrientation)

object GridOccupation:
  def parse(str: Char): Option[GridOccupation] =
    str match
      case '.'  => Some(GridOccupation.EmptySpace)
      case '/'  => Some(GridOccupation.Mirror(MirrorOrientation./))
      case '\\' => Some(GridOccupation.Mirror(MirrorOrientation.\))
      case '-'  => Some(GridOccupation.Splitter(SplitterOrientation.-))
      case '|'  => Some(GridOccupation.Splitter(SplitterOrientation.|))
      case _    => None

case class Contraption(grid: Map[Position, GridOccupation]):
  def size =
    val max = grid.keySet.maxBy(position => (position.x, position.y))
    Size(max.x, max.y)

enum Direction:
  case Rightward
  case Leftward
  case Upward
  case Downward

  def mirrored(orientation: MirrorOrientation): Direction =
    (this, orientation) match
      case (Rightward, MirrorOrientation.\) => Downward
      case (Rightward, MirrorOrientation./) => Upward
      case (Leftward, MirrorOrientation.\)  => Upward
      case (Leftward, MirrorOrientation./)  => Downward
      case (Upward, MirrorOrientation.\)    => Leftward
      case (Upward, MirrorOrientation./)    => Rightward
      case (Downward, MirrorOrientation.\)  => Rightward
      case (Downward, MirrorOrientation./)  => Leftward

  def split(orientation: SplitterOrientation): List[Direction] =
    (this, orientation) match
      case (direction @ (Rightward | Leftward), SplitterOrientation.-) =>
        List(direction)
      case (direction @ (Downward | Upward), SplitterOrientation.|) =>
        List(direction)
      case ((Rightward | Leftward), SplitterOrientation.|) =>
        List(Upward, Downward)
      case ((Downward | Upward), SplitterOrientation.-) =>
        List(Leftward, Rightward)

case class Beam(position: Position, direction: Direction):
  def next(occupation: GridOccupation) =
    occupation match
      case GridOccupation.EmptySpace =>
        List(Beam(position.next(direction), direction))

      case GridOccupation.Mirror(orientation) =>
        val newDirection = direction.mirrored(orientation)
        List(Beam(position.next(newDirection), newDirection))

      case GridOccupation.Splitter(orientation) =>
        direction
          .split(orientation)
          .map(direction => Beam(position.next(direction), direction))

case class BeamTravel(
    contraption: Contraption,
    beams: Set[Beam],
    beamPositionsHistory: Set[(Position, Direction)]
):
  def beamsStillWithinGrid =
    beams.filter(_.position.isWithin(contraption.size))

  def energisedTiles = beamPositionsHistory.map(_._1)

  def next() =
    val newBeams = beamsStillWithinGrid
      .diff(beamPositionsHistory.map(b => Beam(b._1, b._2)))
      .flatMap(beam => beam.next(contraption.grid(beam.position)))

    BeamTravel(
      contraption,
      newBeams,
      beamPositionsHistory | beamsStillWithinGrid
        .map(b => (b.position, b.direction))
        .toSet
    )

  def toStep(n: Int) =
    LazyList
      .from(0)
      .scanLeft(this, 0)((acc, idx) => (acc._1.next(), idx))
      .takeWhile(_._2 < n)
      .last
      ._1

  def runUntilStationary() =
    LazyList
      .from(1)
      .scanLeft((this, false))((acc, idx) =>
        val next = acc._1.next()
        (next, next.beamPositionsHistory == acc._1.beamPositionsHistory)
      )
      .takeWhile(!_._2)
      .last
      ._1

  def asGridOfEnergised =
    def line(y: Int) =
      Range(0, contraption.size.width + 1)
        .map(x => if energisedTiles.contains(Position(x, y)) then '#' else '.')
        .mkString

    Range(0, contraption.size.height + 1).map(line).mkString("\n")

  def asGridOfBeams =
    def line(y: Int) =
      Range(0, contraption.size.width + 1)
        .map(x =>
          beamPositionsHistory.filter(_._1 == Position(x, y)).toList match
            case Nil                             => '.'
            case (_, Direction.Downward) :: Nil  => 'v'
            case (_, Direction.Upward) :: Nil    => '^'
            case (_, Direction.Leftward) :: Nil  => '<'
            case (_, Direction.Rightward) :: Nil => '>'
            case _ :: tail                       => tail.size + 1
        )
        .mkString

    Range(0, contraption.size.height + 1).map(line).mkString("\n")

object BeamTravel:
  def init(contraption: Contraption) =
    BeamTravel(
      contraption,
      Set(Beam(Position(0, 0), Direction.Rightward)),
      Set()
    )

object Contraption:
  def parse(lines: Iterator[String]) =
    val grid = lines.zipWithIndex
      .flatMap((string, y) =>
        string.zipWithIndex.map((char, x) =>
          (Position(x, y), GridOccupation.parse(char).get)
        )
      )
      .toMap

    Contraption(grid)

object Day16 {
  def getEnergisedTiles(values: Iterator[String]) =
    val contraption = Contraption.parse(values)
    val travel = BeamTravel.init(contraption)

    travel.runUntilStationary().energisedTiles.size
}
