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

enum Operation:
  case Dash
  case Equals(focalLength: Int)

case class Step(label: String, operation: Operation):
  def box = Hash.getHASHValue(label)

object Step:
  def parse(str: String): Option[Step] =
    str.split("\\=|-").toList match
      case List(label) => Some(Step(label, Operation.Dash))
      case List(label, focal) =>
        Some(Step(label, Operation.Equals(focal.toInt)))
      case _ => None

case class Lens(label: String, focalLength: Int)

case class Box(id: Int, lenses: List[Lens]):
  def add(lens: Lens) =
    val existingLensIdx = lenses.indexWhere(_.label == lens.label)
    if existingLensIdx >= 0 then Box(id, lenses.updated(existingLensIdx, lens))
    else Box(id, lenses.appended(lens))

  def remove(label: String) =
    Box(id, lenses.filterNot(_.label == label))

  def focusingPower =
    lenses.zipWithIndex
      .map((lens, idx) => (idx + 1) * lens.focalLength)
      .sum * (id + 1)

case class BoxArrangement(boxes: Map[Int, Box]):
  def run(step: Step) =
    val newBoxes = step match
      case Step(label, Operation.Dash) =>
        boxes.updatedWith(step.box)(box => Some(box.get.remove(label)))
      case Step(label, Operation.Equals(focalLength)) =>
        boxes.updatedWith(step.box)(box =>
          Some(box.get.add(Lens(label, focalLength)))
        )

    BoxArrangement(newBoxes)

  def focusingPower =
    boxes.filterNot((_, box) => box.lenses.isEmpty).map(_._2.focusingPower).sum

object BoxArrangement:
  def init() =
    BoxArrangement(Range(0, 256).map(id => (id, Box(id, List()))).toMap)

object Day15 {
  def sumOfHashes(values: Iterator[String]) =
    values.map(Hash.getHASHValue(_)).sum

  def focusingPower(values: Iterator[String]) =
    val arrangement = BoxArrangement.init()
    values
      .map(Step.parse(_).get)
      .foldLeft(arrangement)((arrangement, step) => arrangement.run(step))
      .focusingPower
}
