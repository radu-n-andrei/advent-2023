package day15

final case class Box(lenses: List[Lens]) {

  def remove(label: String): Box =
    Box(lenses.filterNot(_.label == label))

  def add(lens: Lens): Box = {
    val ind = lenses.map(_.label).indexOf(lens.label)
    if (ind < 0) Box(lenses :+ lens)
    else {
      val parts = lenses.splitAt(ind)
      Box((parts._1 :+ lens) ++ parts._2.tail)
    }
  }

  def focusingPower(index: Int): Int =
    lenses.zipWithIndex.map { case (l, i) =>
      (1 + index) * (i + 1) * l.focalStrength
    }.sum
}

object Box {
  val empty = Box(List.empty)
}

final case class Lens(label: String, focalStrength: Int)
