package day10

final case class EnclosureGap(pipe1: Coordinate, pipe2: Coordinate) {
  val isVertical: Boolean = pipe1.y == pipe2.y
  val isHorizontal: Boolean = pipe1.x == pipe2.x

  def containsCoord(other: Coordinate) = pipe1 == other || pipe2 == other

  def diff(other: EnclosureGap): List[Coordinate] =
    List(pipe1, pipe2).diff(List(other.pipe1, other.pipe2))

  def reversed: EnclosureGap = EnclosureGap(pipe2, pipe1)

  def eq(other: EnclosureGap): Boolean =
    (pipe1 == other.pipe1 && pipe2 == other.pipe2) ||
      (pipe2 == other.pipe1 && pipe1 == other.pipe2)
}
