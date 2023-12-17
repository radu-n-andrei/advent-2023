package day17

final case class PathNode(
    coord: Coordinate,
    enteredVia: Direction,
    score: Int,
    canTurn: Boolean = true
) {

  val directedCoordinate: DirectedCoordinate =
    DirectedCoordinate(enteredVia, coord)

  def moves(step: Int): List[List[DirectedCoordinate]] =
    enteredVia.transversal.map(d =>
      (1 to step)
        .map(step => DirectedCoordinate(d.opposite, d.moveBy(coord, step)))
        .toList
    )
}

sealed trait Direction {
  val transversal: List[Direction]
  def moveBy(coord: Coordinate, step: Int): Coordinate
  def opposite: Direction
}
case object North extends Direction {
  override val transversal: List[Direction] = List(East, West)
  override def moveBy(coord: Coordinate, step: Int): Coordinate =
    Coordinate(coord.x, coord.y - step)
  override def opposite: Direction = South
}
case object South extends Direction {
  override val transversal: List[Direction] = List(East, West)
  override def moveBy(coord: Coordinate, step: Int): Coordinate =
    Coordinate(coord.x, coord.y + step)
  override def opposite: Direction = North
}
case object East extends Direction {
  override val transversal: List[Direction] = List(North, South)
  override def moveBy(coord: Coordinate, step: Int): Coordinate =
    Coordinate(coord.x + step, coord.y)
  override def opposite: Direction = West
}
case object West extends Direction {
  override val transversal: List[Direction] = List(North, South)
  override def moveBy(coord: Coordinate, step: Int): Coordinate =
    Coordinate(coord.x - step, coord.y)
  override def opposite: Direction = East
}

final case class DirectedCoordinate(
    direction: Direction,
    coordinate: Coordinate
)
