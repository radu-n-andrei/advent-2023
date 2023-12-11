package day10

final case class Coordinate private (x: Int, y: Int) {
  def isOnBound(horBound: Int, vertBound: Int): Boolean =
    x == 0 || y == 0 || x == horBound || y == vertBound
}

object Coordinate {
  def apply(x: Int, y: Int, maxX: Int, maxY: Int): Option[Coordinate] =
    if (x < 0 || x > maxX || y < 0 || y > maxY) None
    else Some(Coordinate(x, y))

  def moveNorth(coord: Coordinate, maxX: Int, maxY: Int): Option[Coordinate] =
    Coordinate(coord.x, coord.y - 1, maxX, maxY)

  def moveSouth(coord: Coordinate, maxX: Int, maxY: Int): Option[Coordinate] =
    Coordinate(coord.x, coord.y + 1, maxX, maxY)

  def moveEast(coord: Coordinate, maxX: Int, maxY: Int): Option[Coordinate] =
    Coordinate(coord.x + 1, coord.y, maxX, maxY)

  def moveWest(coord: Coordinate, maxX: Int, maxY: Int): Option[Coordinate] =
    Coordinate(coord.x - 1, coord.y, maxX, maxY)

  def moveNorthEast(
      coord: Coordinate,
      maxX: Int,
      maxY: Int
  ): Option[Coordinate] =
    Coordinate(coord.x + 1, coord.y - 1, maxX, maxY)

  def moveNorthWest(
      coord: Coordinate,
      maxX: Int,
      maxY: Int
  ): Option[Coordinate] =
    Coordinate(coord.x - 1, coord.y - 1, maxX, maxY)

  def moveSouthEast(
      coord: Coordinate,
      maxX: Int,
      maxY: Int
  ): Option[Coordinate] =
    Coordinate(coord.x + 1, coord.y - 1, maxX, maxY)

  def moveSouthWest(
      coord: Coordinate,
      maxX: Int,
      maxY: Int
  ): Option[Coordinate] =
    Coordinate(coord.x - 1, coord.y + 1, maxX, maxY)

  def allMoves(coord: Coordinate, maxX: Int, maxY: Int): List[Coordinate] =
    List(
      moveNorth(coord, maxX, maxY),
      moveSouth(coord, maxX, maxY),
      moveEast(coord, maxX, maxY),
      moveWest(coord, maxX, maxY),
      moveNorthEast(coord, maxX, maxY),
      moveNorthWest(coord, maxX, maxY),
      moveSouthEast(coord, maxX, maxY),
      moveSouthWest(coord, maxX, maxY)
    ).flatten

  def allMovesAsMap(
      coord: Coordinate,
      maxX: Int,
      maxY: Int
  ): Map[CardinalDirection, Coordinate] =
    Map(
      North -> moveNorth(coord, maxX, maxY),
      South -> moveSouth(coord, maxX, maxY),
      East -> moveEast(coord, maxX, maxY),
      West -> moveWest(coord, maxX, maxY),
      NorthEast -> moveNorthEast(coord, maxX, maxY),
      NorthWest -> moveNorthWest(coord, maxX, maxY),
      SouthEast -> moveSouthEast(coord, maxX, maxY),
      SouthWest -> moveSouthWest(coord, maxX, maxY)
    ).filterNot(_._2.isEmpty).view.mapValues(_.get).toMap

  def standardMoves(
      coord: Coordinate,
      maxX: Int,
      maxY: Int
  ): Map[CardinalDirection, Coordinate] =
    Map(
      North -> moveNorth(coord, maxX, maxY),
      South -> moveSouth(coord, maxX, maxY),
      East -> moveEast(coord, maxX, maxY),
      West -> moveWest(coord, maxX, maxY)
    ).filter(_._2.isDefined).view.mapValues(_.get).toMap
}

trait WithCoordinate {
  val coordinate: Coordinate
}

trait WithSymbol {
  val toSymbol: Char
}

sealed trait CardinalDirection
case object North extends CardinalDirection
case object South extends CardinalDirection
case object East extends CardinalDirection
case object West extends CardinalDirection
case object NorthEast extends CardinalDirection
case object NorthWest extends CardinalDirection
case object SouthEast extends CardinalDirection
case object SouthWest extends CardinalDirection

object CardinalDirection {
    def opposite(cardinalDir: CardinalDirection): CardinalDirection = 
        cardinalDir match {
            case North => South
            case East => West
            case South => North
            case West => East
            case NorthWest => SouthEast
            case NorthEast => SouthWest
            case SouthEast => NorthWest
            case SouthWest => NorthEast
        }
}
