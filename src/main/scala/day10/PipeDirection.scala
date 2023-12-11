package day10

sealed trait PipeDirection {
  val toSymbol: Char
  def moves(cood: Coordinate, maxX: Int, maxY: Int): List[Coordinate]
}
case class NS(toSymbol: Char = '|') extends PipeDirection {
  override def moves(
      coord: Coordinate,
      maxX: Int,
      maxY: Int
  ): List[Coordinate] =
    List(
      Coordinate.moveNorth(coord, maxX, maxY),
      Coordinate.moveSouth(coord, maxX, maxY)
    ).flatten
}
case class EW(toSymbol: Char = '-') extends PipeDirection {
  override def moves(
      coord: Coordinate,
      maxX: Int,
      maxY: Int
  ): List[Coordinate] =
    List(
      Coordinate.moveEast(coord, maxX, maxY),
      Coordinate.moveWest(coord, maxX, maxY)
    ).flatten
}
case class NE(toSymbol: Char = 'L') extends PipeDirection {
  override def moves(
      coord: Coordinate,
      maxX: Int,
      maxY: Int
  ): List[Coordinate] =
    List(
      Coordinate.moveNorth(coord, maxX, maxY),
      Coordinate.moveEast(coord, maxX, maxY)
    ).flatten

}
case class NW(toSymbol: Char = 'J') extends PipeDirection {
  override def moves(
      coord: Coordinate,
      maxX: Int,
      maxY: Int
  ): List[Coordinate] =
    List(
      Coordinate.moveNorth(coord, maxX, maxY),
      Coordinate.moveWest(coord, maxX, maxY)
    ).flatten
}
case class SW(toSymbol: Char = '7') extends PipeDirection {
  override def moves(
      coord: Coordinate,
      maxX: Int,
      maxY: Int
  ): List[Coordinate] =
    List(
      Coordinate.moveWest(coord, maxX, maxY),
      Coordinate.moveSouth(coord, maxX, maxY)
    ).flatten

}
case class SE(toSymbol: Char = 'F') extends PipeDirection {
  override def moves(
      coord: Coordinate,
      maxX: Int,
      maxY: Int
  ): List[Coordinate] =
    List(
      Coordinate.moveEast(coord, maxX, maxY),
      Coordinate.moveSouth(coord, maxX, maxY)
    ).flatten
}
case class Start(toSymbol: Char = 'S') extends PipeDirection {
  override def moves(
      coord: Coordinate,
      maxX: Int,
      maxY: Int
  ): List[Coordinate] =
    List.empty
}
case class Ground(toSymbol: Char = '.') extends PipeDirection {
  override def moves(
      coord: Coordinate,
      maxX: Int,
      maxY: Int
  ): List[Coordinate] =
    List.empty
}

object PipeDirection {
  def apply(c: Char): PipeDirection =
    c match {
      case '|' => NS()
      case '-' => EW()
      case 'L' => NE()
      case 'J' => NW()
      case '7' => SW()
      case 'F' => SE()
      case 'S' => Start()
      case '.' => Ground()
    }
}
