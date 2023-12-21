package common

final case class Coordinate(x: Int, y: Int) {
    def allMoves(): List[Coordinate] = 
        List(North, South, East, West).flatMap(dir => dir.move(this, 1))
}
final case class DirectedCoordinate(direction: Direction, coordinate: Coordinate)
