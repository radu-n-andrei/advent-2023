package day10

sealed abstract class Enclosure(
    override val coordinate: Coordinate,
    override val toSymbol: Char,
    val isDefind: Boolean
) extends WithCoordinate
    with WithSymbol

case class Outside(coord: Coordinate) extends Enclosure(coord, 'O', true)
case class Undecided(coord: Coordinate) extends Enclosure(coord, '?', false) {
  val turnOutside: Outside = Outside(coord)
}
case class ConnectedPipe(pipe: Pipe)
    extends Enclosure(pipe.coordinate, pipe.pipeDirection.toSymbol, true)
