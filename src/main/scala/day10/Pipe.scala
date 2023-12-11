package day10

final case class Pipe(coordinate: Coordinate, pipeDirection: PipeDirection)
    extends WithCoordinate
    with WithSymbol {
  override val toSymbol: Char = pipeDirection.toSymbol

  def connectedTowards(towards: CardinalDirection): Boolean =
    pipeDirection.connectsCardinal.contains(towards)

  def connectedWith(other: Pipe, dir: CardinalDirection): Boolean = {
    connectedTowards(dir) && other.connectedTowards(
      CardinalDirection.opposite(dir)
    ) 
  }

}

object Pipe {
  def apply(x: Int, y: Int, symbol: Char): Pipe =
    Pipe(Coordinate(x, y), PipeDirection(symbol))
}
