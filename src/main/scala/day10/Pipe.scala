package day10

final case class Pipe(coordinate: Coordinate, pipeDirection: PipeDirection)
    extends WithCoordinate
    with WithSymbol {
  override val toSymbol: Char = pipeDirection.toSymbol

  def connectedTowards(towards: CardinalDirection): Boolean =
    pipeDirection.connectsCardinal.contains(towards)

  def hasGapWith(other: Pipe, dir: CardinalDirection): Boolean = {
    (dir, pipeDirection, other.pipeDirection) match {
        // vertical
        case (East, NS(_), NS(_)) => true
        case (East, NS(_), SE(_)) => true
        case (East, NS(_), NE(_)) => true
        case (East, NW(_), NS(_)) => true
        case (East, NW(_), SE(_)) => true
        case (East, NW(_), NE(_)) => true
        case (East, SW(_), NS(_)) => true
        case (East, SW(_), NE(_)) => true
        case (East, SW(_), SE(_)) => true
        case (West, NS(_), NS(_)) => true
        case (West, SE(_), NS(_)) => true
        case (West, NE(_), NS(_)) => true
        case (West, NS(_), NW(_)) => true
        case (West, SE(_), NW(_)) => true
        case (West, NE(_), NW(_)) => true
        case (West, NS(_), SW(_)) => true
        case (West, NE(_), SW(_)) => true
        case (West, SE(_), SW(_)) => true
        // horizontal
        case (South, EW(_), EW(_)) => true
        case (South, EW(_), SE(_)) => true
        case (South, EW(_), SW(_)) => true
        case (South, NW(_), EW(_)) => true
        case (South, NW(_), SW(_)) => true
        case (South, NW(_), SE(_)) => true
        case (South, NE(_), EW(_)) => true
        case (South, NE(_), SW(_)) => true
        case (South, NE(_), SE(_)) => true
        case (North, EW(_), EW(_)) => true
        case (North, SE(_), EW(_)) => true
        case (North, SW(_), EW(_)) => true
        case (North, EW(_), NW(_)) => true
        case (North, SW(_), NW(_)) => true
        case (North, SE(_), NW(_)) => true
        case (North, EW(_), NE(_)) => true
        case (North, SW(_), NE(_)) => true
        case (North, SE(_), NE(_)) => true
        case _ => false
    }
    // connectedTowards(dir) && other.connectedTowards(
    //   CardinalDirection.opposite(dir)
    // ) 
  }

}

object Pipe {
  def apply(x: Int, y: Int, symbol: Char): Pipe =
    Pipe(Coordinate(x, y), PipeDirection(symbol))
}
