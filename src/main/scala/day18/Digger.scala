package day18

/** Inefficient for bigger inputs, but i like it
  */
object Digger {

  def dig(orders: List[TrenchOrder]): List[DirectedCoordinate] = {
    val start = DirectedCoordinate(orders.head.direction, Coordinate(0, 0))
    def keepDigging(
        current: Coordinate,
        orders: List[TrenchOrder],
        acc: List[DirectedCoordinate]
    ): List[DirectedCoordinate] = {
      orders match {
        case Nil => acc
        case o :: os =>
          val trench = o.direction
            .move(current, o.distance)
            .map(c => DirectedCoordinate(o.direction, c))
          keepDigging(trench.last.coordinate, os, acc ++ trench)
      }
    }

    normalizeTrench(keepDigging(start.coordinate, orders, List(start)))
  }

  def normalizeTrench(
      trench: List[DirectedCoordinate]
  ): List[DirectedCoordinate] = {
    val minX = trench.map(t => t.coordinate.x).min
    val minY = trench.map(t => t.coordinate.y).min
    val xF = if (minX < 0) minX else 0
    val yF = if (minY < 0) minY else 0
    if (xF != 0 || yF != 0)
      trench.map(c =>
        c.copy(coordinate =
          Coordinate(c.coordinate.x - xF, c.coordinate.y - yF)
        )
      )
    else trench
  }
}

case class Coordinate(x: Int, y: Int)
case class DirectedCoordinate(direction: Direction, coordinate: Coordinate)
