package day18

object MegaLagoon {

  def surface(orders: List[TrenchOrder]): BigInt = {

    val start = DirectedCoordinate(orders.head.direction, Coordinate(0, 0))
    def buildCorners(
        orders: List[TrenchOrder],
        acc: List[Coordinate],
        last: Coordinate
    ): List[Coordinate] = {
      orders match {
        case Nil => acc
        case o :: os =>
          val nextCoord = o.direction.jump(last, o.distance)
          buildCorners(os, acc :+ nextCoord, nextCoord)

      }
    }

    val corners = buildCorners(orders, List(Coordinate(0, 0)), start.coordinate)
    val s = corners.sliding(2, 1).foldLeft(BigInt(0)) { case (acc, l) =>
      acc + (BigInt(l(0).x) * BigInt(l(1).y) - BigInt(l(0).y) * BigInt(l(1).x))
    }
    val p = corners.sliding(2, 1).foldLeft(BigInt(0)) { case (acc, l) =>
      if (l(0).y == l(1).y) acc + BigInt(l(0).x - l(1).x).abs
      else acc + BigInt(l(0).y - l(1).y).abs
    } / 2
    s.abs / 2 + p + 1
  }
}
