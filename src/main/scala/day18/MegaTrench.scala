package day18

final case class MegaTrench(start: Coordinate, end: Coordinate, dir: Direction)

object MegaTrench {
  def fromOrder(digFrom: Coordinate, trenchOrder: TrenchOrder): MegaTrench = {
    MegaTrench(
      trenchOrder.direction.jump(digFrom, 1),
      trenchOrder.direction.jump(digFrom, trenchOrder.distance),
      trenchOrder.direction
    )
  }
}
