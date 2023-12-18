package day18

final case class MegaLagoon(trenches: List[MegaTrench]) {
    val width = trenches.flatMap(t => List(t.end, t.start)).map(_.x).max
    val height = trenches.flatMap(t => List(t.end, t.start)).map(_.y).max
    val minWidth = trenches.flatMap(t => List(t.end, t.start)).map(_.x).min
    val minHeight = trenches.flatMap(t => List(t.end, t.start)).map(_.y).min

    val verticalOrdered = trenches.filter(t => t.dir == Up || t.dir == Down).sortBy(_.end.x)

}

object MegaLagoon {
    def fromOrders(orders: List[TrenchOrder]): MegaLagoon = {
        val startTrench = MegaTrench(Coordinate(0, 0), Coordinate(0,0), Up)
        def rec(start: Coordinate, orders: List[TrenchOrder], acc: List[MegaTrench]): List[MegaTrench] = {
            orders match {
                case Nil => acc
                case o :: os => 
                    val m = MegaTrench.fromOrder(start, o)
                    rec(m.end, os, acc :+ m)
            }
        }
       MegaLagoon(rec(Coordinate(0,0), orders, List(startTrench))) 
    }
}
