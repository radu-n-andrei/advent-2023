package day24

import scala.annotation.tailrec

final case class Observation(
    hails: List[Hail],
    lowerBound: Double,
    upperBound: Double
) {

  def intersections: Long = {

    def singleIntersect(
        hailA: Hail, // 1
        hailB: Hail, // 3
        endA: Coordinate3d, // 2
        endB: Coordinate3d // 4
    ): Boolean = {
      val d: Double =
        ((hailA.startCoord.x - endA.x) * (hailB.startCoord.y - endB.y) - 
        (hailA.startCoord.y - endA.y) * (hailB.startCoord.x - endB.x))
      lazy val px: Double =
        (hailA.startCoord.x * endA.y - hailA.startCoord.y * endA.x) * (hailB.startCoord.x - endB.x) - 
        (hailA.startCoord.x - endA.x) * (hailB.startCoord.x * endB.y - hailB.startCoord.y * endB.x)
      lazy val py: Double =
        (hailA.startCoord.x * endA.y - hailA.startCoord.y * endA.x) * (hailB.startCoord.y - endB.y) - 
        (hailA.startCoord.y - endA.y) * (hailB.startCoord.x * endB.y - hailB.startCoord.y * endB.x)

      if (d == 0) false // parallel
      else {
        val x = px / d
        val y = py / d
        x >= lowerBound && x <= upperBound && y >= lowerBound && y <= upperBound && hailA
          .inTheFuture(x, y) && hailB.inTheFuture(x, y)
      }
    }

    @tailrec
    def singleHail(h: Hail, rest: List[Hail], total: Int): Int = {
      rest match {
        case Nil => total
        case r :: rs =>
          val hAtEnd = h.positionAtT(1)
          val rAtEnd = r.positionAtT(1)
          if (
            singleIntersect(
              h,
              r,
              hAtEnd,
              rAtEnd
            )
          ) {
            singleHail(h, rs, total + 1)
          } else {
            singleHail(h, rs, total)
          }
      }

    }

    @tailrec
    def simulate(hails: List[Hail], total: Long): Long =
      hails match {
        case Nil      => total
        case _ :: Nil => total
        case h :: hs => {
          val t = singleHail(h, hs, 0)
          // println(s"$h hits $t")
          simulate(hs, total + t)
        }
      }

    simulate(hails, 0)
  }

}
