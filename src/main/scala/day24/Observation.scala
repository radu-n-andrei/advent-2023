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
      lazy val t =
        (hailA.startCoord.x - hailB.startCoord.x) * (hailB.startCoord.y - endB.y) -
          (hailA.startCoord.y - hailB.startCoord.y) * (hailB.startCoord.x - endB.x)
      lazy val u =
        (hailA.startCoord.x - hailB.startCoord.x) * (hailA.startCoord.y - endA.y) -
          (hailA.startCoord.y - hailB.startCoord.y) * (hailA.startCoord.x - endA.x)
      lazy val px: Double =
        (hailA.startCoord.x * endA.y - hailA.startCoord.y * endA.x) * (hailB.startCoord.x - endB.x) -
          (hailA.startCoord.x - endA.x) * (hailB.startCoord.x * endB.y - hailB.startCoord.y * endB.x)
      lazy val py: Double =
        (hailA.startCoord.x * endA.y - hailA.startCoord.y * endA.x) * (hailB.startCoord.y - endB.y) -
          (hailA.startCoord.y - endA.y) * (hailB.startCoord.x * endB.y - hailB.startCoord.y * endB.x)

      if (d == 0) {
        false
      } // parallel
      else {
        if (t / d < 0 || u / d < 0) false
        else {
          val t0 = t/d
          val x = hailA.startCoord.x + t0 * (endA.x - hailA.startCoord.x)
          val y = hailA.startCoord.y + t0 * (endA.y - hailA.startCoord.y)
          x >= lowerBound && x <= upperBound && y >= lowerBound && y <= upperBound
        }
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
