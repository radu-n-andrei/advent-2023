package day24

import scala.annotation.tailrec

final case class Observation(
    hails: List[Hail],
    lowerBound: Double,
    upperBound: Double
) {

  def intersections: Long = {

    def singleIntersect(
        startA: Coordinate3d, // 1
        endA: Coordinate3d, // 2
        startB: Coordinate3d, // 3
        endB: Coordinate3d // 4
    ): Boolean = {
      val t: Double =
        ((startA.x - startB.x) * (startB.y - endB.y) - (startA.y - startB.y) * (startB.x - endB.x)) /
          ((startA.x - endA.x) * (startB.y - endB.y) - (startA.y - endA.y) * (startB.x - endB.x))
      val u: Double =
        ((startA.x - startB.x) * (startA.y - endA.y) - (startA.y - startB.y) * (startA.x - endA.x)) /
          ((startA.x - endA.x) * (startB.y - endB.y) - (startA.y - endA.y) * (startB.x - endB.x))

      t >= 0 && t <= 1 && u >= 0 && u <= 1
    }

    val intervalMap =
      hails.map(h => h -> h.timeInBounds2D(lowerBound, upperBound)).toMap

    @tailrec
    def singleHail(h: Hail, rest: List[Hail], total: Int): Int = {
      rest match {
        case Nil                               => total
        case r :: rs if intervalMap(r).isEmpty => singleHail(h, rs, total)
        case r :: rs =>
          val timeIntersection =
            intervalMap(h).get.intersection(intervalMap(r).get)
          if (timeIntersection.isEmpty) {
            singleHail(h, rs, total)
          }
          else {
            val hAtStart = h.positionAtT(timeIntersection.get.start)
            val hAtEnd = h.positionAtT(timeIntersection.get.end)
            val rAtStart = r.positionAtT(timeIntersection.get.start)
            val rAtEnd = r.positionAtT(timeIntersection.get.end)
            if (
                h.velocity.xyIncline != r.velocity.xyIncline && 
              singleIntersect(
                hAtStart,
                hAtEnd,
                rAtStart,
                rAtEnd
              )
            ) {
              singleHail(h, rs, total + 1)
            } else {
              singleHail(h, rs, total)
            }
          }
      }
    }

    @tailrec
    def simulate(hails: List[Hail], total: Long): Long =
      hails match {
        case Nil      => total
        case _ :: Nil => total
        case h :: hs if intervalMap(h).isEmpty => simulate(hs, total)
        case h :: hs => {
          val t = singleHail(h, hs, 0)
          //println(s"$h hits $t")
          simulate(hs, total + t)
        }
      }

    simulate(hails, 0)
  }

}
