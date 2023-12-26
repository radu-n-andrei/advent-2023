package day24

final case class Hail(startCoord: Coordinate3d, velocity: Velocity) {

  def timeInBounds2D(
      lowerBound: Double,
      upperBound: Double
  ): Option[TimeInterval] = {
    val (minTx, maxTx) =
      timeInBounds2D(startCoord.x, velocity.vx, lowerBound, upperBound)
    val (minTy, maxTy) =
      timeInBounds2D(startCoord.y, velocity.vy, lowerBound, upperBound)
    val minT = Math.max(minTx, minTy)
    val maxT = Math.min(maxTx, maxTy)
    if (maxT < minT) None
    else Some(TimeInterval(minT, maxT))

  }

  private def timeInBounds2D(
      startPosition: Double,
      velocity: Double,
      lowerBound: Double,
      upperBound: Double
  ): (Double, Double) = {
    val alreadyIn = startPosition >= lowerBound && startPosition <= upperBound
    val t1 = (lowerBound - startPosition) / velocity
    val t2 = (upperBound - startPosition) / velocity
    if (alreadyIn) {
      (0, Math.max(t1, t2))
    } else if (t1 < t2) (t1, t2)
    else (t2, t1)
  }

  def positionAtT(time: Double): Coordinate3d =
    Coordinate3d(
      startCoord.x + time * velocity.vx,
      startCoord.y + time * velocity.vy,
      startCoord.z + time * velocity.vz
    )

  def edgesAtBounds2D(
      lowerBound: Long,
      upperBound: Long
  ): Option[(Coordinate3d, Coordinate3d)] = 
    timeInBounds2D(lowerBound, upperBound).map { interval =>
      (positionAtT(interval.start), positionAtT(interval.end))
    }

  def inTheFuture(fx: Double, fy: Double): Boolean = {
    val deltaX = fx - startCoord.x 
    val deltaY = fy - startCoord.y
    Math.signum(deltaX) == Math.signum(velocity.vx) && Math.signum(deltaY) == Math.signum(velocity.vy)
  }  

}

object Hail {
  def apply(s: String): Hail = {
    val pat =
      """([0-9]+),\s([0-9]+),\s([0-9]+)\s@\s+(-*[0-9]+),\s+(-*[0-9]+),\s+(-*[0-9]+)""".r
    s match {
      case pat(x, y, z, vx, vy, vz) =>
        Hail(
          Coordinate3d(x.toLong.toDouble, y.toLong.toDouble, z.toLong.toDouble),
          Velocity(
            Velocity.fromString(vx),
            Velocity.fromString(vy),
            Velocity.fromString(vz)
          )
        )
      case _ => throw new RuntimeException(s"Illegal hail definition: $s")
    }
  }
}
