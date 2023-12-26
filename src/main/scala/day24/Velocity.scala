package day24

final case class Velocity(vx: Double, vy: Double, vz: Double) {

    val xyIncline: Double = vx / vy
}

object Velocity {
    def fromString(s: String): Double = 
        if(s.startsWith("-")) -1 * s.drop(1).toLong.toDouble else s.toLong.toDouble
}

case class TimeInterval(start: Double, end: Double) {

    def intersection(other: TimeInterval): Option[TimeInterval] = {
        val maxStart = Math.max(start, other.start)
        val minEnd = Math.min(end, other.end)
        if(minEnd < maxStart) None
        else Some(TimeInterval(maxStart, minEnd))
    }
}