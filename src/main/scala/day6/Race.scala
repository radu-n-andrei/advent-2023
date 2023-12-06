package day6

final case class Race(time: Long, distance: Long) {

  /** Solutions to speed * (time - speed) = distance - 1 (=> speed^2 +
    * speed*time - distance + 1) will give out the minimum and maximum speeds to
    * fail. Applying ciel to the min and floor the max will produce the min and
    * max speeds to win.
    */
  val winPossibilities: Long = {
    val delta = time * time - 4 * distance + 4
    val sqrtDelta = Math.sqrt(delta.toDouble)
    val s1 = (-time.toDouble + sqrtDelta) / -2
    val s2 = (-time.toDouble - sqrtDelta) / -2
    Math.floor(s2).toLong - Math.ceil(s1).toLong + 1
  }
}

object Race {
  def apply(in: (String, String)): Race = Race(in._1.toLong, in._2.toLong)
}
