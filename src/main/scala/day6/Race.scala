package day6

final case class Race(time: Long, distance: Long) {

  // min speed is s * (t - s) = d + 1
  // -s^2 + s*t - d - 1 = 0
  // delta = time^2 - 4 * (d + 1)
  val minSpeedToWin: Long = {
    val delta = time * time - 4 * distance - 4
    val sqrtDelta = Math.sqrt(delta.toDouble)
    // taking the positive solution only
    val s1 = (-time.toDouble + sqrtDelta) / -2
    Math.ceil(s1).toLong
  }

  val maxTimeToLose: Long = {
    (time - 1 to 1L by -1)
      .collectFirst {
        case speed if speed * (time - speed) > distance => speed
      }
      .getOrElse(0L)
  }

  val winPossibilities: Int = {
    (maxTimeToLose - minSpeedToWin + 1).toInt
  }
}

object Race {
  def apply(in: (String, String)): Race = Race(in._1.toLong, in._2.toLong)
}
