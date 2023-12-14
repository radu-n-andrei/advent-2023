package day14

final case class Platform(rows: List[List[Rock]]) {

  val width: Int = rows.head.length
  val height: Int = rows.length

  def print: Unit =
    rows.foreach(r => println(r.map(_.symbol).mkString))

  def score: Int = {
    rows.zipWithIndex.map { case (rocks, index) =>
      rocks.filter {
        case RoundRock => true
        case _         => false
      }.length * (height - index)
    }.sum
  }

  def tiltNorth: Platform = {

    def tilt(
        rocks: List[List[Rock]],
        acc: List[List[Rock]]
    ): List[List[Rock]] = {
      if (rocks.head.isEmpty) acc
      else {
        val revColumn = rocks.map(_.head)
        val shifted = shiftRow(revColumn, List.empty).reverse
        val newAcc = acc.zip(shifted).map(x => x._1 :+ x._2)
        tilt(rocks.map(_.tail), newAcc)
      }
    }
    Platform(tilt(rows, List.fill(width)(List.empty)))
  }

  private def rotateRight: Platform = {
    def rotate(
        rocks: List[List[Rock]],
        acc: List[List[Rock]]
    ): List[List[Rock]] = {
      rocks match {
        case Nil     => acc
        case r :: rs => rotate(rs, acc.zip(r).map(x => x._2 +: x._1))
      }
    }
    Platform(rotate(rows, List.fill(width)(List.empty)))
  }

  def tiltRowsRight(
      rocks: List[List[Rock]],
      acc: List[List[Rock]]
  ): List[List[Rock]] = {
    rocks match {
      case Nil => acc
      case r :: rs =>
        val shifted = shiftRow(r.reverse, List.empty)
        tiltRowsRight(rs, acc :+ shifted)
    }
  }

  private def spin: Platform = {
    // to the north
    val north = Platform(tiltRowsRight(rotateRight.rows, List.empty))
    val west = Platform(tiltRowsRight(north.rotateRight.rows, List.empty))
    val south = Platform(tiltRowsRight(west.rotateRight.rows, List.empty))
    Platform(tiltRowsRight(south.rotateRight.rows, List.empty))
  }

  def cycle(times: Int): Platform = {

    // try to find repeating patterns
    // (list_of_platforms_before_cycle, list_of_platform_in_cycle)
    def cycleWithBuff(
        times: Int,
        platform: Platform,
        cache: List[Platform]
    ): (List[Platform], List[Platform]) = {
      if (times == 0) (cache, List.empty)
      else {
        if (cache.contains(platform)) {
          val i = cache.indexOf(platform)
          if (i == 0) (List.empty, cache)
          else
            cache.splitAt(i)
        } else {
          val s = platform.spin
          cycleWithBuff(times - 1, s, cache :+ platform)
        }
      }
    }
    val pattern = cycleWithBuff(times - 1, spin, List.empty)
    if (pattern._2.nonEmpty)
      pattern._2(((times - pattern._1.length - 1) % pattern._2.length)) // cycle detected, calculate the index of the solution
    else pattern._1.last // unlucky, get the last platform
  }

  def moveRock(r: Rock, into: List[Rock], acc: List[Rock]): List[Rock] =
    into match {
      case Nil          => acc :+ r
      case NoRock :: rs => moveRock(r, rs, NoRock +: acc)
      case _ :: rs      => (acc :+ r) ++ into
    }

  def shiftRow(reversedRocks: List[Rock], acc: List[Rock]): List[Rock] = {
    reversedRocks match {
      case Nil => acc
      case rock :: rocks =>
        rock match {
          case RoundRock => shiftRow(rocks, moveRock(rock, acc, List.empty))
          case _         => shiftRow(rocks, rock +: acc)
        }
    }
  }
}
