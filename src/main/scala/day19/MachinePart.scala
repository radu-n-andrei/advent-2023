package day19

final case class MachinePart(x: Int, m: Int, a: Int, s: Int) {
  val score: Int = x + m + a + s
  def characteristic(p: PartCharacteristic): Int =
    p match {
      case X => x
      case M => m
      case A => a
      case S => s
    }
}

object MachinePart {
  def apply(str: String): MachinePart = {
    val pat = """\{x=([0-9]+),m=([0-9]+),a=([0-9]+),s=([0-9]+)\}""".r
    str match {
      case pat(x, m, a, s) => MachinePart(x.toInt, m.toInt, a.toInt, s.toInt)
      case _ => throw new RuntimeException(s"Illegal part description $str")
    }
  }
}

sealed trait PartCharacteristic
case object X extends PartCharacteristic
case object M extends PartCharacteristic
case object A extends PartCharacteristic
case object S extends PartCharacteristic

object PartCharacteristic {
  def apply(c: Char): PartCharacteristic = {
    c match {
      case 'x' => X
      case 'm' => M
      case 'a' => A
      case 's' => S
      case _   => throw new RuntimeException(s"Illegal part characteristic: $c")
    }
  }
}

case class Range(min: Int, max: Int) {

  /** Intersects this range with another
    * @param other
    *   the range to intersect with
    * @return
    *   the common range
    */
  def intersection(other: Range): Option[Range] = {
    val minCommon = Math.max(min, other.min)
    val maxCommon = Math.min(max, other.max)
    if (maxCommon < minCommon) None
    else Some(Range(minCommon, maxCommon))
  }

  def diff(other: Range): List[Range] =
    intersection(other).fold(List(Range(min, max))) { i =>
      List(Range(min, i.min - 1), Range(i.max + 1, max)).filterNot(r => r.min >= r.max)

    }

}

case class PartPossibilities(parts: Map[PartCharacteristic, Range]) {
  def updateWith(part: PartCharacteristic, range: Range): PartPossibilities = {
    PartPossibilities(parts + (part -> range))
  }

  def total: BigInt = {
    parts.values.map(r => BigInt(r.max - r.min + 1)).product
  }
}
object PartPossibilities {
  val initial: PartPossibilities = PartPossibilities(Map(
    X -> Range(1, 4000),
    M -> Range(1, 4000),
    A -> Range(1, 4000),
    S -> Range(1, 4000)
  ))
}
