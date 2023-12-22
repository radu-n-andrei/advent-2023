package day22

final case class Brick(end1: BrickCoordinate, end2: BrickCoordinate) {
  val lowestPoint: Int = Math.min(end1.z, end2.z)
  val highestPoint: Int = Math.max(end1.z, end2.z)

  def arrangedBy(cond: => Boolean): Brick =
    if (cond) Brick(end1, end2)
    else Brick(end2, end1)

  def moveByZ(z: Int) =
    Brick(end1.copy(z = end1.z - z), end2.copy(z = end2.z - z))

  def overlapsOn(other: Brick)(f: BrickCoordinate => Int): Boolean = {
    val ar = arrangedBy(f(end1) <= f(end2))
    val oar = other.arrangedBy(f(other.end1) <= f(other.end2))
    val overlapStart = Math.max(f(ar.end1), f(oar.end1))
    val overlapEnd = Math.min(f(ar.end2), f(oar.end2))
    overlapStart <= overlapEnd
  }

  def canSupport(other: Brick): Boolean = {
    val overlap = overlapsOn(other) _
    overlap(_.x) && overlap(_.y)
  }
}

object Brick {
  // ascending by the lowest Z
  implicit val ordering: Ordering[Brick] = (b1, b2) =>
    b1.lowestPoint - b2.lowestPoint
  // descending by the highest Z
  val reverseOrder: Ordering[Brick] = (b1, b2) =>
    b2.highestPoint - b1.highestPoint
  // descending by the lowest Z
  val lowZorderingD: Ordering[Brick] = (b1, b2) =>
    b2.lowestPoint - b1.lowestPoint

  def apply(s: String): Brick = {
    val pat = "(.*)~(.*)".r
    s match {
      case pat(e1, e2) => Brick(BrickCoordinate(e1), BrickCoordinate(e2))
      case _           => throw new RuntimeException(s"Illegall brick $s")
    }
  }
}

final case class BrickCoordinate(x: Int, y: Int, z: Int)

object BrickCoordinate {
  def apply(s: String): BrickCoordinate = {
    val pat = "([0-9]+),([0-9]+),([0-9]+)".r
    s match {
      case pat(x, y, z) => BrickCoordinate(x.toInt, y.toInt, z.toInt)
      case _ => throw new RuntimeException(s"Illegal brick coordinate: $s")
    }
  }

}

final case class SupportingBrick(brick: Brick, supports: List[Brick])
