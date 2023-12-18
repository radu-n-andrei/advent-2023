package day18

final case class TrenchOrder(
    distance: Int,
    direction: Direction,
    color: String
) {
  def decode: TrenchOrder = {
    val trueDistance = Integer.parseInt(color.tail.take(5), 16)
    val dir = Direction(color.last)
    println(s"$dir $trueDistance")
    TrenchOrder(trueDistance, dir, "")
  }
}

object TrenchOrder {
  def apply(in: String): TrenchOrder = {
    val pat = """([R|L|U|D]+)\s([0-9]+)\s\((.{7})\)""".r
    in match {
      case pat(d, n, c) => TrenchOrder(n.toInt, Direction(d.head), c)
      case _ => throw new RuntimeException(s"Illegal trench order $in")
    }
  }
}
