package day8

final case class DesertMap(directions: List[Direction], nodes: Map[String, DesertMapEntry]) {

    def move(s: String, position: Direction): String = 
        position match {
            case Left => nodes(s).left
            case Right => nodes(s).right
        }

    val directionLength: Int = directions.length
}

final case class DesertMapEntry(node: String, left: String, right: String)

final case class DesertPath(start: String, end: String, len: Long)

trait Direction

case object Right extends Direction

case object Left extends Direction

object Direction {
  def apply(c: Char): Direction = c.toLower match {
    case 'r' => Right
    case 'l' => Left
    case _   => throw new RuntimeException(s"Invalid direction: $c")
  }
}

final case class RouteEnd(endNode: String, len: Long)

final case class RouteStart(startNode: String, directionIndex: Int)
