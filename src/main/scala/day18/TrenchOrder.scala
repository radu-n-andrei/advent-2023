package day18

final case class TrenchOrder(distance: Int, direction: Direction, color: String) {
    def decode: TrenchOrder = {
        val trueDistance = Integer.parseInt(color.tail.take(5), 16)
        val dir = Direction(color.last)
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

sealed trait Direction {
    def move(coord: Coordinate, steps: Int): List[Coordinate]
    def jump(coord: Coordinate, steps: Int): Coordinate
    val opposite: Direction
}
case object Right extends Direction {
    override def move(coord: Coordinate, steps: Int): List[Coordinate] = 
        (1 to steps).map(
            s => Coordinate(coord.x + s, coord.y)
        ).toList

    override val opposite: Direction = Left   
    override def jump(coord: Coordinate, steps: Int): Coordinate = coord.copy(x = coord.x + steps) 
}
case object Left extends Direction {
    override def move(coord: Coordinate, steps: Int): List[Coordinate] = 
        (1 to steps).map(
            s => Coordinate(coord.x - s, coord.y)
        ).toList
        override val opposite: Direction = Right 
        override def jump(coord: Coordinate, steps: Int): Coordinate = coord.copy(x = coord.x - steps)
}
case object Up extends Direction {
    override def move(coord: Coordinate, steps: Int): List[Coordinate] = 
        (1 to steps).map(
            s => Coordinate(coord.x, coord.y - s)
        ).toList

     override val opposite: Direction = Down   
     override def jump(coord: Coordinate, steps: Int): Coordinate = coord.copy(y = coord.y + steps)
}
case object Down extends Direction {
    override def move(coord: Coordinate, steps: Int): List[Coordinate] = 
        (1 to steps).map(
            s => Coordinate(coord.x, coord.y + s)
        ).toList
    override val opposite: Direction = Up     
    override def jump(coord: Coordinate, steps: Int): Coordinate = coord.copy(coord.y - steps) 
}

object Direction {
    def apply(c: Char): Direction = 
        c match {
            case 'R' | '0' => Right
            case 'L' | '2' => Left
            case 'U' | '3'=> Up
            case 'D' | '1' => Down
            case _ => throw new RuntimeException(s"Illegal direction $c")
        }
}