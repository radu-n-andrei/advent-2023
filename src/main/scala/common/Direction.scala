package common

sealed trait Direction {
    def move(coord: Coordinate, steps: Int): List[Coordinate]
    def moveOne(coord: Coordinate): Coordinate = move(coord, 1).head
    def jump(coord: Coordinate, steps: Int): Coordinate
    val opposite: Direction
}
case object East extends Direction {
    override def move(coord: Coordinate, steps: Int): List[Coordinate] = 
        (1 to steps).map(
            s => Coordinate(coord.x + s, coord.y)
        ).toList

    override val opposite: Direction = West   
    override def jump(coord: Coordinate, steps: Int): Coordinate = coord.copy(x = coord.x + steps) 
}
case object West extends Direction {
    override def move(coord: Coordinate, steps: Int): List[Coordinate] = 
        (1 to steps).map(
            s => Coordinate(coord.x - s, coord.y)
        ).toList
        override val opposite: Direction = East 
        override def jump(coord: Coordinate, steps: Int): Coordinate = coord.copy(x = coord.x - steps)
}
case object North extends Direction {
    override def move(coord: Coordinate, steps: Int): List[Coordinate] = 
        (1 to steps).map(
            s => Coordinate(coord.x, coord.y - s)
        ).toList

     override val opposite: Direction = South   
     override def jump(coord: Coordinate, steps: Int): Coordinate = coord.copy(y = coord.y - steps)
}
case object South extends Direction {
    override def move(coord: Coordinate, steps: Int): List[Coordinate] = 
        (1 to steps).map(
            s => Coordinate(coord.x, coord.y + s)
        ).toList
    override val opposite: Direction = North     
    override def jump(coord: Coordinate, steps: Int): Coordinate = coord.copy(y = coord.y + steps) 
}

object Direction {
    def apply(c: Char): Direction = 
        c match {
            case 'N' => North
            case 'S' => South
            case 'E'=> East
            case 'W' => West
            case _ => throw new RuntimeException(s"Illegal direction $c")
        }

    val all = List(South, North, East, West)    
}
