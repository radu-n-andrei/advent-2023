package day18

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
     override def jump(coord: Coordinate, steps: Int): Coordinate = coord.copy(y = coord.y - steps)
}
case object Down extends Direction {
    override def move(coord: Coordinate, steps: Int): List[Coordinate] = 
        (1 to steps).map(
            s => Coordinate(coord.x, coord.y + s)
        ).toList
    override val opposite: Direction = Up     
    override def jump(coord: Coordinate, steps: Int): Coordinate = coord.copy(y = coord.y + steps) 
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
