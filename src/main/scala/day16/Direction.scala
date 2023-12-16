package day16

sealed trait Direction {
    def next(coord: Coordinate): Coordinate
    val opposite: Direction
}
case object North extends Direction {
    override def next(coord: Coordinate): Coordinate = Coordinate(coord.x, coord.y - 1)
    val opposite: Direction = South
}
case object South extends Direction {
    override def next(coord: Coordinate): Coordinate = Coordinate(coord.x, coord.y + 1)
    val opposite: Direction = North
}
case object East extends Direction {
    override def next(coord: Coordinate): Coordinate = Coordinate(coord.x + 1, coord.y)
    val opposite: Direction = West
}
case object West extends Direction {
    override def next(coord: Coordinate): Coordinate = Coordinate(coord.x - 1, coord.y)
    val opposite: Direction = East
}