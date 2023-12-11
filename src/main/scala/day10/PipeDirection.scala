package day10



sealed trait PipeDirection {
    val toSymbol: Char
    val connectsCardinal: List[CardinalDirection]
    def moves(cood: Coordinate, maxX: Int, maxY: Int): List[Coordinate]
}
case class NS(toSymbol: Char = '|') extends PipeDirection {
    override def moves(coord: Coordinate, maxX: Int, maxY: Int): List[Coordinate] = 
        List(Coordinate.moveNorth(coord, maxX, maxY), Coordinate.moveSouth(coord, maxX, maxY)).flatten
    override val connectsCardinal: List[CardinalDirection] = List(North, South)    
}
case class EW(toSymbol: Char = '-') extends PipeDirection {
    override def moves(coord: Coordinate, maxX: Int, maxY: Int): List[Coordinate] = 
        List(Coordinate.moveEast(coord, maxX, maxY), Coordinate.moveWest(coord, maxX, maxY)).flatten  
    override val connectsCardinal: List[CardinalDirection] = List(East, West)    
}
case class NE(toSymbol: Char = 'L') extends PipeDirection {
    override def moves(coord: Coordinate, maxX: Int, maxY: Int): List[Coordinate] = 
        List(Coordinate.moveNorth(coord, maxX, maxY), Coordinate.moveEast(coord, maxX, maxY)).flatten
    override val connectsCardinal: List[CardinalDirection] = List(North, East)    
 
}
case class NW(toSymbol: Char = 'J') extends PipeDirection {
    override def moves(coord: Coordinate, maxX: Int, maxY: Int): List[Coordinate] = 
        List(Coordinate.moveNorth(coord, maxX, maxY), Coordinate.moveWest(coord, maxX, maxY)).flatten
    override val connectsCardinal: List[CardinalDirection] = List(North, West)
}
case class SW(toSymbol: Char = '7') extends PipeDirection {
    override def moves(coord: Coordinate, maxX: Int, maxY: Int): List[Coordinate] = 
        List(Coordinate.moveWest(coord, maxX, maxY), Coordinate.moveSouth(coord, maxX, maxY)).flatten

    override val connectsCardinal: List[CardinalDirection] = List(West, South)
}
case class SE(toSymbol: Char = 'F') extends PipeDirection {
    override def moves(coord: Coordinate, maxX: Int, maxY: Int): List[Coordinate] = 
        List(Coordinate.moveEast(coord, maxX, maxY), Coordinate.moveSouth(coord, maxX, maxY)).flatten
    override val connectsCardinal: List[CardinalDirection] = List(South, East)
}
case class Start(toSymbol: Char = 'S') extends PipeDirection {
    override def moves(coord: Coordinate, maxX: Int, maxY: Int): List[Coordinate] = 
        List.empty
    override val connectsCardinal: List[CardinalDirection] = List.empty
}
case class Ground(toSymbol: Char = '.') extends PipeDirection {
    override def moves(coord: Coordinate, maxX: Int, maxY: Int): List[Coordinate] = 
        List.empty
    override val connectsCardinal: List[CardinalDirection] = List.empty    
}

object PipeDirection {
  def apply(c: Char): PipeDirection =
    c match {
      case '|' => NS()
      case '-' => EW()
      case 'L' => NE()
      case 'J' => NW()
      case '7' => SW()
      case 'F' => SE()
      case 'S' => Start()
      case '.' => Ground()
    }
}

