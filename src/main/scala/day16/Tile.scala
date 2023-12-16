package day16

sealed abstract class Tile(val symbol: Char) {
    def energize(direction: Direction): List[Direction]
}


case object EmptySpace extends Tile('.') {
    override def energize(direction: Direction): List[Direction] = List(direction.opposite)
}
case object VerticalSplitter extends Tile('|') {
    override def energize(direction: Direction): List[Direction] = 
        direction match {
            case North | South => List(direction.opposite)
            case East | West =>  List(North, South)
        }
}
case object HorizontalSplitter extends Tile('-') {
    override def energize(direction: Direction): List[Direction] = 
        direction match {
            case North | South => List(East, West)
            case East | West =>  List(direction.opposite)
        }
}
case object RightTiltedMirror extends Tile('/') {
    override def energize(direction: Direction): List[Direction] = 
        direction match {
            case West => List(North)
            case East => List(South)
            case North => List(West)
            case South => List(East)
        }
}
case object LeftTiltedMirror extends Tile('\\') {
    override def energize(direction: Direction): List[Direction] = 
        direction match {
            case West => List(South)
            case East => List(North)
            case North => List(East)
            case South => List(West)
        }
}

object Tile {
    def apply(c: Char): Tile = 
        c match {
            case VerticalSplitter.symbol => VerticalSplitter
            case HorizontalSplitter.symbol => HorizontalSplitter
            case RightTiltedMirror.symbol => RightTiltedMirror
            case LeftTiltedMirror.symbol => LeftTiltedMirror
            case _ => EmptySpace
        }
}

