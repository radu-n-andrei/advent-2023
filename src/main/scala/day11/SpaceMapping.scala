package day11

sealed abstract class SpaceMapping(val x: Int, val y: Int, val symbol: Char) 

case class Galaxy(override val x: Int, override val y: Int) extends SpaceMapping(x, y, '#')
case class EmptySpace(override val x: Int, override val y: Int) extends SpaceMapping(x, y, '.')

object SpaceMapping {
    def apply(c: Char, x: Int, y: Int): SpaceMapping = 
        if(c == '#')
            Galaxy(x, y)
        else 
            EmptySpace(x, y)    
}
