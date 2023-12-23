package day23

import common.Direction
import common.North
import common.South
import common.East
import common.West
import common.Coordinate

sealed trait Tile {
  val symbol: Char
  def moves(coord: Coordinate): Map[Direction, Coordinate]
  def reachableFrom(directon: Direction): Boolean
}

sealed trait WalkableTile extends Tile

case object Path extends WalkableTile {
  override val symbol: Char = '.'
  override def moves(coord: Coordinate): Map[Direction, Coordinate] =
    Direction.all.map(d => d -> d.moveOne(coord)).toMap
    override def reachableFrom(directon: Direction): Boolean = true
}
case object Forrest extends Tile {
  override val symbol: Char = '#'
  override def moves(coord: Coordinate):  Map[Direction, Coordinate] = Map.empty
  override def reachableFrom(directon: Direction): Boolean = false
}

case class Slope(direction: Direction) extends WalkableTile {
  override val symbol: Char = direction match {
    case North => '^'
    case South => 'v'
    case East  => '>'
    case West  => '<'
  }
  override def moves(coord: Coordinate): Map[Direction, Coordinate] = Map(
    direction -> direction.moveOne(coord)
  )
  override def reachableFrom(dir: Direction): Boolean = dir != direction.opposite
}

object Tile {
  def apply(c: Char): Tile = {
    c match {
      case '.' => Path
      case '#' => Forrest
      case '>' => Slope(East)
      case '<' => Slope(West)
      case 'v' => Slope(South)
      case '^' => Slope(North)
    }
  }

  def walkable(t: Tile): Boolean = 
    t match {
        case _: WalkableTile => true
        case _ => false
    }
}
