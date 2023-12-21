package day21

sealed trait Tile {
  val symbol: Char
}

sealed trait WalkableTile extends Tile
case object Ground extends WalkableTile {
  override val symbol: Char = '.'
}
case object Rock extends Tile {
  override val symbol: Char = '#'
}
case object Start extends WalkableTile {
  override val symbol: Char = 'S'
}

object Tile {
  def apply(c: Char): Tile =
    c match {
      case '.' => Ground
      case '#' => Rock
      case 'S' => Start
      case _   => throw new RuntimeException(s"Illegal space $c")
    }
}
