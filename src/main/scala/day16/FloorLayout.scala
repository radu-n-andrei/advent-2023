package day16

import scala.annotation.tailrec

final case class FloorLayout(tiles: List[List[Tile]]) {

  val width = tiles.headOption.fold(0)(_.length)
  val height = tiles.length

  val tilesAsMap =
    tiles.zipWithIndex.flatMap { case (l, y) =>
      l.zipWithIndex.map { case (t, x) =>
        (Coordinate(x, y) -> t)
      }
    }.toMap

  def print: Unit =
    tiles.foreach(t => println(t.map(_.symbol).mkString))

  @tailrec
  def enterTileWithShortcuts(
      coordinates: List[(Coordinate, Direction)],
      acc: List[(Coordinate, Direction)],
      shortcuts: Map[(Coordinate, Direction), List[(Coordinate, Direction)]]
  ): (
      Int,
      Map[(Coordinate, Direction), List[(Coordinate, Direction)]]
  ) =
    coordinates match {
      case Nil => (acc.map(_._1).distinct.length, shortcuts)
      case c :: coords if acc.contains(c) =>
        enterTileWithShortcuts(coords, acc, shortcuts)
      case (coord, direction) :: coords =>
        val nextDirs = tilesAsMap(coord).energize(direction)
        val nextTileCoords =
          nextDirs.map(d => (d.next(coord), d.opposite)).filter {
            case (newCoord, _) =>
              newCoord.x >= 0 && newCoord.x < width && newCoord.y >= 0 && newCoord.y < height
          }
        if (nextTileCoords.isEmpty) {
          enterTileWithShortcuts(
            coords,
            (coord, direction) +: acc,
            shortcuts + ((coord, direction) -> List((coord, direction)))
          )
        } else if (nextTileCoords.forall(shortcuts.isDefinedAt)) {
          val short = nextTileCoords.flatMap(shortcuts(_))
          val currentEnerg = 1 + short.map(_._1).distinct.length
          enterTileWithShortcuts(
            coords,
            ((coord, direction) +: short) ++ acc,
            shortcuts + ((coord, direction) -> ((coord, direction) +: short))
          )
        } else {
          val short = nextTileCoords.flatMap(shortcuts.get).flatten.distinct
          val passed = (coord, direction) +: short
          val next = nextTileCoords.filterNot(shortcuts.isDefinedAt)
          enterTileWithShortcuts(
            next ++ coords,
            passed ++ acc,
            shortcuts
          )
        }
    }

  def maxEnergy: Int = {
    val northBound = (0 until width).map(i => (Coordinate(i, 0), North))
    val southBound =
      (0 until width).map(i => (Coordinate(i, height - 1), South))
    val eastBound = (0 until height).map(i => (Coordinate(width - 1, i), East))
    val westBound = (0 until height).map(i => (Coordinate(0, i), West))

    // map!
    val allBounds = northBound ++ southBound ++ eastBound ++ westBound
    allBounds
      .foldLeft(
        (
          0,
          Map.empty[(Coordinate, Direction), List[(Coordinate, Direction)]]
        )
      ) { case ((acc, shortcuts), start) =>
        //println(s"Processing $start")
        val (energ, shorts) =
          enterTileWithShortcuts(
            List(start),
            List.empty,
            shortcuts
          )
        (Math.max(acc, energ), shorts)
      }
      ._1
  }

}
