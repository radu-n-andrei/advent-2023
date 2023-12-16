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
      coordinates: List[DirectedTile],
      acc: List[DirectedTile],
      shortcuts: TileCache,
      exits: List[Coordinate],
      split: Boolean
  ): (
      Int,
      TileCache,
      List[Coordinate]
  ) =
    coordinates match {
      case Nil if split =>
        (acc.map(_.coord).distinct.length, shortcuts, List.empty)
      case Nil => (acc.map(_.coord).distinct.length, shortcuts, exits)
      case c :: coords if acc.contains(c) =>
        enterTileWithShortcuts(coords, acc, shortcuts, exits, split)
      case dirTile :: dirTiles =>
        val nextDirs = tilesAsMap(dirTile.coord).energize(dirTile.dir)
        val ttt = tilesAsMap(dirTile.coord)

        val nextTileCoords =
          nextDirs
            .map(d => DirectedTile(d.next(dirTile.coord), d.opposite))
            .filter { case dt =>
              dt.coord.x >= 0 && dt.coord.x < width && dt.coord.y >= 0 && dt.coord.y < height
            }
        val isSplit = split || (ttt match {
          case HorizontalSplitter
              if dirTile.dir == North || dirTile.dir == South =>
            true
          case VerticalSplitter if dirTile.dir == East || dirTile.dir == West =>
            true
          case _ => false
        })
        if (nextTileCoords.isEmpty) {
          enterTileWithShortcuts(
            dirTiles,
            dirTile +: acc,
            shortcuts.put(dirTile.coord, dirTile.dir, List(dirTile)),
            dirTile.coord +: exits,
            split
          )
        } else if (nextTileCoords.forall(shortcuts.isDefinedAt)) {
          val short = nextTileCoords.flatMap(shortcuts.get)
          val currentEnerg = short.flatten.distinct.length
          enterTileWithShortcuts(
            dirTiles,
            (dirTile +: short.flatten) ++ acc,
            shortcuts.put(dirTile.coord, dirTile.dir, dirTile +: short.flatten),
            exits,
            isSplit
          )
        } else {
          val short = nextTileCoords.flatMap(shortcuts.get).flatten.distinct
          val passed = dirTile +: short
          val next = nextTileCoords.filterNot(shortcuts.isDefinedAt)
          enterTileWithShortcuts(
            next ++ dirTiles,
            passed ++ acc,
            shortcuts,
            exits,
            isSplit
          )
        }
    }

  def maxEnergy: Int = {
    val northBound =
      (0 until width).map(i => DirectedTile(Coordinate(i, 0), North))
    val southBound =
      (0 until width).map(i => DirectedTile(Coordinate(i, height - 1), South))
    val eastBound =
      (0 until height).map(i => DirectedTile(Coordinate(width - 1, i), East))
    val westBound =
      (0 until height).map(i => DirectedTile(Coordinate(0, i), West))

    // map!
    val allBounds = northBound ++ southBound ++ eastBound ++ westBound
    allBounds
      .foldLeft(
        (
          0,
          TileCache.empty,
          List.empty[Coordinate]
        )
      ) { case ((acc, shortcuts, exploredExits), start) =>
        println(s"Processing $start")
        if (exploredExits.contains(start.coord)) {
          println(s"Skipping $start")
          (acc, shortcuts, exploredExits)
        }
        val (energ, shorts, exits) =
          enterTileWithShortcuts(
            List(start),
            List.empty,
            shortcuts,
            List.empty,
            false
          )
        (Math.max(acc, energ), shorts, (exits ++ exploredExits).distinct)
      }
      ._1
  }

}
