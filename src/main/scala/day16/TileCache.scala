package day16

final case class TileCache(cache: Map[Coordinate, TileCacheEntry]) {

  def get(dirTile: DirectedTile): Option[List[DirectedTile]] =
    cache.get(dirTile.coord).flatMap(_.entry.get(dirTile.dir))

  def put(coord: Coordinate, dir: Direction, l: List[DirectedTile]): TileCache =
    cache.get(coord) match {
      case None => TileCache(cache + (coord -> TileCacheEntry(Map(dir -> l))))
      case Some(e) =>
        TileCache(cache + (coord -> TileCacheEntry(e.entry + (dir -> l))))
    }

  def isDefinedAt(dt: DirectedTile): Boolean =
    cache.get(dt.coord).map(_.entry.isDefinedAt(dt.dir)).getOrElse(false)
}

object TileCache {
  val empty = TileCache(Map.empty)
}

final case class TileCacheEntry(entry: Map[Direction, List[DirectedTile]])
object TileCacheEntry {
  val empty = TileCacheEntry(Map.empty)
}
final case class DirectedTile(coord: Coordinate, dir: Direction)

final case class Coordinate(x: Int, y: Int)
