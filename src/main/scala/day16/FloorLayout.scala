package day16

import scala.annotation.tailrec

final case class FloorLayout(tiles: List[List[Tile]]) {

    val width = tiles.headOption.fold(0)(_.length)
    val height = tiles.length

    val tilesAsMap = 
            tiles.zipWithIndex.flatMap {
                case(l, y) => l.zipWithIndex.map {
                    case (t, x) => (Coordinate(x, y) -> t)
                }
            }.toMap

    def print: Unit = 
        tiles.foreach(t => println(t.map(_.symbol).mkString))
    
    def energizeFrom(c: Coordinate, towards: Direction): Int = {

        //tine map de la (coordinate, direction) -> energized
        @tailrec
        def enterTile(coordinates: List[(Coordinate, Direction)], acc: List[(Coordinate, Direction)]): Int = {
            coordinates match {
                case Nil => acc.map(_._1).distinct.length
                case c :: coords if acc.contains(c) => enterTile(coords, acc)
                case (coord, direction) :: coords => 
                    val nextDirs = tilesAsMap(coord).energize(direction)
                    val nextTileCoords = nextDirs.map(d => (d.next(coord), d.opposite)).filter {
                        case (newCoord, _) => newCoord.x >= 0 && newCoord.x < width && newCoord.y >=0 && newCoord.y < height
                    }
                    enterTile(nextTileCoords ++ coords, (coord, direction) +: acc)
            }
            
        }

        
        enterTile(List((c, towards)), List.empty)
    }  

    
         @tailrec
        def enterTileWithExits(coordinates: List[(Coordinate, Direction)], acc: List[(Coordinate, Direction)], exits: List[Coordinate]): (Int, List[Coordinate]) = {
            coordinates match {
                case Nil => (acc.map(_._1).distinct.length, exits)
                case c :: coords if acc.contains(c) => enterTileWithExits(coords, acc, exits)
                case (coord, direction) :: coords => 
                    val nextDirs = tilesAsMap(coord).energize(direction)
                    val nextTileCoords = nextDirs.map(d => (d.next(coord), d.opposite)).filter {
                        case (newCoord, _) => newCoord.x >= 0 && newCoord.x < width && newCoord.y >=0 && newCoord.y < height
                    }
                    if(nextTileCoords.isEmpty) {
                        enterTileWithExits(coords, (coord, direction) +: acc, coord +: exits)
                    } else 
                        enterTileWithExits(nextTileCoords ++ coords, (coord, direction) +: acc, exits)
            }
            
        }


    def maxEnergy: Int = {
        val northBound = (0 until width).map(i => (Coordinate(i, 0), North))
        val southBound = (0 until width).map(i => (Coordinate(i, height - 1), South))
        val eastBound = (0 until height).map(i => (Coordinate(width - 1, i), East))
        val westBound = (0 until height).map(i => (Coordinate(0, i), West))

        // map!
        val allBounds = northBound ++ southBound ++ eastBound ++ westBound
        allBounds.foldLeft((List.empty[Coordinate], 0)) {
            case ((exits, acc), start) => 
                println(s"Exploring $start")
                if(exits.contains(start._1)) {
                println(s"SKIPPING $start")
                (exits, acc) 
            }
            else {
                val (energ, exitsReached) = enterTileWithExits(List(start), List.empty, List.empty)
                (exits ++ exitsReached, Math.max(acc, energ))
            } 
        }._2
    }

}
