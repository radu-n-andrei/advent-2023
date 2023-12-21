package day21

import common.Coordinate
import scala.annotation.tailrec

final case class Garden(
    tiles: Map[Coordinate, Tile],
    start: Coordinate,
    height: Int,
    width: Int
) {

  /**
    * S is in the center with walkable tiles in each cardinal direction 
    * => expanding it will create a rhombus f limit x limit
    * that means we will have N full maps + 4 corners + N-
    * the solution will be N * walkable_tiles_in_map + T1 + T2 + T3 + T4
    * @return
    */
  def getMegaSteps(limit: Int): Long = {
    val lengthInSquares: Long = (limit - start.y) / height.toLong
    val fullSquares = 2 * (lengthInSquares - 2) * (lengthInSquares - 1) + (lengthInSquares - 1) * 4 + 1
    val partialSquares = (lengthInSquares - 1) * 4
    val totalSquares = fullSquares + partialSquares / 2 
    val walkable = tiles.values.count {
      case _: WalkableTile => true
      case _ => false
    }
    // subtract diagonals each times lengthInSquare - 1
    // for corners? subtract inner rommbus edges
    val diag1 = tiles.filter {
      case (c, t) => c.x == c.y
    }.values.count {
      case _: WalkableTile => true
      case _ => false
    }
    val diag2 = tiles.filter {
      case (c, t) => 
        c.x + c.y == width - 1
    }.values.count {
      case _: WalkableTile => true
      case _ => false
    }

    val leftCorner = (0 to width - 1 ).map { x =>
        tiles.view.filterKeys(c => c.x == x && c.y >= start.y - x && c.y <= start.y + x).toMap.values.count{
          case _: WalkableTile => true
          case _ => false
        }
      }.sum

    val rightCorner = (0 to width - 1).map { x =>
        tiles.view.filterKeys(c =>c.x == x && c.y >= x && c.y <= width - x - 1).toMap.values.count{
          case _: WalkableTile => true
          case _ => false
        }
      }.sum  

    val topCorner = (0 to width - 1).map { y =>
        tiles.view.filterKeys(c => c.y == y && c.x >= start.x - y && c.x <= start.x + y).toMap.values.count{
          case _: WalkableTile => true
          case _ => false
        }
      }.sum   

    val bottomCorner = (0 to width - 1).map { y =>
        tiles.view.filterKeys(c => c.y == y && c.x >= y && c.x <= width - y - 1).toMap.values.count{
          case _: WalkableTile => true
          case _ => false
        }
      }.sum 
    
    
    totalSquares * walkable - (diag1 + diag2) * (lengthInSquares - 1)  + topCorner + bottomCorner + leftCorner + rightCorner
  }

  def getSteps(limit: Int): Int = {

    @tailrec
    def explore(
        nextMoves: List[ExplorationTarget],
        explored: Map[Coordinate, List[Int]],
        target: Set[Coordinate]
    ): Int = {
      nextMoves match {
        case Nil => target.size
        case c :: cs =>
          val moves = c.coordinate
            .allMoves()
            .filter(coord =>
              coord.x >= 0 && coord.y >= 0 && coord.x < width && coord.y < height
            )
            .filter(coord =>
              tiles(coord) match {
                case _: WalkableTile => true
                case _               => false
              }
            ).filter( coord =>
                !explored.isDefinedAt(coord) || !explored(coord).contains(c.step)
            )
          //println(s"At $c with moves: $moves... ${c.coordinate.allMoves()}")    
          val newExplored = moves.foldLeft(explored) { case (acc, coord) =>
            acc + acc
              .get(coord)
              .fold(coord -> List(c.step))(s => (coord -> (s :+ c.step)))
          }
          if(c.step == limit) {
            explore(cs, explored ++ newExplored, target ++ moves)
          } else {
            val nextUp = moves.map(coord => ExplorationTarget(coord, c.step + 1))
            explore(nextUp ++ cs, explored ++ newExplored, target)
          }
      }
    }

    explore(List(ExplorationTarget(start,1)), Map(start -> List(0)), Set.empty)
  }
}

object Garden {
  def apply(input: List[String]): Garden = {
    def gatherTiles(
        in: List[String],
        row: Int,
        layout: Map[Coordinate, Tile],
        start: Option[Coordinate]
    ): Garden =
      in match {
        case Nil =>
          Garden(layout, start.get, row, input.headOption.fold(0)(_.length))
        case i :: ins =>
          val tiles = i.zipWithIndex.map { case (tile, col) =>
            (Coordinate(col, row) -> Tile(tile))
          }.toMap
          val st = tiles.collectFirst { case (c, Start) =>
            c
          }
          gatherTiles(ins, row + 1, layout ++ tiles, start.orElse(st))
      }
    gatherTiles(input, 0, Map.empty, None)
  }
}

case class ExplorationTarget(coordinate: Coordinate, step: Int)
