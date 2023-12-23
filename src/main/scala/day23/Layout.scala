package day23

import common.Coordinate

final case class Layout(tiles: Map[Coordinate, Tile], width: Int, height: Int) {

  def print: Unit =
    (0 until height).foreach { y =>
      println((0 until width).map(x => tiles(Coordinate(x, y)).symbol).mkString)
    }

  def printSolution(coords: List[Coordinate]): Unit =
    (0 until height).foreach { y =>
      println(
        (0 until width)
          .map(x =>
            if (coords.contains(Coordinate(x, y))) 'O'
            else tiles(Coordinate(x, y)).symbol
          )
          .mkString
      )
    }

  val start: Coordinate =
    tiles.filter(e => e._1.y == 0 && Tile.walkable(e._2)).head._1
  val end: Coordinate =
    tiles.filter(e => e._1.y == height - 1 && Tile.walkable(e._2)).head._1

  def longestPath: Unit = {

    def explore(
        tile: UnvisitedTile,
        toVisit: List[UnvisitedTile],
        solutions: List[Solution]
    ): Unit = {

      // maybe here?
      val (tunneled, standardMoves) = tunnelVision(tile)

      if (tunneled.coord == end) {
        println(s"FOUND ONE: ${tunneled.path.length - 1}")
        if (toVisit.isEmpty)
          println(
            s"SOL: ${(solutions :+ Solution(tunneled.path)).map(_.tiles.keySet.size).max - 1}"
          )
        else {
          val nextUp = toVisit.maxBy(_.path.length)
          explore(
            nextUp,
            toVisit.filterNot(_ == nextUp),
            solutions :+ Solution(tunneled.path)
          )
        }
      } else {
        val potentialSolutions = solutions.flatMap(_.buildUp(tunneled.path, tunneled.coord))
        if (potentialSolutions.nonEmpty) {
          println(
            s"Adding ${potentialSolutions.length} new solutions on top of existing ${solutions.length}. Current max: ${(potentialSolutions ++ solutions).map(_.tiles.keySet.size).max}"
          )
          println(s"\tLeft to check: ${toVisit.length}")
        }
        val exhausted = potentialSolutions.map { s =>
          val ind = s.tiles(tunneled.coord)
          s.tiles.filter(_._2 == ind + 1).head._1
        }
        val moves = standardMoves.filter(c => !exhausted.contains(c))

        val unvisited =
          moves.map(c => UnvisitedTile(c, tunneled.path :+ c)).toList
        val nextVisits = unvisited ++ toVisit
        if (nextVisits.isEmpty)
          println(
            s"From ${solutions.length} solutions we get: ${solutions.map(_.tiles.keySet.size).max - 1}"
          )
        else {
          val nextUp = nextVisits.maxBy(_.path.length)
          explore(
            nextUp,
            nextVisits.filterNot(_ == nextUp),
            solutions ++ potentialSolutions//.filter(s => solutions.forall(sol => sol.tiles.length <= s.tiles.length))

          )
        }

      }
    }

    def tunnelVision(tile: UnvisitedTile): (UnvisitedTile, List[Coordinate]) = {
      val moves = tiles(tile.coord)
        .moves(tile.coord)
        .filter(c =>
          c._2.x >= 0 && c._2.y >= 0 && c._2.x < width && c._2.y < height
            && tiles(c._2).reachableFrom(c._1) && !tile.path
              .contains(c._2)
        )
        .map(_._2)
        .toList

      if (moves.length == 1) {
        tunnelVision(UnvisitedTile(moves.head, tile.path :+ moves.head))
      } else {
        (tile, moves)
      }
    }
    explore(UnvisitedTile(start, List(start)), List.empty, List.empty)
  }

  def desloped: Layout = {
    val x: Map[Coordinate, Tile] = tiles.view.mapValues {
      case Forrest => Forrest
      case _       => Path
    }.toMap
    Layout(x, width, height)
  }

}

object Layout {
  def apply(input: List[String]): Layout =
    Layout(
      input.zipWithIndex.flatMap { in =>
        in._1.zipWithIndex.map(c => (Coordinate(c._2, in._2) -> Tile(c._1)))
      }.toMap,
      input.headOption.fold(0)(_.length()),
      input.length
    )
}

case class UnvisitedTile(coord: Coordinate, path: List[Coordinate])

case class Solution(tiles: Map[Coordinate, Int]) {

  def buildUp(path: List[Coordinate], head: Coordinate): Option[Solution] = {
    tiles.get(head).flatMap{
      i => 
        val newPath = path ++ tiles.filter(_._2 > i).map(_._1)
        if(newPath.distinct.length < newPath.length) None
        else Some(Solution(newPath))
    }
  }
}

object Solution {
  def apply(tiles: List[Coordinate]): Solution = {
    val m = tiles.zipWithIndex.map(c => c._1 -> c._2).toMap
    Solution(m)
  }
}
