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
    val longestPathTo = tiles
      .filter(x => Tile.walkable(x._2))
      .map(e => e._1 -> List.empty[Coordinate])
      .toMap
    val updatedPaths = longestPathTo + (start -> List(start))

    def walkAround(
        coord: Coordinate,
        paths: Map[Coordinate, List[Coordinate]],
        unvisited: List[Coordinate],
        currentMax: Int
    ): Int = {
      if (coord == end) {
        println(
          s"Reached  end with len ${paths(coord).length}. Unvisited len: ${unvisited.length}"
        )
        scala.io.StdIn.readLine()
        if (unvisited.length == 1) {
          printSolution(paths(end))
          paths(coord).length - 1
        } else {
          val updatedUnvisited = unvisited.filterNot(_ == coord)
          val nextUp = (paths - coord)
            .filter(c => updatedUnvisited.contains(c._1))
            .maxBy(_._2.length)
            ._1
          walkAround(nextUp, paths, updatedUnvisited, paths(coord).length - 1)
        }
      } else {
        println(s"AT $coord")
        printSolution(paths(coord))
        println(unvisited)
        scala.io.StdIn.readLine()

        val currentPath = paths(coord)
        val moves = tiles(coord)
          .moves(coord)
          .filter(c =>
            c._2.x >= 0 && c._2.y >= 0 && c._2.x < width && c._2.y < height
              && tiles(c._2).reachableFrom(c._1)
              && !currentPath
                .contains(c._2)
          )
          .map(_._2)
        val (updatedPaths, toCheck) =
          moves.foldLeft((paths, List.empty[Coordinate])) { case (p, m) =>
            if (paths(m).length < currentPath.length + 1)
              (p._1 + (m -> (currentPath :+ m)), p._2 :+ m)
            else p
          }

        val unvisitedPart = unvisited.partition(_ == coord)
        val updatedUnvisited =
          unvisitedPart._1.drop(1) ++ unvisitedPart._2 ++ toCheck
        val finalPaths =
          if (toCheck.isEmpty)
            updatedPaths + (coord -> List.empty)
          else updatedPaths
        val nextUpCandidates = finalPaths
          .filter(c => updatedUnvisited.contains(c._1))

        if (nextUpCandidates.isEmpty) {
          printSolution(paths(end))
          currentMax
        } else
          walkAround(
            nextUpCandidates.maxBy(_._2.length)._1,
            finalPaths,
            updatedUnvisited,
            currentMax
          )
      }
    }

    println(s"SOL1: ${walkAround(start, updatedPaths, List.empty, 0)}")
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
