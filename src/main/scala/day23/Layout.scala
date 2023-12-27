package day23

import common.Coordinate
import common.North
import scala.concurrent.duration.fromNow
import scala.annotation.tailrec

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
            if (coords.contains(Coordinate(x, y)))
              if(coords.last == Coordinate(x,y)) 'S'
              else '0'
            else tiles(Coordinate(x, y)).symbol
          )
          .mkString
      )
    }

  val start: Coordinate =
    tiles.filter(e => e._1.y == 0 && Tile.walkable(e._2)).head._1
  val end: Coordinate =
    tiles.filter(e => e._1.y == height - 1 && Tile.walkable(e._2)).head._1

  val preEnd: Coordinate = North.moveOne(end)

  def longestPath: Unit = {

    @tailrec
    def tunnelVision(tile: UnvisitedTile): (UnvisitedTile, List[Coordinate]) = {
      val moves = tiles(tile.coord)
        .moves(tile.coord)
        .filter(c =>
          c._2.x >= 0 && c._2.y >= 0 && c._2.x < width && c._2.y < height
            && tiles(c._2).reachableFrom(c._1)
        )
        .map(_._2)
        .toList
      val validMoves = moves.filter(c =>
        !tile.path
          .contains(c)
      )
      val junc = if(moves.length > 2) tile.junctions :+ tile.coord else tile.junctions
      if (validMoves.length == 1) {
        printSolution(tile.path :+ validMoves.head)
        scala.io.StdIn.readLine()
        tunnelVision(
          UnvisitedTile(
            validMoves.head,
            tile.path :+ validMoves.head,
            junc
          )
        )
      } else {
        if (tile.coord == end)
          (tile, validMoves)
        else
          (tile.copy(junctions = junc), validMoves)
      }
    }
    @tailrec
    def explore(
        tile: UnvisitedTile,
        toVisit: List[UnvisitedTile],
        solution: JunctionSolution
    ): Unit = {
      // maybe here?
      val (tunneled, standardMoves) = tunnelVision(tile)
     
      if (tunneled.coord == end) {
        if (toVisit.isEmpty)
          println(
            s"SOL: ${solution.withSolution(tunneled).currentMax - 1}"
          )
        else {
          val nextUp = toVisit.maxBy(_.path.length)
          println("SOLUTION")
          printSolution(tunneled.path)
          scala.io.StdIn.readLine()
          explore(
            nextUp,
            toVisit.filterNot(_ == nextUp),
            solution.withSolution(tunneled)
          )
        }
      } else {
        val potentialSolutions = solution.append(tunneled)
        val exhausted = potentialSolutions.map(_._2).getOrElse(List.empty)
        
        val moves = standardMoves.filter(c => !exhausted.contains(c))
        val unvisited =
          moves
            .map(c =>
              UnvisitedTile(
                c,
                tunneled.path :+ c,
                tunneled.junctions
              )
            )
        println(s"Adding: ${unvisited.map(_.coord)}")    
        potentialSolutions.map(_._1).getOrElse(solution).solutions.foreach {
          println
        }
        val nextVisits = unvisited ++ toVisit
        if (nextVisits.isEmpty) {
          println(
            s"Sol: ${potentialSolutions.map(_._1).getOrElse(solution).currentMax - 1}"
          )
        }
        else {
          val nextUp = nextVisits.maxBy(_.path.length)
          println(s"NEXT: ${nextUp.coord} - ${nextUp.path.length}; left: ${nextVisits.filterNot(_ == nextUp).map(_.coord)}")
          scala.io.StdIn.readLine()
          explore(
            nextUp,
            nextVisits.filterNot(_ == nextUp),
            potentialSolutions.map(_._1).getOrElse(solution)
          )
        }

      }

    }

    explore(
      UnvisitedTile(start, List(start), List.empty),
      List.empty,
      JunctionSolution.empty
    )
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
      input.headOption.fold(0)(_.length),
      input.length
    )
}

case class UnvisitedTile(
    coord: Coordinate,
    path: List[Coordinate],
    junctions: List[Coordinate]
)

case class Solution(tiles: List[Coordinate]) {

  val length: Int = tiles.length

  def buildUp(path: List[Coordinate], head: Coordinate): Option[Solution] = {
    val ind = tiles.indexOf(head)
    if (ind < 0) None
    else {
      val newPath = path ++ tiles.drop(ind + 1)
      if (newPath.distinct.length < newPath.length) None
      else Some(Solution(newPath))
    }
  }
}
