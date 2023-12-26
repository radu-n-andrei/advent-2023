package day23

import common.Coordinate
import scala.annotation.tailrec

case class SimplifiedPath(
    head: Coordinate,
    length: Int,
    junctions: List[Coordinate],
    goingThrough: Coordinate
)

case class JunctionSolution(
    solutions: Map[Coordinate, List[SimplifiedPath]],
    currentMax: Int
) {

  def withSolution(tile: UnvisitedTile): JunctionSolution = {
    val simplified =
      readJunctions(tile.junctions, tile.path, List.empty, 0, List.empty, true)
    val updatedMap = simplified.foldLeft(solutions) { case (m, s) =>
      m + (s.head -> m.get(s.head).fold(List(s))(ex => ex :+ s))
    } 
    val newMax = Math.max(tile.path.length, currentMax)
    JunctionSolution(
      updatedMap,
      newMax
    )
  }

  def append(
      tile: UnvisitedTile
  ): Option[(JunctionSolution, List[Coordinate])] = {
    solutions.get(tile.coord).map { sols =>
      val potential =
        sols.filter(s => s.junctions.intersect(tile.junctions).isEmpty)
        
      val newEntries = potential.flatMap { pot =>
        val js = readJunctions(
          tile.junctions,
          tile.path,
          List.empty,
          pot.length,
          pot.junctions,
          false
        )
        js.map(j => j.head -> j)
      }
      val updatedSolution = newEntries.foldLeft(solutions) { case (acc, e) =>
        acc + (e._1 -> acc
          .get(e._1)
          .fold(List(e._2))(existing => existing :+ e._2))
      }
      // found a valid solution
      if (potential.nonEmpty) {
        println(s"New sol: ${Math.max(
              potential.map(_.length).max + tile.path.length,
              currentMax
            )}")
        (
          JunctionSolution(
            updatedSolution,
            Math.max(
              potential.map(_.length).max + tile.path.length,
              currentMax
            )
          ),
          potential.map(_.goingThrough).distinct
        )
      }
      // found a cyclical solution, don't update - continue exploring   
      else (JunctionSolution(solutions, currentMax), List.empty)
    }
  }

  @tailrec
  private def readJunctions(
      junctions: List[Coordinate],
      path: List[Coordinate],
      simplified: List[SimplifiedPath],
      addedLength: Int,
      addedJunctions: List[Coordinate],
      fullSolution: Boolean
  ): List[SimplifiedPath] =
    junctions match {
      case Nil      => simplified
      case _ :: Nil if !fullSolution => simplified
      case j :: js =>
        val fromHere = path.dropWhile(_ != j).tail
        readJunctions(
          js,
          path,
          simplified :+ SimplifiedPath(
            j,
            fromHere.length + addedLength,
            js ++ addedJunctions,
            fromHere.head
          ),
          addedLength,
          addedJunctions,
          fullSolution
        )
    }

}

object JunctionSolution {
  val empty: JunctionSolution = JunctionSolution(Map.empty, 0)
}
