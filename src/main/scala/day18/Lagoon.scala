package day18

import java.io.FileWriter
import java.io.File

final case class Lagoon(
    layout: Map[Coordinate, Space],
    width: Int,
    height: Int
) {

  def print: Unit =
    (0 to height).foreach { y =>
      println((0 to width).map { x =>
        layout(Coordinate(x, y)) match {
          case Trench(_) => '#'
          case Ground    => '.'
        }
      }.mkString)
    }

  def printToFile(fName: String): Unit = {
    val fileWriter = new FileWriter(new File(fName))
    (0 to height).foreach { y =>
      fileWriter.write((0 to width).map { x =>
        layout(Coordinate(x, y)) match {
          case Trench(Up) => '^'
          case Trench(Right) => '>'
          case Trench(Left) => '<'
          case Trench(Down) => 'v'
          case Ground    => '.'
        }
      }.mkString + "\n")

    }
    fileWriter.close()
  }

  def fill: Lagoon = {

    def connectedDir(dirCoord: DirectedCoordinate): List[Direction] = {
       List(Right, Left, Up, Down)
        .map(d =>
          d.move(dirCoord.coordinate, 1)
            .filter(c => c.x >= 0 && c.y >= 0 && c.x <= width && c.y <= height)
        ).filterNot(_.isEmpty).map(l => layout(l.head)).collect {
            case Trench(direction) => direction
        }
        
    }

    def fillRow(
        rowIndex: Int,
        colIndex: Int,
        inside: Boolean,
        acc: List[DirectedCoordinate],
        prevTrenchDirections: Set[Direction],
        currentDir: Option[Direction]
    ): List[DirectedCoordinate] = {
      if (rowIndex > height) acc
      else if (colIndex > width)
        fillRow(rowIndex + 1, 0, false, acc, Set.empty, None)
      else {
        val coord = Coordinate(colIndex, rowIndex)
        layout(coord) match {
          case Trench(d) =>
            val connectedDirs = connectedDir(DirectedCoordinate(d, coord))
            val adjacentTurns = connectedDirs.filterNot(_ == d)
            fillRow(
              rowIndex,
              colIndex + 1,
              inside,
              acc,
              (prevTrenchDirections + d) ++ adjacentTurns.toSet,
              Some(d)
            )
          case Ground =>
            val nextInside = prevTrenchDirections.size match {
                case 0 => inside
                case 1 | 2 => !inside
                case _ => inside
            }
            val newAcc =
              if (nextInside)
                acc :+ DirectedCoordinate(
                  currentDir.get,
                  Coordinate(colIndex, rowIndex)
                )
              else acc
            fillRow(
              rowIndex,
              colIndex + 1,
              nextInside,
              newAcc,
              Set.empty,
              currentDir
            )
        }
      }
    }

    val filledCoord = fillRow(0, 0, false, List.empty, Set.empty, None)
    Lagoon(
      filledCoord.foldLeft(layout) { case (acc, f) =>
        acc + (f.coordinate -> Trench(f.direction))
      },
      width,
      height
    )

  }

  def volume: Int =
    layout.values.count {
      case Trench(_) => true
      case _         => false
    }

}

object Lagoon {
  def fromTrench(trenches: List[DirectedCoordinate]): Lagoon = {
    val width = trenches.map(t => t.coordinate.x).max
    val height = trenches.map(t => t.coordinate.y).max
    val spaces: Map[Coordinate, Space] = (0 to width).flatMap { x =>
      (0 to height).map { y =>
        (Coordinate(x, y) -> Ground)
      }
    }.toMap
    Lagoon(
      trenches.foldLeft(spaces) { case (acc, t) =>
        acc + (t.coordinate -> Trench(t.direction))
      },
      width,
      height
    )
  }
}

sealed trait Space
case class Trench(direction: Direction) extends Space
case object Ground extends Space
