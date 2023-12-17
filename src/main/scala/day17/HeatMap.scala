package day17

import scala.annotation.tailrec

final case class HeatMap(
    layout: Map[Coordinate, Node],
    width: Int,
    height: Int
) {

  def inBound(coord: Coordinate): Boolean =
    coord.x >= 0 && coord.y >= 0 && coord.x < width && coord.y < height

  def nextNodes(pathNode: PathNode, step: Int, ultra: Boolean): List[PathNode] =
    pathNode
      .moves(step)
      .flatMap(m =>
        m.filter(dc => inBound(dc.coordinate)).foldLeft(List.empty[PathNode]) {
          case (acc, dc) =>
            acc :+ PathNode(
              dc.coordinate,
              dc.direction,
              acc.lastOption.fold(layout(dc.coordinate).score + pathNode.score)(
                t => t.score + layout(dc.coordinate).score
              ),
              !ultra || acc.length >= 3
            )
        }
      )

  val start = Coordinate(0, 0)
  val end = Coordinate(width - 1, height - 1)

  def shortestRoute(step: Int, ultra: Boolean): Unit = {

    val startNodeN = PathNode(start, North, 0)
    val preloadedN =
      nextNodes(startNodeN, step, ultra).filter(p => !ultra || p.canTurn)
    val startNodeW = PathNode(start, West, 0)
    val preloadedW =
      nextNodes(startNodeW, step, ultra).filter(p => !ultra || p.canTurn)
    val preloadedTodos = (preloadedN ++ preloadedW)
      .map(pn => DirectedCoordinate(pn.enteredVia, pn.coord) -> pn)
      .toMap
    val preloadedDone = List(startNodeN, startNodeW)
      .map(pn => DirectedCoordinate(pn.enteredVia, pn.coord) -> pn)
      .toMap

    val firstNode = preloadedTodos.minBy(_._2.score)

    def processNodes(
        pathNode: PathNode,
        todos: Map[DirectedCoordinate, PathNode],
        done: Map[DirectedCoordinate, PathNode]
    ): Unit = {
      if (pathNode.coord == end) {
        println(s"SOL: ${pathNode.score}")
      } else {
        val next =
          nextNodes(pathNode, step, ultra).filter(p => !ultra || p.canTurn)
        val updatedNext = next
          .map { n =>
            todos
              .get(n.directedCoordinate)
              .fold(n)(existing =>
                if (existing.score > n.score) n
                else existing
              )
          }
          .filterNot(pn => done.isDefinedAt(pn.directedCoordinate))

        val newTodos = updatedNext.foldLeft(todos) { case (tds, pn) =>
          tds + (pn.directedCoordinate -> pn)
        }
        val updatedDone = done + (pathNode.directedCoordinate -> pathNode)
        val nextUp = newTodos.minBy(_._2.score)
        processNodes(nextUp._2, newTodos - nextUp._1, updatedDone)
      }

    }
    processNodes(firstNode._2, preloadedTodos - firstNode._1, preloadedDone)

  }

}

object HeatMap {

  def apply(in: List[List[Int]]): HeatMap = {
    val height = in.length
    val width = in.headOption.fold(0)(_.length)

    @tailrec
    def buildNode(
        coord: List[Coordinate],
        p: Map[Coordinate, Node]
    ): Map[Coordinate, Node] = {
      coord match {
        case Nil                             => p
        case c :: coords if p.isDefinedAt(c) => buildNode(coords, p)
        case c :: coords =>
          val currerntHeat = in(c.y)(c.x)
          val next = List(
            Coordinate(c.x, c.y - 1),
            Coordinate(c.x, c.y + 1),
            Coordinate(c.x - 1, c.y),
            Coordinate(c.x + 1, c.y)
          ).filter(cc =>
            cc.x >= 0 && cc.y >= 0 && cc.x < width && cc.y < height
          )
          buildNode(
            next.filterNot(p.isDefinedAt) ++ coords,
            p + (c -> Node(currerntHeat))
          )
      }

    }
    HeatMap(buildNode(List(Coordinate(0, 0)), Map.empty), width, height)
  }

}

case class Coordinate(x: Int, y: Int)
final case class Node(score: Int)
