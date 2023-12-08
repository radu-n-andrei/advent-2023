package day8

import scala.io.Source
import scala.annotation.tailrec

object Haunted {

  val endNode = "ZZZ"
  val startNode = "AAA"

  type Shortcut = Map[RouteStart, RouteEnd]

  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day8.input")
    val input = file.getLines().toList
    val desertMap =
      readMap(input, DesertMap(List.empty, Map.empty), stillOnDirections = true)
    //println(s"SOL1: ${navigate(startNode, 0, desertMap, 0, _ == endNode).len}")
    val startNodes =
      desertMap.nodes.view.filterKeys(_.endsWith("A")).keySet.toList
    val firstPaths = startNodes.map { startNode =>
      val navigation = navigate(startNode, 0, desertMap, 0, _.endsWith("Z"))
      DesertPath(
        startNode,
        navigation.endNode,
        navigation.len
      )
    }
    firstPaths.foreach(println)
    println(
      s"SOL2: ${advance(firstPaths, desertMap, Map.empty, List.empty, firstPaths.map(_.len).max)}"
    )

    file.close()
  }

  private def readMap(
      input: List[String],
      result: DesertMap,
      stillOnDirections: Boolean
  ): DesertMap = {
    val template =
      s"([A-Z1-9]{3})\\s+=\\s+\\(([A-Z1-9]{3}),\\s([A-Z1-9]{3})\\)".r
    input match {
      case Nil => result
      case x :: xs if x.trim.isEmpty =>
        readMap(xs, result, stillOnDirections = false)
      case x :: xs if stillOnDirections =>
        readMap(
          xs,
          result.copy(directions =
            result.directions ++ x.map(Direction(_)).toList
          ),
          stillOnDirections
        )
      case x :: xs =>
        x match {
          case template(root, left, right) =>
            readMap(
              xs,
              result.copy(nodes =
                result.nodes + (root -> DesertMapEntry(root, left, right))
              ),
              stillOnDirections
            )
          case _ => throw new RuntimeException(s"Illegal node structure: $x")
        }
    }
  }

  private def nextMove(
      desertMap: DesertMap,
      currentNode: String,
      nextDirection: Int
  ): String =
    desertMap.move(currentNode, desertMap.directions(nextDirection))

  @tailrec
  private def navigate(
      currentNode: String,
      nextDirection: Int,
      desertMap: DesertMap,
      steps: Long,
      isEndNode: String => Boolean
  ): RouteEnd = {
    if (isEndNode(currentNode) && steps > 0) RouteEnd(currentNode, steps)
    else {
      val nextNode =
        nextMove(
          desertMap,
          currentNode,
          nextDirection
        )
      navigate(
        nextNode,
        (nextDirection + 1) % desertMap.directionLength,
        desertMap,
        steps + 1,
        isEndNode
      )
    }
  }

  @tailrec
  private def advance(
      currentPaths: List[DesertPath],
      desertMap: DesertMap,
      shortcuts: Shortcut,
      newPaths: List[DesertPath],
      longestPath: Long
  ): Long = {
    println(s"Advancing towards: $longestPath")
    currentPaths match {
      case Nil if newPaths.map(_.len).distinct.size == 1 => newPaths.head.len
      case Nil =>
        advance(
          newPaths,
          desertMap,
          shortcuts,
          List.empty,
          newPaths.map(_.len).max
        )
      case p :: paths if p.len < longestPath => // this path needs to keep going
        val (updatedPath, shorts) =
          tryToMatchHighest(
            p,
            desertMap,
            shortcuts,
            longestPath
          ) // recursively grow single path and shortcuts
        advance( // on to the next path
          paths,
          desertMap,
          shorts,
          updatedPath +: newPaths,
          Math.max(longestPath, updatedPath.len)
        )
      case p :: paths => // already at longest
        advance(paths, desertMap, shortcuts, newPaths :+ p, longestPath)
    }
  }

  private def toTheNextEnd(
      dp: DesertPath,
      desertMap: DesertMap,
      shortcuts: Shortcut,
      nextDirection: Int
  ): (DesertPath, Shortcut) = {
    val endToEnd = shortcuts
      .getOrElse(
        RouteStart(dp.end, nextDirection),
        navigate(
          dp.end,
          nextDirection,
          desertMap,
          0,
          _.endsWith("Z")
        ) // use shortcut or find next path to a Z
      )
    (
      DesertPath(
        dp.start,
        endToEnd.endNode,
        dp.len + endToEnd.len
      ),
      shortcuts + (RouteStart(dp.end, nextDirection) -> endToEnd)
    )
  }

  @tailrec
  private def tryToMatchHighest(
      dp: DesertPath,
      desertMap: DesertMap,
      shortcuts: Shortcut,
      longestPath: Long
  ): (DesertPath, Shortcut) =
    if (dp.len >= longestPath) {
      (dp, shortcuts) // at or passed the target
    } else {
      val nextDirection = (dp.len % desertMap.directionLength).toInt
      val (updatedPath, shorts) =
        toTheNextEnd(dp, desertMap, shortcuts, nextDirection)
      tryToMatchHighest(
        updatedPath,
        desertMap,
        shorts,
        longestPath
      )
    }
}
