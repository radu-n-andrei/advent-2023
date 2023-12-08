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
    // println(s"SOL1: ${navigate(startNode, 0, desertMap, 0, _ == endNode).len}")
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
    // precalculate shortcuts (aka roads between Zs)
    val endsFromFirstRun = firstPaths.map(p => RouteEnd(p.end, p.len))
    val shortcuts =
      getShortcuts(endsFromFirstRun, desertMap, Map.empty, List.empty)

    println(
      s"SOL2: ${extremelyUnlikelyCornerCaseLCM(endsFromFirstRun, shortcuts, desertMap.directionLength)
          .getOrElse(advance(firstPaths, desertMap.directionLength, shortcuts, List.empty, firstPaths.map(_.len).max))}"
    )

    println(
      s"SOL2: ${advance(firstPaths, desertMap.directionLength, shortcuts, List.empty, firstPaths.map(_.len).max)}"
    )

    file.close()
  }

  private def extremelyUnlikelyCornerCaseLCM(
      firstRunEnds: List[RouteEnd],
      shortcuts: Shortcut,
      mapLen: Int
  ): Option[Long] = {
    def gcd(a: Long, b: Long): Long = {
      val (first, second) = if (a < b) (a, b) else (b, a)
      if (second % first == 0) first
      else gcd(first, second % first)
    }

    if (
      firstRunEnds.forall(re =>
        re == shortcuts(RouteStart(re.endNode, (re.len % mapLen).toInt))
      )
    ) {
      println("THIS IS A JOKE!")
      Some(firstRunEnds.map(_.len).reduce { (a, b) =>
        a * b / gcd(a, b)
      })
    } else None
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
      desertMapLen: Int,
      shortcuts: Shortcut,
      newPaths: List[DesertPath],
      longestPath: Long
  ): Long = {
    currentPaths match {
      case Nil if newPaths.map(_.len).distinct.size == 1 => newPaths.head.len
      case Nil =>
        advance(
          newPaths,
          desertMapLen,
          shortcuts,
          List.empty,
          newPaths.map(_.len).max
        )
      case p :: paths if p.len < longestPath => // this path needs to keep going
        val updatedPath =
          tryToMatchHighest(
            p,
            desertMapLen,
            shortcuts,
            longestPath
          ) // recursively grow single path and shortcuts
        advance( // on to the next path
          paths,
          desertMapLen,
          shortcuts,
          updatedPath +: newPaths,
          Math.max(longestPath, updatedPath.len)
        )
      case p :: paths => // already at longest
        advance(paths, desertMapLen, shortcuts, p +: newPaths, longestPath)
    }
  }

  private def toTheNextEnd(
      dp: DesertPath,
      shortcuts: Shortcut,
      nextDirection: Int
  ): DesertPath = {
    val endToEnd = shortcuts(RouteStart(dp.end, nextDirection))
    DesertPath(
      dp.start,
      endToEnd.endNode,
      dp.len + endToEnd.len
    )
  }

  @tailrec
  private def tryToMatchHighest(
      dp: DesertPath,
      desertMapLen: Int,
      shortcuts: Shortcut,
      longestPath: Long
  ): DesertPath =
    if (dp.len >= longestPath) {
      dp // at or passed the target
    } else {
      val nextDirection = (dp.len % desertMapLen).toInt
      val updatedPath =
        toTheNextEnd(dp, shortcuts, nextDirection)
      tryToMatchHighest(
        updatedPath,
        desertMapLen,
        shortcuts,
        longestPath
      )
    }

  @tailrec
  private def getShortcuts(
      ends: List[RouteEnd],
      desertMap: DesertMap,
      shorcuts: Shortcut,
      newEnds: List[RouteEnd]
  ): Shortcut = {
    ends match {
      case Nil if newEnds.isEmpty => shorcuts
      case Nil => getShortcuts(newEnds, desertMap, shorcuts, List.empty)
      case p :: ps =>
        val correspondingStart =
          RouteStart(p.endNode, (p.len % desertMap.directionLength).toInt)
        if (shorcuts.isDefinedAt(correspondingStart)) {
          getShortcuts(ps, desertMap, shorcuts, newEnds)
        } else {
          val navigation = navigate(
            correspondingStart.startNode,
            correspondingStart.directionIndex,
            desertMap,
            0,
            _.endsWith("Z")
          )
          getShortcuts(
            ps,
            desertMap,
            shorcuts + (correspondingStart -> navigation),
            newEnds :+ navigation
          )
        }
    }
  }
}
