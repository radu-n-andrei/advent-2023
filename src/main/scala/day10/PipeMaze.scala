package day10

final case class PipeMaze(
    distances: Map[Coordinate, Int]
)

object PipeMaze {

  def getByCoordinate[T <: WithCoordinate](
      layout: List[List[T]],
      coordinate: Coordinate
  ): T =
    layout(coordinate.y)(coordinate.x)

  private def setByCoordinate[T <: WithCoordinate](
      layout: List[List[T]],
      value: T
  ): List[List[T]] =
    layout.updated(
      value.coordinate.y,
      layout(value.coordinate.y).updated(value.coordinate.x, value)
    )

  def apply(layout: List[List[Pipe]]): PipeMaze = {
    val vertBound = layout.length - 1
    val horBound = layout(0).length - 1
    val start = layout
      .flatMap(_.find {
        _.pipeDirection match {
          case Start(_) => true
          case _        => false
        }
      })
      .head
    val nowhere: Option[Pipe] = None

    def blindCheck(
        move: (Coordinate, Int, Int) => Option[Coordinate],
        validDestinations: List[PipeDirection]
    ): Option[Pipe] =
      move(start.coordinate, horBound, vertBound)
        .fold(nowhere)(coord => {
          val nextPipe = layout(coord.y)(coord.x)
          nextPipe.pipeDirection match {
            case x if validDestinations.contains(x) => Some(nextPipe)
            case _                                  => None
          }
        })

    def advance(
        nextMoves: List[Pipe],
        maze: PipeMaze,
        steps: Int
    ): PipeMaze = {
      val unmarkedPipes =
        nextMoves.filterNot(m => maze.distances.isDefinedAt(m.coordinate))
      if (unmarkedPipes.isEmpty) maze
      else {
        val updatedDists =
          maze.distances ++ unmarkedPipes
            .map(p => (p.coordinate -> (steps + 1)))
            .toMap
        val nextPipes = unmarkedPipes
          .flatMap(pipe =>
            pipe.pipeDirection.moves(pipe.coordinate, horBound, vertBound)
          )
          .map(c => getByCoordinate[Pipe](layout, c))
        advance(nextPipes, maze.copy(distances = updatedDists), steps + 1)
      }
    }

    val startMoves = List(
      blindCheck(Coordinate.moveNorth, List(NS(), SW(), SE())),
      blindCheck(Coordinate.moveSouth, List(NS(), NE(), NW())),
      blindCheck(Coordinate.moveEast, List(NW(), SW(), EW())),
      blindCheck(Coordinate.moveWest, List(NE(), SE(), EW()))
    )

    val startType = startMoves match {
      case List(Some(_), Some(_), _, _) => NS()
      case List(Some(_), _, Some(_), _) => NE()
      case List(Some(_), _, _, Some(_)) => NW()
      case List(_, Some(_), Some(_), _) => SE()
      case List(_, Some(_), _, Some(_)) => SW()
      case List(_, _, Some(_), Some(_)) => EW()
      case _                            => Ground()
    }

    val maze = advance(
      startMoves.flatten,
      PipeMaze(distances = Map(start.coordinate -> 0)),
      0
    )
    println(s"SOL1: ${maze.distances.values.max}")
    // --------------SOL2---------------
    val normalizedLayout =
      setByCoordinate(layout, start.copy(pipeDirection = startType))
    printMaze(normalizedLayout)
    println("=======")

    scanMaze(normalizedLayout, maze)
    maze
  }

  def printMaze[T <: WithSymbol](input: List[List[T]]): Unit =
    input.foreach { case pipeList =>
      println(pipeList.map(_.toSymbol).mkString(" "))
    }

  def scanMaze(layout: List[List[Pipe]], pipeMaze: PipeMaze): Unit = {
    val maxX = layout.head.length - 1
    val maxY = layout.length - 1
    val baseEnclosed = (0 to maxY)
      .map(y =>
        (0 to maxX)
          .map(x => {
            val coord = Coordinate(x, y)
            if (pipeMaze.distances.isDefinedAt(coord))
              ConnectedPipe(getByCoordinate(layout, coord))
            else Undecided(coord)
          })
          .toList
      )
      .toList

    val bounded = boundedEnclosure(baseEnclosed)
    val outs = bounded.flatMap(ec =>
      ec.collect { case o: Outside =>
        o
      }
    )
    // gaps(pipeMaze.distances.keySet.toList, bounded, List.empty)
    val expanded = expandOutside(outs, bounded, maxX, maxY)
    printMaze(expanded)
    println(s"SOL2: ${expanded.map(line => line.count(e => !e.isDefind)).sum}")
  }

  def boundedEnclosure(
      enclosure: List[List[Enclosure]]
  ): List[List[Enclosure]] = {
    def turnOutside(enc: Enclosure): Enclosure =
      enc match {
        case Undecided(coord) => Outside(coord)
        case x                => x
      }

    val maxX = enclosure.head.length - 1
    val maxY = enclosure.length - 1
    enclosure.zipWithIndex.map {
      case (l, i) if (i == 0 || i == maxY) => l.map(turnOutside)
      case (l, i) =>
        val updatedHead = turnOutside(l.head)
        val updatedLast = turnOutside(l.last)
        (updatedHead +: l.tail).dropRight(1) :+ updatedLast
    }
  }

  
  def otherExpand(
      outside: List[Outside],
      enclosure: List[List[Enclosure]],
      maxX: Int,
      maxY: Int
  ): List[List[Enclosure]] = {
    outside match {
      case Nil => enclosure
      case out :: outs =>
        val surroundings = Coordinate.allMovesAsMap(out.coord, maxX, maxY)
        val surroundingsAsEnclosure =
          surroundings.view.mapValues(c => getByCoordinate(enclosure, c)).toMap
        val newOutsides = surroundingsAsEnclosure.collect {
          case (_, u: Undecided) => u.turnOutside
          // if pipe and going in the same general direction => ride the pipe
        }.toList

        val surroundingDirs = surroundingsAsEnclosure.view
          .filterKeys(List(North, South, East, West).contains)
          .toMap
        val maybeGaps = surroundingsAsEnclosure.view
          .filterKeys(List(North, South, East, West).contains)
          .toMap
          .collect {
            case (dir, cp: ConnectedPipe) if cp.pipe.connectedTowards(dir) =>
              Gap2.maybeGap(cp, enclosure, maxX, maxY)
          }.flatten.toList
          enclosure
    }
  }

  def goThroughGap2(gaps: List[Gap2], enclosure: List[List[Enclosure]], maxX: Int, maxY: Int, acc: List[Outside]) = 
    gaps match {
        case Nil => acc
        case g :: gs => acc
    }

  def expandGap(gap: Gap2, enclosure: List[List[Enclosure]], maxX: Int, maxY: Int): Unit = {

  }  

  def expandOutside(
      outside: List[Outside],
      enclosure: List[List[Enclosure]],
      maxX: Int,
      maxY: Int
  ): List[List[Enclosure]] = {
    outside match {
      case Nil => enclosure
      case out :: outs =>
        val surroundings = Coordinate.allMovesAsMap(out.coord, maxX, maxY)
        val surroundingsAsEnclosure =
          surroundings.view.mapValues(c => getByCoordinate(enclosure, c)).toMap
        val newOutsides = surroundingsAsEnclosure.collect {
          case (_, u: Undecided) => u.turnOutside
          // if pipe and going in the same general direction => ride the pipe
        }.toList

        val surroundingDirs = surroundingsAsEnclosure.view
          .filterKeys(List(North, South, East, West).contains)
          .toMap
        val xxs = surroundingDirs.collect {
          case (dir, c: ConnectedPipe)
              if c.pipe.pipeDirection.connectsCardinal.contains(
                dir
              ) => // going in the same direction
            val stdMoves =
              Coordinate.standardMoves(c.pipe.coordinate, maxX, maxY)
            val gapTowards = stdMoves.view
              .mapValues(c => getByCoordinate(enclosure, c))
              .toMap
              .filter(p =>
                p._2 match {
                  case _: ConnectedPipe => !c.pipe.connectedTowards(p._1)
                  case _                => false
                }
              )
            if (gapTowards.nonEmpty) {
              val gapStart = List(
                Gap(c, gapTowards.head._1, dir),
                Gap(
                  gapTowards.head._2.asInstanceOf[ConnectedPipe],
                  CardinalDirection.opposite(gapTowards.head._1),
                  dir
                )
              )
              gapStart.foreach(println)
              goThoughGaps(gapStart, maxX, maxY, enclosure, List.empty)
            } else List.empty
        }.flatten

        val xs = xxs.collect { case u: Undecided =>
          u.turnOutside
        }
        val allOutsides = newOutsides ++ xs
        println(s"Adding outsides: $allOutsides")
        val updatedEnclosure =
          allOutsides.foldLeft(enclosure)((acc, o) => setByCoordinate(acc, o))
        println("UPDATED")
        printMaze(updatedEnclosure)
        scala.io.StdIn.readLine()
        expandOutside(outs ++ allOutsides, updatedEnclosure, maxX, maxY)
    }
  }

  def goThoughGaps(
      gaps: List[Gap],
      maxX: Int,
      maxY: Int,
      enclosure: List[List[Enclosure]],
      openings: List[Enclosure]
  ): List[Enclosure] = {
    gaps match {
      case Nil => openings
      case g :: gs =>
        val op = goThroughGap(
          g.onPipe,
          g.direction,
          maxX,
          maxY,
          enclosure,
          g.gapTowards
        )
        goThoughGaps(
          gs,
          maxX,
          maxY,
          enclosure,
          op.fold(openings)(o => openings :+ o)
        )
    }
  }

  def goThroughGap(
      pipe: ConnectedPipe,
      direction: CardinalDirection,
      maxX: Int,
      maxY: Int,
      enclosure: List[List[Enclosure]],
      gapTowards: CardinalDirection
  ): Option[Enclosure] = {
    // determine direction to go through
    val stdMoves = Coordinate.standardMoves(pipe.pipe.coordinate, maxX, maxY)
    val nextUp = stdMoves.get(direction)
    val gapSide = stdMoves.get(gapTowards)
    val gapEnds = gapSide match {
      case None => None
      case Some(c) =>
        val enclosedValue = getByCoordinate(enclosure, c)
        enclosedValue match {
          case u: Undecided =>
            Some(u)
          case o: Outside => Some(o) // got back outside - dead end
          case _          => None
        }
    }
    if (gapEnds.isDefined) gapEnds
    else
      nextUp match {
        case None => None
        case Some(c) =>
          val enclosedValue = getByCoordinate(enclosure, c)
          enclosedValue match {
            case cp: ConnectedPipe =>
              val comingFrom = CardinalDirection.opposite(direction)
              val nextDir = cp.pipe.pipeDirection.connectsCardinal
                .filterNot(_ == comingFrom)
                .head
              val nextGapDir =
                if (nextDir == direction) gapTowards // going the same way
                else
                  rotateDir(
                    direction,
                    nextDir,
                    gapTowards
                  ) // gap changed direction
              goThroughGap(cp, nextDir, maxX, maxY, enclosure, nextGapDir)
            case _ => None
          }

      }
  }

  private def rotateDir(
      directionChangeFrom: CardinalDirection,
      directionChangedTo: CardinalDirection,
      gapTowards: CardinalDirection
  ): CardinalDirection =
    if (gapTowards == directionChangedTo)
      CardinalDirection.opposite(directionChangeFrom)
    else directionChangeFrom

}
