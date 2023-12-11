package day10

final case class PipeEnclosure(
    enclosure: List[List[Enclosure]],
    start: Coordinate
) {
  val maxX: Int = enclosure.headOption.fold(0)(_.length - 1)
  val maxY: Int = enclosure.length - 1

  val g1 = EnclosureGap(Coordinate(35, 13), Coordinate(35, 14))
    val g2 = EnclosureGap(Coordinate(36, 13), Coordinate(36, 14))
    val g3 = EnclosureGap(Coordinate(37, 13), Coordinate(37, 14))
    val g4 = EnclosureGap(Coordinate(38, 13), Coordinate(38, 14))

  def scanMaze: Unit = {
    // get left & right side coords moving clockwise (N -> E -> S -> W)
    // val startPipe =
    //   PipeMaze.getByCoordinate(enclosure, start).asInstanceOf[ConnectedPipe]
    // val comingFrom = startPipe.pipe.pipeDirection match {
    //   case NS(_) => South
    //   case NE(_) => West
    //   case NW(_) => East
    //   case SW(_) => West
    //   case SE(_) => South
    //   case EW(_) => West
    //   case _     => throw new RuntimeException(s"Illegal start pipe $startPipe")
    // }
    // val (left, right) = sides(startPipe, List.empty, List.empty, comingFrom)
    // val leftOut = isOutside(left.distinct)
    // val rightOut = isOutside(right.distinct)
    // val enc1 =
    //   leftOut.foldLeft(enclosure) { (acc, c) =>
    //     PipeMaze.setByCoordinate(acc, Outside(c))
    //   }

    // val enc2 =
    //   rightOut.foldLeft(enc1) { (acc, c) =>
    //     PipeMaze.setByCoordinate(acc, Outside(c))
    //   }
    val pipes = enclosure.flatMap(e =>
      e.collect { case c: ConnectedPipe =>
        c
      }
    )
    val g = gaps(pipes, List.empty)
    
    println(s"G1: ${g.contains(g1) || g.contains(g1.reversed)}")
    println(s"G2: ${g.contains(g2) || g.contains(g2.reversed)}")
    println(s"G3: ${g.contains(g3) || g.contains(g3.reversed)}")
    println(s"G4: ${g.contains(g4) || g.contains(g4.reversed)}")
    val bounded = PipeMaze.boundedEnclosure(enclosure)
    val outs = bounded.flatMap(encs =>
      encs.collect { case o: Outside =>
        o
      }
    )
    val expanded =
      expandOutside(outs, bounded, g)
    PipeMaze.printMaze(expanded)
    println(s"SOL2: ${expanded.map(e =>
          e.count {
            case Undecided(_) => true
            case _            => false
          }
        )
        .sum}")

  }

  private def sides(
      currentPipe: ConnectedPipe,
      left: List[Coordinate],
      right: List[Coordinate],
      comingFrom: CardinalDirection
  ): (List[Coordinate], List[Coordinate]) =
    if (
      currentPipe.pipe.coordinate == start && (left.nonEmpty || right.nonEmpty)
    ) (left, right)
    else {
      val pipeCoord = currentPipe.pipe.coordinate
      // determine left and right of the pipe
      val (l, r, switchSides, nextPipe, nextComingFrom) =
        currentPipe.pipe.pipeDirection match {
          case NS(_) => // |
            (
              List(Coordinate.moveWest(pipeCoord, maxX, maxY)),
              List(Coordinate.moveEast(pipeCoord, maxX, maxY)),
              comingFrom == North,
              if (comingFrom == South)
                Coordinate.moveNorth(pipeCoord, maxX, maxY)
              else Coordinate.moveSouth(pipeCoord, maxX, maxY),
              comingFrom
            )
          case NE(_) =>
            ( // L
              List(
                Coordinate.moveSouth(pipeCoord, maxX, maxY),
                Coordinate.moveWest(pipeCoord, maxX, maxY),
                Coordinate.moveSouthWest(pipeCoord, maxX, maxY)
              ),
              List(Coordinate.moveNorthEast(pipeCoord, maxX, maxY)),
              comingFrom == North,
              if (comingFrom == East)
                Coordinate.moveNorth(pipeCoord, maxX, maxY)
              else Coordinate.moveEast(pipeCoord, maxX, maxY),
              if (comingFrom == East) South else West
            )
          case NW(_) =>
            ( // J
              List(Coordinate.moveNorthWest(pipeCoord, maxX, maxY)),
              List(
                Coordinate.moveEast(pipeCoord, maxX, maxY),
                Coordinate.moveSouth(pipeCoord, maxX, maxY),
                Coordinate.moveSouthEast(pipeCoord, maxX, maxY)
              ),
              comingFrom == North,
              if (comingFrom == West)
                Coordinate.moveNorth(pipeCoord, maxX, maxY)
              else Coordinate.moveWest(pipeCoord, maxX, maxY),
              if (comingFrom == West) South else East
            )
          case SW(_) =>
            ( // 7
              List(Coordinate.moveSouthWest(pipeCoord, maxX, maxY)),
              List(
                Coordinate.moveEast(pipeCoord, maxX, maxY),
                Coordinate.moveNorth(pipeCoord, maxX, maxY),
                Coordinate.moveNorthEast(pipeCoord, maxX, maxY)
              ),
              comingFrom == West,
              if (comingFrom == West)
                Coordinate.moveSouth(pipeCoord, maxX, maxY)
              else Coordinate.moveWest(pipeCoord, maxX, maxY),
              if (comingFrom == West) North else East
            )
          case SE(_) =>
            ( // F
              List(
                Coordinate.moveNorth(pipeCoord, maxX, maxY),
                Coordinate.moveWest(pipeCoord, maxX, maxY),
                Coordinate.moveNorthWest(pipeCoord, maxX, maxY)
              ),
              List(Coordinate.moveSouthEast(pipeCoord, maxX, maxY)),
              comingFrom == East,
              if (comingFrom == East)
                Coordinate.moveSouth(pipeCoord, maxX, maxY)
              else Coordinate.moveEast(pipeCoord, maxX, maxY),
              if (comingFrom == East) North else West
            )
          case EW(_) => // -
            (
              List(Coordinate.moveNorth(pipeCoord, maxX, maxY)),
              List(Coordinate.moveSouth(pipeCoord, maxX, maxY)),
              comingFrom == West,
              if (comingFrom == West) Coordinate.moveEast(pipeCoord, maxX, maxY)
              else Coordinate.moveWest(pipeCoord, maxX, maxY),
              comingFrom
            )
          case _ => (Nil, Nil, false, None, North)
        }
      val (dLeft, dRight) = if (switchSides) (r, l) else (l, r)

      val nextPipeEnc =
        nextPipe.map(c => PipeMaze.getByCoordinate(enclosure, c)).collect {
          case cp: ConnectedPipe => cp
        }
      if (!nextPipeEnc.isDefined)
        throw new RuntimeException(
          s"Illegal next pipe from ${currentPipe.coordinate} => $nextPipe"
        )
      sides(
        nextPipeEnc.get,
        left ++ parseUndecided(dLeft),
        right ++ parseUndecided(dRight),
        nextComingFrom
      )
    }

  private def parseUndecided(in: List[Option[Coordinate]]): List[Coordinate] =
    in.flatten.map(c => PipeMaze.getByCoordinate(enclosure, c)).collect {
      case Undecided(coord) => coord
    }

  private def isOutside(coords: List[Coordinate]): List[Coordinate] = {
    def moveTowardsOutside(
        coord: Coordinate,
        direction: CardinalDirection
    ): Boolean = {
      val nextCoord = Coordinate.moveTowards(coord, direction, maxX, maxY)
      nextCoord.fold(true) { c =>
        PipeMaze.getByCoordinate(enclosure, c) match {
          case _: Undecided => moveTowardsOutside(c, direction)
          case _            => false
        }
      }
    }
    def checkSingle(coord: Coordinate): Boolean = {
      moveTowardsOutside(coord, East) || moveTowardsOutside(
        coord,
        West
      ) || moveTowardsOutside(coord, North) || moveTowardsOutside(coord, South)
    }

    coords.filter(checkSingle)
  }

  def expandOutside(
      outside: List[Outside],
      pe: List[List[Enclosure]],
      gaps: List[EnclosureGap]
  ): List[List[Enclosure]] = {
    outside match {
      case Nil => pe
      case out :: outs =>
        val surroundings = Coordinate.allMovesAsMap(out.coord, maxX, maxY)
        val surroundingsAsEnclosure =
          surroundings.view
            .mapValues(c => PipeMaze.getByCoordinate(pe, c))
            .toMap
        val newOutsides = surroundingsAsEnclosure.collect {
          case (_, u: Undecided) => u.turnOutside

        }.toList
        val potentialGaps =
          gaps.filter(g =>
            // if it contains both sides of the gap it can explore it
            surroundings.values.toList.contains(
              g.pipe1
            ) && surroundings.values.toList.contains(g.pipe2)
          )
        if(out.coord == Coordinate(34,13)) {
          println(surroundings)
          println(s"at 34,13 with $potentialGaps")  
        scala.io.StdIn.readLine()}
        // if (potentialGaps.nonEmpty) println(s"$out found gaps: $potentialGaps")
        // scala.io.StdIn.readLine()
        val (undefs, explored) =
          goThroughGap(potentialGaps, gaps, List.empty, List.empty)
        if(explored.exists(e => e.eq(g1))) {
          println(s"Gap 1 explored aleady by ${out.coord}")
          scala.io.StdIn.readLine()
        }  
        val undefsAsOutside = undefs.map(u => u.turnOutside)
        val allOutsides = newOutsides ++ undefsAsOutside
        val updatedEnclosure =
          allOutsides.foldLeft(pe)((acc, o) => PipeMaze.setByCoordinate(acc, o))
        val updatedGaps = gaps.filterNot(explored.contains)
        expandOutside(allOutsides ++ outs, updatedEnclosure, updatedGaps)
    }
  }

  def goThroughGap(
      gapsToExplore: List[EnclosureGap],
      allGaps: List[EnclosureGap],
      explored: List[EnclosureGap],
      discoveries: List[Undecided]
  ): (List[Undecided], List[EnclosureGap]) = {

    def validCorner(gap1: EnclosureGap, gap2: EnclosureGap): Boolean = {
      val diff = gap1.diff(gap2)
      if (diff.length == 1) {
        val otherDiff = gap2.diff(gap1)
        val isOne = Math.abs(diff.head.x - otherDiff.head.x) == 1 && Math.abs(
          diff.head.y - otherDiff.head.y
        ) == 1

        isOne
      } else false
    }

    gapsToExplore match {
      case Nil =>
        (discoveries, explored)
      case g :: gs =>
        if(g.eq(g1)) println(s"Entering G1")
        val ends = if (g.isVertical) { // check N and S of both ends
          List(g.pipe1, g.pipe2)
            .map(c =>
              List(
                Coordinate.moveNorth(c, maxX, maxY),
                Coordinate.moveSouth(c, maxX, maxY)
              ).flatten
            )

        } else {
          List(g.pipe1, g.pipe2)
            .map(c =>
              List(
                Coordinate.moveEast(c, maxX, maxY),
                Coordinate.moveWest(c, maxX, maxY)
              ).flatten
            )
        }
        val undefs =
          ends.flatten
            .map(c => PipeMaze.getByCoordinate(enclosure, c))
            .collect { case u: Undecided =>
              if(u.coord == Coordinate(39, 14)) println("FUND YU")
              u
            }
        // if (undefs.nonEmpty) {
        //   println(s"Found ${undefs} through a gap")
        // }
        // println(s"Ends: $ends")
        val endsAsGaps = ends(0).zip(ends(1)).map(c => EnclosureGap(c._1, c._2))
        val potentialGaps = allGaps.filter(eg =>
          !explored.contains(eg) && // not explored
            (endsAsGaps.exists(e => e.eq(eg) // end is a direct continuation
            ) ||
              validCorner(eg, g)) // the currernt gap forms a corner
        )
        // println(s"NEW GAPS: $potentialGaps")
        // scala.io.StdIn.readLine()
        goThroughGap(
          potentialGaps ++ gs,
          allGaps,
          g +: explored,
          discoveries ++ undefs
        )

    }
  }

  def gaps(
      pipes: List[ConnectedPipe],
      acc: List[(Coordinate, Coordinate)]
  ): List[EnclosureGap] = {
    pipes match {
      case Nil => acc.map(a => EnclosureGap(a._1, a._2))
      case p :: ps =>
        val std = Coordinate.standardMoves(p.pipe.coordinate, maxX, maxY)
        val onlyOtherPipes = std.view
          .mapValues { c =>
            PipeMaze.getByCoordinate(enclosure, c)
          }
          .collect { case (dir, c: ConnectedPipe) =>
            dir -> c
          }
          .filter { case (dir, cp) =>
            p.pipe.hasGapWith(cp.pipe, dir)
          }
          .toMap
          .values
          .map { cp =>
            p.pipe.coordinate -> cp.pipe.coordinate
          }
          .toList
        val filtered = onlyOtherPipes
          .filter { case (p1, p2) =>
            !acc.contains((p1, p2)) && !acc.contains((p2, p1))
          }
        gaps(ps, acc ++ filtered)
    }
  }
}
