package day10

final case class PipeEnclosure(
    enclosure: List[List[Enclosure]]
) {
  val maxX: Int = enclosure.headOption.fold(0)(_.length - 1)
  val maxY: Int = enclosure.length - 1

  def boundedEnclosure(
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

  def scanMaze: PipeEnclosure = {
    val pipes = enclosure.flatMap(e =>
      e.collect { case c: ConnectedPipe =>
        c
      }
    )
    val g = gaps(pipes, List.empty)
    val bounded = boundedEnclosure()
    val outs = bounded.flatMap(encs =>
      encs.collect { case o: Outside =>
        o
      }
    )
    PipeEnclosure(expandOutside(outs, bounded, g))
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
        val (undefs, explored) =
          goThroughGap(potentialGaps, gaps, List.empty, List.empty)
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
              u
            }
        val endsAsGaps = ends(0).zip(ends(1)).map(c => EnclosureGap(c._1, c._2))
        val potentialGaps = allGaps.filter(eg =>
          !explored.contains(eg) && // not explored
            (endsAsGaps.exists(e => e.eq(eg) // end is a direct continuation
            ) ||
              validCorner(eg, g)) // the currernt gap forms a corner
        )
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

object PipeEnclosure {
  def apply(layout: List[List[Pipe]], maze: PipeMaze): PipeEnclosure = {
    val normalizedLayout =
      PipeMaze.setByCoordinate(
        layout,
        maze.start.copy(pipeDirection = maze.startType)
      )
    val maxY = layout.length - 1
    val maxX = layout.headOption.fold(0)(_.length - 1)
    val baseEnclosed = (0 to maxY)
      .map(y =>
        (0 to maxX)
          .map(x => {
            val coord = Coordinate(x, y)
            if (maze.distances.isDefinedAt(coord))
              ConnectedPipe(PipeMaze.getByCoordinate(normalizedLayout, coord))
            else Undecided(coord)
          })
          .toList
      )
      .toList
    PipeEnclosure(baseEnclosed)
  }
}
