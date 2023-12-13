package day10

final case class PipeEnclosure(
    enclosure: List[List[Enclosure]]
) {
  val maxX: Int = enclosure.headOption.fold(0)(_.length - 1)
  val maxY: Int = enclosure.length - 1

  /**
    * This enclosure with all non-pipe elements on the edges set to Outside
    * @return
    */
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

  /**
    * Attempt to expand the outside inwards from each outside space
    *
    * @param outside
    * @param pe
    * @param gaps
    * @return
    */
  def expandOutside(
      outside: List[Outside],
      pe: List[List[Enclosure]],
      gaps: List[EnclosureGap]
  ): List[List[Enclosure]] = {
    outside match {
      case Nil => pe // no more Outside spaces to expand -> solution
      case out :: outs =>
        val surroundings = Coordinate.allMovesAsMap(out.coord, maxX, maxY)
        // determine the type of all surrounding coordinates
        val surroundingsAsEnclosure =
          surroundings.view
            .mapValues(c => PipeMaze.getByCoordinate(pe, c))
            .toMap
        // mark all Undecided spaces as outside    
        val newOutsides = surroundingsAsEnclosure.collect {
          case (_, u: Undecided) => u.turnOutside
        }.toList
        // check if there are any gaps to explore around the current space
        val potentialGaps =
          gaps.filter(g =>
            // if it contains both sides of the gap it can explore it
            surroundings.values.toList.contains(
              g.pipe1
            ) && surroundings.values.toList.contains(g.pipe2)
          )
        // fully explore the gaps leading away from the current space  
        val (undefs, explored) =
          goThroughGap(potentialGaps, gaps, List.empty, List.empty)
        // mark all of the Undecided spaces found through gaps as Outside  
        val undefsAsOutside = undefs.map(u => u.turnOutside)
        val allOutsides = newOutsides ++ undefsAsOutside
        // update the enclosure
        val updatedEnclosure =
          allOutsides.foldLeft(pe)((acc, o) => PipeMaze.setByCoordinate(acc, o))
        // remove explored gaps  
        val updatedGaps = gaps.filterNot(explored.contains)
        // carry on
        expandOutside(allOutsides ++ outs, updatedEnclosure, updatedGaps)
    }
  }

  /**
    * Traverse a gap, continuing on until it ends
    *
    * @param gapsToExplore
    * @param allGaps
    * @param explored
    * @param discoveries
    * @return
    */
  def goThroughGap(
      gapsToExplore: List[EnclosureGap],
      allGaps: List[EnclosureGap],
      explored: List[EnclosureGap],
      discoveries: List[Undecided]
  ): (List[Undecided], List[EnclosureGap]) = {

    /**
      * A corner is possible between 2 gaps if the share a coordinate and the 
      * absolute difference between the non-matching coordinates is (1,1)
      *
      * @param gap1
      * @param gap2
      * @return
      */
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
        (discoveries, explored) // no more gaps available - return the list of Undecided spaces and the explored gaps
      case g :: gs =>
        val ends = if (g.isVertical) { 
          // advance N and S from both ends
          List(g.pipe1, g.pipe2)
            .map(c =>
              List(
                Coordinate.moveNorth(c, maxX, maxY),
                Coordinate.moveSouth(c, maxX, maxY)
              ).flatten
            )

        } else {
          // advance E and W from both ends
          List(g.pipe1, g.pipe2)
            .map(c =>
              List(
                Coordinate.moveEast(c, maxX, maxY),
                Coordinate.moveWest(c, maxX, maxY)
              ).flatten
            )
        }
        // collect Undecided spaces that are one step away from this gap in its traversal direction
        val undefs =
          ends.flatten
            .map(c => PipeMaze.getByCoordinate(enclosure, c))
            .collect { case u: Undecided =>
              u
            }
        val endsAsGaps = ends(0).zip(ends(1)).map(c => EnclosureGap(c._1, c._2))
        // find all unexplored gaps leading away from the current one
        // valid gaps are either direct continuations eg NS+NS -> NS+NS or corners NS+NE -> EW+NE
        val potentialGaps = allGaps.filter(eg =>
          !explored.contains(eg) && // not explored
            (endsAsGaps.exists(e => e.eq(eg) // end is a direct continuation
            ) ||
              validCorner(eg, g)) // the currernt gap forms a corner
        )
        // update gaps and carry on
        goThroughGap(
          potentialGaps ++ gs,
          allGaps,
          g +: explored,
          discoveries ++ undefs
        )

    }
  }

  /**
    * Determine all of the gaps between the valid pipes
    *
    * @param pipes
    * @param acc
    * @return
    */
  def gaps(
      pipes: List[ConnectedPipe],
      acc: List[(Coordinate, Coordinate)]
  ): List[EnclosureGap] = {
    pipes match {
      case Nil => acc.map(a => EnclosureGap(a._1, a._2)) 
      case p :: ps =>
        val std = Coordinate.standardMoves(p.pipe.coordinate, maxX, maxY) // NSEW coordinates from the current pipe
        // all pipes surrounding the current pipe that form a gap with it
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
        // filter out duplicates  
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
