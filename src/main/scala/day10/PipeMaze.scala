package day10

final case class PipeMaze(
    distances: Map[Coordinate, Int],
    start: Pipe, 
    startType: PipeDirection
)

object PipeMaze {

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
      PipeMaze(distances = Map(start.coordinate -> 0), start, startType),
      0
    )
    maze
  }

  def printMaze[T <: WithSymbol](input: List[List[T]]): Unit =
    input.foreach { case pipeList =>
      println(pipeList.map(_.toSymbol).mkString(""))
    }

  def getByCoordinate[T <: WithCoordinate](
      layout: List[List[T]],
      coordinate: Coordinate
  ): T =
    layout(coordinate.y)(coordinate.x)

  def setByCoordinate[T <: WithCoordinate](
      layout: List[List[T]],
      value: T
  ): List[List[T]] =
    layout.updated(
      value.coordinate.y,
      layout(value.coordinate.y).updated(value.coordinate.x, value)
    )  

}
