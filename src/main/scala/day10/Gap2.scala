package day10

final case class Gap2(pipe1: ConnectedPipe, pipe2: ConnectedPipe) {

  def moveNorth(enclosure: List[List[Enclosure]]): List[Gap2] = {
    val maxX = enclosure.head.length - 1
    val maxY = enclosure.length - 1
    val p1 = Coordinate.moveNorth(pipe1.pipe.coordinate, maxX, maxY)
    val p2 = Coordinate.moveNorth(pipe2.pipe.coordinate, maxX, maxY)
    if(p1.isDefined && p2.isDefined) {
        List(p1.get, p2.get).map(p => PipeMaze.getByCoordinate[Enclosure](enclosure, p)).collect {
            case c: ConnectedPipe => c
        }.flatMap(cp => Gap2.maybeGap(cp, enclosure, maxX, maxY))
    }
    else List.empty
  }

}

object Gap2 {

  def maybeGap(
      connectedPipe: ConnectedPipe,
      enclosure: List[List[Enclosure]],
      maxX: Int,
      maxY: Int
  ): List[Gap2] = {
    val stdDirs =
      Coordinate.standardMoves(connectedPipe.pipe.coordinate, maxX, maxY)
    val stdDirsAsEnc =
      stdDirs.view.mapValues(c => PipeMaze.getByCoordinate(enclosure, c))
    stdDirsAsEnc.collect {
      case (dir, p: ConnectedPipe)
          if !connectedPipe.pipe.connectedWith(p.pipe, dir) =>
        Gap2(connectedPipe, p)
    }.toList
  }

}
