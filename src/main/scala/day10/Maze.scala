package day10

import scala.io.Source

object Maze {

  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day10.input")
    val input = file.getLines().toList
    val maxX = input.head.length()
    val maxY = input.size
    val pipes = readMap(input, List.empty, 0)
    val pipeMaze = PipeMaze(pipes)
    println(s"SOL1: ${pipeMaze.distances.values.max}")
    val pipeEnclosure = PipeEnclosure(pipes, pipeMaze)
    println(s"SOL2: ${pipeEnclosure.scanMaze.enclosure
        .map(e =>
          e.count {
            case Undecided(_) => true
            case _            => false
          }
        )
        .sum}")
    file.close()
  }

  private def readMap(
      input: List[String],
      acc: List[List[Pipe]],
      currentY: Int
  ): List[List[Pipe]] =
    input match {
      case Nil => acc
      case in :: ins =>
        val line = in.zipWithIndex.toList.map { case (c, x) =>
          Pipe(x, currentY, c)
        }
        readMap(ins, acc :+ line, currentY + 1)
    }
}
