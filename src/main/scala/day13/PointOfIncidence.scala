package day13

import scala.io.Source

object PointOfIncidence {

  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day13.input")
    val lines = file.getLines().toList
    val input = readInput(lines, List.empty, List.empty)
    println(s"SOL1: ${input.map(_.evaluate).sum}")
    println(s"SOL2: ${input.map(_.evaluateSmudge).sum}")
    file.close
  }

  private def readInput(
      in: List[String],
      acc: List[Mapping],
      buffer: List[String]
  ): List[Mapping] =
    in match {
      case Nil => acc :+ Mapping(buffer)
      case i :: ins if i.isEmpty =>
        readInput(ins, acc :+ Mapping(buffer), List.empty)
      case i :: ins => readInput(ins, acc, buffer :+ i)
    }

}
