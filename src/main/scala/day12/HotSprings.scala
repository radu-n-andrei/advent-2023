package day12

import scala.io.Source

object HotSprings {
  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day12.input")
    val input = file.getLines().toList
    val springRows = readInput(input, List.empty)
    val multiplied = springRows.map(s => SpringRow.multiplyBy(s, 5))
    println(s"SOL1: ${springRows.map(_.evaluateWithMaps).sum}")
    println(s"SOL2: ${multiplied.map(_.evaluateWithMaps).sum}")
    file.close()
  }

  private def readInput(
      input: List[String],
      acc: List[SpringRow]
  ): List[SpringRow] =
    input match {
      case Nil          => acc
      case in :: inputs => readInput(inputs, acc :+ SpringRow(in))
    }

  private def print(springRows: List[SpringRow]): Unit =
    springRows.foreach(r => println(r.toString))
}
