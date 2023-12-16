package day16

import scala.io.Source

object FloorWillBeLava {
  
    def main(args: Array[String]): Unit = {
        val file = Source.fromResource("day16.input")
        val input = file.getLines().toList
        val floorLayout = readInput(input, List.empty)
        println(s"SOL1: ${floorLayout.energizeFrom(Coordinate(0, 0), West)}")
        println(s"SOL2: ${floorLayout.maxEnergy}")
        file.close
    }

    private def readInput(input: List[String], acc: List[List[Tile]]): FloorLayout = 
        input match {
            case Nil => FloorLayout(acc)
            case in :: ins => readInput(ins, acc :+ in.map(Tile(_)).toList)
        }
}
