package day11

import scala.io.Source

object CosmicExpansion {

    def main(args: Array[String]): Unit = {
        val file = Source.fromResource("day11.input")
        val input = file.getLines().toList
        val map = SpaceMap.fromLines(input)
        println(s"SOL1: ${map.distances(2)}")
        println(s"SOL1: ${map.distances(1000000)}")
        file.close()
    }
  
}
