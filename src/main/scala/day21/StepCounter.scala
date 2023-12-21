package day21

import scala.io.Source

object StepCounter {
  
    def main(args: Array[String]): Unit = {
        val file = Source.fromResource("day21.input")
        val input = file.getLines().toList
        val garden = Garden(input)
        println(garden.getSteps(64))
        println(garden.getMegaSteps(26501365))
        file.close()
    }
}
