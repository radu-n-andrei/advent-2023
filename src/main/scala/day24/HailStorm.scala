package day24

import scala.io.Source

object HailStorm {
  
    def main(args: Array[String]): Unit = {
        val file = Source.fromResource("day24.input")
        val input = file.getLines().toList
        val obs = Observation(input.map(Hail(_)), 200000000000000d, 400000000000000d)
        println(s"SOL1: ${obs.intersections}")
        file.close()
    }
}
