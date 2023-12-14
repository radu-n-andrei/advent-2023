package day14

import scala.io.Source

object ParabolicDish {
  
    def main(args: Array[String]): Unit = {
        val file = Source.fromResource("day14.input")
        val lines = file.getLines().toList
        val platform = readInput(lines, List.empty)
        println(s"SOL1: ${platform.tiltNorth.score}")
        val superCycled = platform.cycle(1000000000)
        println(s"SOL2: ${superCycled.score}")
        
        file.close()
    }

    private def readInput(lines: List[String], acc: List[List[Rock]]): Platform = 
        lines match {
            case Nil => Platform(acc)
            case l :: ls => readInput(ls, acc :+ l.map(Rock(_)).toList)
        }
}
