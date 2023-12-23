package day23

import scala.io.Source

object LongWalk {
  
    def main(args: Array[String]): Unit = {
        val file = Source.fromResource("day23.input")
        val input = file.getLines().toList
        val layout = Layout(input)
        //layout.longestPath
        //layout.desloped.print
        layout.desloped.longestPath
        file.close
    }
}
