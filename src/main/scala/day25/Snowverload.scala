package day25

import scala.io.Source

object Snowverload {
  
    def main(args: Array[String]): Unit = {
        val file = Source.fromResource("day25.input")
        val input = file.getLines().toList
        val configs = input.map(WireConfiguration(_))
        val layout = Layout(configs)
        layout.componentGroups.foreach {
            g => 
                println(s"GROUP: ${g.mkString(", ")}")
        }
        file.close()
    }

    
}
