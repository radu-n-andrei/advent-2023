package day20

import scala.io.Source

object PulseProp {
  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day20.input")
    val input = file.getLines().toList
    val initialConfiguration = ModuleConfiguration(input)
    ModuleExecution.execute(initialConfiguration, 1000)
    ModuleExecution.minToModule(initialConfiguration, "rx")
    file.close()
  }
}
