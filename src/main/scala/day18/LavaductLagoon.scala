package day18

import scala.io.Source

object LavaductLagoon {
  def main (args: Array[String]): Unit = {
    val file = Source.fromResource("day18.input")
    val input = file.getLines().toList
    val orders = input.map(TrenchOrder(_))
    val lagoon = Lagoon.fromTrench(Digger.dig(orders))
    //val filled = lagoon.fill
    println(s"W: ${lagoon.width}; h: ${lagoon.height}")
    //println(s"SOL1: ${filled.volume}")
    val updatedOrders = orders.map(_.decode)  
    val megaLagoon = MegaLagoon.fromOrders(updatedOrders)
    println(s"mW: ${megaLagoon.minWidth} MW: ${megaLagoon.width}; mH: ${megaLagoon.minHeight} MH: ${megaLagoon.height}")
  }

}
