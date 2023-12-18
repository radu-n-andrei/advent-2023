package day18

import scala.io.Source

object LavaductLagoon {
  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day18.input")
    val input = file.getLines().toList
    val orders = input.map(TrenchOrder(_))
    val lagoon = Lagoon.fromTrench(Digger.dig(orders))
    println(MegaLagoon.surface(orders))
    println(MegaLagoon.surface(orders.map(_.decode)))
    file.close()

  }

}
