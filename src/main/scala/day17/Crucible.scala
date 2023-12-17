package day17

import scala.io.Source

object Crucible {
  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day17.input")
    val input = file.getLines().toList
    val rawHeatMap = input.map(_.map(_.toString.toInt).toList)
    val heatMap = HeatMap(rawHeatMap)
    heatMap.shortestRoute(3, false)
    heatMap.shortestRoute(10, true)
  }
}
