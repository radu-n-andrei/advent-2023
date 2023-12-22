package day22

import scala.io.Source

object SandSlabs {

  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day22.input")
    val input = file.getLines().toList
    val wall = BrickWall(input.map(Brick(_)))
    wall.lowerBricks
    file.close()
  }
}
