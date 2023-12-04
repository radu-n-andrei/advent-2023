package day2

import scala.io.Source

object CubeConundrum {

  val rules = GameRules(red = 12, green = 13, blue = 14)

  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day2.input")
    val games = file.getLines().map(Game(_)).toList
    println(
      s"SOL 1: ${games.filter(g => Game.isValid(g, rules)).map(_.id).sum}"
    )
    println(s"SOL 2: ${games.map(_.power).sum}")
    file.close()
  }
}
