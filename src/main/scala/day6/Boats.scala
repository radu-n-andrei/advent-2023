package day6

import scala.io.Source

object Boats {

  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day6.input")
    val input = file.getLines().toList
    val races = parseInput(input)
    println(s"SOL1: ${races.map(_.winPossibilities).product}")
    val singleRace = Race(races
      .foldLeft(("", ""))((acc, r) =>
        (acc._1 + r.time.toString, acc._2 + r.distance.toString)
      ))
    println(s"SOL2: ${singleRace.winPossibilities}")  
      
    file.close()
  }

  private def parseInput(input: List[String]): List[Race] = {
    val times = input(0)
      .replace("Time:", "")
      .trim
      .split(" ")
      .filter(_.nonEmpty)
      .map(_.toInt)
      .toList
    val dists = input(1)
      .replace("Distance:", "")
      .trim
      .split(" ")
      .filter(_.nonEmpty)
      .map(_.toInt)
      .toList
    times.zip(dists).map { case (time, distance) =>
      Race(time, distance)
    }
  }
}
