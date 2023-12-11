package day11

import scala.annotation.tailrec

final case class SpaceMap(elements: List[List[SpaceMapping]]) {
  def print(): Unit =
    elements.foreach(l => println(l.map(_.symbol).mkString(" ")))

  val emptyLines = elements.zipWithIndex
    .filter(_._1.count {
      case g: Galaxy =>
        true
      case _ => false
    } == 0)
    .map(_._2)

  val galaxies = elements.flatten.collect { case g: Galaxy =>
    g
  }

  val emptyColumns = {
    val galaxyColumns = galaxies.map(_.x).distinct
    (0 to elements.head.length).filterNot(galaxyColumns.contains)
  }

  def distances(times: Int): BigInt = {
    galaxies
      .combinations(2)
      .map(pair => {
        val sortedX = pair.sortBy(_.x)
        val sortedY = pair.sortBy(_.y)
        val deltaX = sortedX(1).x - sortedX(0).x
        val deltaY = sortedY(1).y - sortedY(0).y
        val doubledRows =
          emptyLines.count(l => l > sortedY(0).y && l < sortedY(1).y)
        val doubledCols =
          emptyColumns.count(c => c > sortedX(0).x && c < sortedX(1).x)
        deltaX + deltaY + BigInt(doubledRows * (times - 1)) + BigInt(doubledCols * (times - 1))
      }).reduce(_ + _)
  }
}

object SpaceMap {

  def fromLines(input: List[String]): SpaceMap =
    SpaceMap(input.zipWithIndex map { case (str, i) =>
      readLine(str.toCharArray().toList, i, 0, List.empty)
    })

  @tailrec
  private def readLine(
      str: List[Char],
      lineIndex: Int,
      currentIndex: Int,
      acc: List[SpaceMapping]
  ): List[SpaceMapping] =
    str match {
      case Nil => acc
      case c :: cs =>
        readLine(
          cs,
          lineIndex,
          currentIndex + 1,
          acc :+ SpaceMapping(c, currentIndex, lineIndex)
        )
    }
}
