package day1

import scala.io.Source

object Trebuchet {

  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day1.input")
    val inputList = file.getLines().toList
    val s1 = inputList.foldLeft(0)((acc, i) =>
      acc + findCoordinate(i.toList, None, None, false)
    )
    val s2 = inputList.foldLeft(0)((acc, i) =>
      acc + findCoordinate(i.toList, None, None, true)
    )
    println(s"SOL 1: $s1")
    println(s"SOL 2: $s2")
  }

  private def findCoordinate(
      s: List[Char],
      firstCoord: Option[String],
      lastCoord: Option[String],
      withText: Boolean
  ): Int =
    s match {
      case Nil => (firstCoord.getOrElse("0") ++ lastCoord.getOrElse("0")).toInt
      case x :: xs if x.isDigit =>
        findCoordinate(
          xs,
          firstCoord.orElse(Some(x.toString())).map(identity),
          Some(x.toString()),
          withText
        )
      case FullDigit(n) if withText =>
        findCoordinate(
          s.tail,
          firstCoord.orElse(Some(n)).map(identity),
          Some(n),
          withText
        )
      case _ :: xs =>
        findCoordinate(
          xs,
          firstCoord = firstCoord,
          lastCoord = lastCoord,
          withText
        )
    }

}
