package day5

import scala.io.Source
import scala.annotation.tailrec

object Gardener {

  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day5.input")
    val input = file.getLines().toList
    val almanac = readAlmanac(input, Almanac.empty, None)

    println(s"SOL1: ${almanac.seedLocations.min}")
    
    println(s"SOL2: ${almanac.seeds
        .sliding(2, 2)
        .toList
        .map(l =>
          AlmanacRange(almanac, l(0), l(0) + l(1) - 1)
            .minBy(_.rangeStart)
            .rangeStart
        )
        .min}")
    file.close()
  }

  @tailrec
  private def readAlmanac(
      input: List[String],
      almanac: Almanac,
      writingAt: Option[AlmanacHeader]
  ): Almanac = {
    input match {
      case Nil => almanac
      case a :: as =>
        updateAlmanac(a, almanac, writingAt) match {
          case Right(al) => readAlmanac(as, al, writingAt)
          case Left(ah)  => readAlmanac(as, almanac, Some(ah))
        }
    }
  }

  private def updateAlmanac(
      s: String,
      almanac: Almanac,
      currentHeader: Option[AlmanacHeader]
  ): Either[AlmanacHeader, Almanac] = {
    val seedR = "seeds:\\s+(.*)".r
    s match {
      case seedR(seedInput) =>
        Right(
          almanac.copy(seeds =
            seedInput.trim().split("\\s").map(BigInt(_)).toList
          )
        )
      case AlmanacHeader(h) =>
        Left(h)
      case line if line.trim.nonEmpty =>
        Right(currentHeader.fold(almanac)(h => almanac.updateMap(line, h)))
      case _ => Right(almanac)
    }
  }
}
