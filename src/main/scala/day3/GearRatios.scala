package day3

import scala.io.Source
import scala.annotation.tailrec

object GearRatios {

  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day3.input")
    val lines = file.getLines().toList
    val allParts = lines.zipWithIndex.flatMap(l =>
      parseLine(
        l._1.toCharArray().toList,
        List.empty[EngineSchematicObject],
        0,
        l._2
      )
    )

    val engineParts = allParts.collect { case ep: EnginePart =>
      ep
    }
    val symbols = allParts.collect { case s: Symbol =>
      s
    }

    println(
      s"SOL 1: ${engineParts
          .filter { p =>
            validPart(
              p,
              symbols.filter(s => s.lineIndex >= p.lineIndex - 1 && s.lineIndex <= p.lineIndex + 1)
            )
          }
          .map(_.number.toInt)
          .sum}"
    )

    println(s"SOL 2: ${symbols
        .filter(_.isGear)
        .map { s =>
          engineParts.filter(ep => ep.lineIndex >= s.lineIndex - 1 && ep.lineIndex <= s.lineIndex + 1 && ep.isSymbolAdjacent(s))
        }
        .filter(_.size == 2)
        .map(_.map(ep => ep.number.toInt).product)
        .sum}")

  }

  @tailrec
  private def parseLine(
      input: List[Char],
      acc: List[EngineSchematicObject],
      currIndex: Int,
      line: Int
  ): List[EngineSchematicObject] =
    input match {
      case Nil => acc
      case xs if xs.head.isDigit =>
        val number = xs.takeWhile(_.isDigit)
        val leftOver = xs.drop(number.size)
        parseLine(
          leftOver,
          acc :+ EnginePart(line, currIndex, number.mkString),
          currIndex + number.size,
          line
        )
      case x :: xs if x != '.' =>
        parseLine(xs, acc :+ Symbol(line, currIndex, x), currIndex + 1, line)
      case _ :: xs => parseLine(xs, acc, currIndex + 1, line)
    }

  @tailrec
  def validPart(enginePart: EnginePart, schematic: List[Symbol]): Boolean =
    schematic match {
      case Nil => false
      case x :: xs =>
        if (enginePart.isSymbolAdjacent(x)) true
        else validPart(enginePart, xs)

    }

}
