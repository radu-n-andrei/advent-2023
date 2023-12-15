package day15

import scala.io.Source
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object LensLibrary {

  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day15.input")

    val t = file.iter.foldLeft((List.empty[LensInput], "")) {
      case ((acc, s), c) =>
        if (c == ',') (acc :+ LensInput(s), "")
        else (acc, s + c)
    }

    val allLenses = t._1 :+ LensInput(t._2)

    println(s"SOL1: ${allLenses.map(_.hash).sum}")

    val boxMap = (0 to 255).map(i => (i -> Box.empty)).toMap

    println(s"SOL2: ${processInput(allLenses, boxMap).map { case (index, box) =>
        box.focusingPower(index)
      }.sum}")

    file.close()
  }

  def processInput(
      input: List[LensInput],
      boxMap: Map[Int, Box]
  ): Map[Int, Box] = {
    input match {
      case Nil => boxMap
      case i :: ins =>
        val box = boxMap(i.box)
        val newBox = i.operation match {
          case Add(focalStrength) => box.add(Lens(i.label, focalStrength))
          case Remove             => box.remove(i.label)
        }
        processInput(ins, boxMap + (i.box -> newBox))
    }
  }
}
