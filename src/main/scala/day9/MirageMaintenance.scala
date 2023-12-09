package day9

import scala.io.Source
import scala.annotation.tailrec

object MirageMaintenance {
  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day9.input")
    val input = file.getLines.toList
    val readings = input.map(in =>
      in.split("\\s").map(_.trim).filter(_.nonEmpty).map(_.toInt).toList
    )
    println(
      s"TAIL SOL1: ${readings.map(r => tailPrediction(r, List.empty, _.last).foldRight(0)((x, acc) => acc + x)).sum}"
    )
    println(
      s"TAIL SOL2: ${readings.map(r => tailPrediction(r, List.empty, _.head).foldRight(0)((x, acc) => x - acc)).sum}"
    )
    file.close()
  }

  @tailrec
  private def tailPrediction(
      input: List[Int],
      acc: List[Int],
      collect: List[Int] => Int
  ): List[Int] =
    if (input.forall(_ == 0)) acc
    else
      tailPrediction(
        input.sliding(2, 1).toList.map(_.reverse.reduce(_ - _)),
        acc :+ collect(input),
        collect
      )

}
