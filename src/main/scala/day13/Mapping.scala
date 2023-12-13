package day13

import scala.annotation.tailrec

final case class Mapping(lines: List[String]) {
  def print: Unit =
    lines.foreach(println)

  private def rotate: List[String] = {
    def rotateToTheRight(l: List[String], rot: List[String]): List[String] =
      l match {
        case Nil => rot
        case i :: is =>
          rotateToTheRight(
            is,
            rot.zip(i).map { case (s, c) =>
              c +: s
            }
          )
      }
    rotateToTheRight(lines, List.fill(lines.head.length)(""))
  }

  def evaluate: Int = {
    @tailrec
    def checkLines(mirrorIndex: Int, diff: Int, layout: List[String]): Boolean =
      (mirrorIndex - diff < 0 || mirrorIndex + diff + 1 >= layout.length) ||
        (layout(mirrorIndex - diff) == layout(
          mirrorIndex + diff + 1
        ) && checkLines(mirrorIndex, diff + 1, layout))

    @tailrec
    def recCalc(index: Int, layout: List[String]): Int = {
      if (index == layout.length - 1) 0
      else {
        if (layout(index) == layout(index + 1) && checkLines(index, 1, layout))
          index + 1
        else
          recCalc(index + 1, layout)
      }
    }
    recCalc(0, lines) * 100 + recCalc(0, rotate)
  }

  /** In theory the steps should be:
    *   1. if you find the smudge in the horizontal part apply it to the rotated
    *      lines and calculate 2. if you don't find the smudge in the horizontal
    *      part, find it in the vertical part
    * then run the horizontal part again witth the smudge applied
    *
    * But, yeah...i didn't have to do it and i've earned a break after
    * yesterday's part 2
    * @return
    */
  def evaluateSmudge: Int = {
    @tailrec
    def checkLines(
        mirrorIndex: Int,
        diff: Int,
        layout: List[String],
        smudgeFound: Boolean
    ): Boolean =
      if (mirrorIndex - diff < 0 || mirrorIndex + diff + 1 >= layout.length)
        smudgeFound
      else {
        val d = lineDiff(
          layout(mirrorIndex - diff),
          layout(mirrorIndex + diff + 1),
          0
        )
        if (d == 1)
          !smudgeFound && checkLines(mirrorIndex, diff + 1, layout, true)
        else if (d > 1) false
        else checkLines(mirrorIndex, diff + 1, layout, smudgeFound)
      }

    def lineDiff(l1: String, l2: String, i: Int): Int =
      (l1, l2) match {
        case (s1, _) if s1.isEmpty          => i
        case (s1, s2) if s1.head == s2.head => lineDiff(s1.tail, s2.tail, i)
        case (s1, s2)                       => lineDiff(s1.tail, s2.tail, i + 1)
      }

    @tailrec
    def recCalc(index: Int, layout: List[String]): Int = {
      if (index == layout.length - 1) 0
      else {
        val diff = lineDiff(layout(index), layout(index + 1), 0)
        if (
          diff == 0 && checkLines(
            index,
            1,
            layout,
            false
          ) || diff == 1 && checkLines(index, 1, layout, true)
        )
          index + 1
        else
          recCalc(index + 1, layout)
      }
    }

    recCalc(0, lines) * 100 + recCalc(0, rotate)
  }
}
