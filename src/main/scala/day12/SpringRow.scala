package day12

import scala.annotation.tailrec

final case class SpringRow(row: String, survey: List[Int]) {
  def evaluateWithMaps: Long = {
    // part 1 startPossibilities
    @tailrec
    def startPossibilities(
        remaining: String,
        currentIndex: Int,
        currentSurvey: Int,
        acc: List[BlockAttempt],
        prevDamaged: Boolean
    ): List[BlockAttempt] =
      if (remaining.size < currentSurvey) acc
      else {
        if (prevDamaged)
          startPossibilities(
            remaining.tail,
            currentIndex + 1,
            currentSurvey,
            acc,
            remaining.startsWith("#")
          )
        else {
          val reg = ("""[\?|\#]{""" + currentSurvey + """}[\?|\.]*$""").r
          // val cleared = remaining.takeWhile(_ == '.')

          val attempt = remaining.take(currentSurvey + 1)

          if (reg.matches(attempt)) {
            val nextBlock = remaining.drop(currentSurvey + 1).indexOf('#')
            startPossibilities(
              remaining.tail,
              currentIndex + 1,
              currentSurvey,
              acc :+ BlockAttempt(
                currentIndex,
                Option.when(nextBlock >= 0)(
                  nextBlock + currentIndex + currentSurvey + 1
                )
              ),
              attempt.startsWith("#")
            )
          } else
            startPossibilities(
              remaining.tail,
              currentIndex + 1,
              currentSurvey,
              acc,
              attempt.startsWith("#")
            )
        }
      }
    // construct map
    val surveyMap = survey.distinct
      .map(i => (i -> startPossibilities(row, 0, i, List.empty, false)))
      .toMap

    @tailrec
    def recCalcIm(
        currentIndex: Int,
        surveys: List[Int],
        sol: Long,
        partialMap: Map[(Int, List[Int]), Long],
        acc: List[BlockAttemptSave]
    ): Long = {
      if (surveys.length == 1) {
        val s = surveyMap(surveys.head)
          .filter(ba =>
            ba.index >= currentIndex && ba.nextUncheckedDmgIndex.isEmpty && !row
              .substring(currentIndex, ba.index)
              .contains("#")
          )
          .length
        if (acc.isEmpty) sol + s
        else
          recCalcIm(
            acc.head.startIndex,
            acc.head.survey,
            sol + s,
            partialMap + ((currentIndex, surveys) -> s),
            acc.tail
          )
      } else {
        val pot = surveyMap(surveys.head)
          .filter(ba => {
            ba.index >= currentIndex && !row
              .substring(currentIndex, ba.index)
              .contains("#")
          })
        if (pot.isEmpty) {
          if (acc.isEmpty) sol
          else {
            recCalcIm(
              acc.head.startIndex,
              acc.head.survey,
              sol,
              partialMap + ((currentIndex, surveys) -> 0),
              acc.tail
            )
          }
        } else {
          val mapped = pot.map(ba =>
            partialMap.get((ba.index + surveys.head + 1, surveys.tail))
          )
          if (mapped.forall(_.isDefined)) {
            val s = mapped.flatten.sum
            if (acc.isEmpty) sol + s
            else {
              recCalcIm(
                acc.head.startIndex,
                acc.head.survey,
                sol + s,
                partialMap + ((currentIndex, surveys) -> s),
                acc.tail
              )
            }
          } else {
            val (known, unknown) = pot.partition(ba =>
              partialMap.isDefinedAt(
                (ba.index + surveys.head + 1, surveys.tail)
              )
            )
            val s = known
              .map(ba =>
                partialMap((ba.index + surveys.head + 1, surveys.tail))
              )
              .sum
            val headPotential = unknown.head
            recCalcIm(
              headPotential.index + surveys.head + 1,
              surveys.tail,
              sol + s,
              partialMap,
              unknown.tail.map(ba =>
                BlockAttemptSave(ba.index + surveys.head + 1, surveys.tail)
              ) ++ acc
            )
          }
        }
      }
    }
    recCalcIm(0, survey, 0, Map.empty, List.empty)
  }
}

final case class BlockAttempt(index: Int, nextUncheckedDmgIndex: Option[Int])

final case class BlockAttemptSave(startIndex: Int, survey: List[Int])

object SpringRow {
  def apply(s: String): SpringRow = {
    val parts = s.split("\\s")
    SpringRow(
      parts(0),
      parts(1).split(",").map(_.toInt).toList
    )
  }

  def multiplyBy(springRow: SpringRow, i: Int): SpringRow = {
    val newSprings: String =
      s"${springRow.row}?".repeat(i).dropRight(1)
    val newSurvey: String =
      s"${springRow.survey.mkString(",")},".repeat(i).dropRight(1)
    apply(s"$newSprings $newSurvey")
  }
}
