package day12

import scala.annotation.tailrec

final case class SpringRow(row: String, survey: List[Int]) {
  def evaluateWithMaps: Long = {
    /**
      * Find all possible valid start locations for a survey length S
      * To be valid, it must not be of size S, preceded by a # and followed by ? or . or $.
      * Each valid position is described by its start index and the potential location of the first
      * known # not included in its grouping.
      */
    @tailrec
    def startPossibilities(
        remaining: String,
        currentIndex: Int,
        currentSurvey: Int,
        acc: List[BlockAttempt],
        prevDamaged: Boolean
    ): List[BlockAttempt] =
      // not enough springs to make an attempt
      if (remaining.size < currentSurvey) acc
      else {
        // the previous spring was damaged so a valid group can't start from here
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
          // the attempt is formed by taking S springs + 1 (for the separator)
          val attempt = remaining.take(currentSurvey + 1)

          if (reg.matches(attempt)) {
            val nextBlock = remaining.drop(currentSurvey + 1).indexOf('#')
            // a match is found, update the result and carry on
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
            // no match, carry on without updating the result
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

    /**
      * Count the number of valid arrangements
      * currentIndex is where the previous blocked (including its trailing .) stopped
      *   That's why recusion calls will have the currentIndex set to index + S + 1
      */
    @tailrec
    def recCalcIm(
        currentIndex: Int,
        surveys: List[Int],
        sol: Long,
        partialMap: Map[(Int, List[Int]), Long],
        acc: List[BlockAttemptSave]
    ): Long = {
      // the last survey S to fulfil 
      if (surveys.length == 1) {
        // count the number of S length possibilities that are:
          // 1. after the current index
          // 2. will not be followed by a #
          // 3. will not skip over a #
        val s = surveyMap(surveys.head)
          .filter(ba =>
            ba.index >= currentIndex && ba.nextUncheckedDmgIndex.isEmpty && !row
              .substring(currentIndex, ba.index)
              .contains("#")
          )
          .length
          // no more attempts to consider - return value
        if (acc.isEmpty) sol + s
        else
          // update the solution, update the partial solution map and evaluate the next attempt
          recCalcIm(
            acc.head.startIndex,
            acc.head.survey,
            sol + s,
            partialMap + ((currentIndex, surveys) -> s),
            acc.tail
          )
      } else {
        // Find all the potential S size blocks that are:
          // 1. after the current index
          // 2. will not skip over a #
        val pot = surveyMap(surveys.head)
          .filter(ba => {
            ba.index >= currentIndex && !row
              .substring(currentIndex, ba.index)
              .contains("#")
          })
          // there is nowhere to go from here
        if (pot.isEmpty) {
          // no more attempts to consider - return the solution
          if (acc.isEmpty) sol
          else {
            // update the partial solution map and evaluate the next attempt
            recCalcIm(
              acc.head.startIndex,
              acc.head.survey,
              sol,
              partialMap + ((currentIndex, surveys) -> 0),
              acc.tail
            )
          }
        } else {
          // lookup the potential next steps in the partial solution map
          val mapped = pot.map(ba =>
            partialMap.get((ba.index + surveys.head + 1, surveys.tail))
          )
          // if they are all defined
          if (mapped.forall(_.isDefined)) {
            // calculate the sum of their solutions
            val s = mapped.flatten.sum
            // there are no more attempts to consider - return the solution
            if (acc.isEmpty) sol + s
            else {
              // update the partial solution map and evaluate the next attempt
              recCalcIm(
                acc.head.startIndex,
                acc.head.survey,
                sol + s,
                partialMap + ((currentIndex, surveys) -> s),
                acc.tail
              )
            }
          } else {
            // split the potential next steps into known (have already been mapped in the partial solution map)
            // and unknown
            val (known, unknown) = pot.partition(ba =>
              partialMap.isDefinedAt(
                (ba.index + surveys.head + 1, surveys.tail)
              )
            )
            // calculate the sum of the known steps
            val s = known
              .map(ba =>
                partialMap((ba.index + surveys.head + 1, surveys.tail))
              )
              .sum
            val headPotential = unknown.head
            // update the solution and start evaluating the first unknown potential next step
            // while prepending the attempt accumulator with the other unknown next steps
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
