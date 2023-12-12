package day12

import scala.annotation.tailrec

final case class SpringRow(row: List[Spring], survey: List[Int]) {
  override def toString: String =
    s"${row.map(_.symbol).mkString} ${survey.mkString(",")}"

  // FIXME too inefficient
//   def evaluate: Int = {

//     def checkRow(
//         r: List[Spring],
//         currentPossibility: List[Spring]
//     ): List[List[Spring]] = {
//       r match {
//         case Nil                     => List(currentPossibility)
//         case r :: rs if !r.isUnknown => checkRow(rs, currentPossibility :+ r)
//         case r :: rs =>
//           checkRow(rs, currentPossibility :+ FunctionalSpring) ++ checkRow(
//             rs,
//             currentPossibility :+ DamagedSpring
//           )
//       }
//     }

//     checkRow(row, List.empty).count(l => SpringRow.valid(SpringRow(l, survey)))
//   }
  // also inefficient
  def evaluate(index: Option[Int] = None): Int = {
    def checkRow(
        rows: List[Spring],
        currentSurvey: List[Int],
        prevDamaged: Boolean
    ): Int =
      rows match {
        case Nil if currentSurvey.forall(_ == 0) =>
          1 // reached the end of the list and survey is empty => Success
        case Nil =>
          0 // reached the end of the list and survey is not empty => Failure
        case r if r.length < currentSurvey.sum + currentSurvey.length - 1 =>
          0 // not enough elemets to continue survey
        case r
            if currentSurvey.isEmpty => { // survey clear, if only ? and . left => solution else failure
          val damaged = r.count {
            case DamagedSpring => true
            case _             => false
          }
          if (damaged == 0) 1 else 0
        }
        case r :: rs =>
          r match {
            case DamagedSpring =>
              currentSurvey.headOption.fold(0)(
                check => // a damaged spring is found but all have been accounted for
                  if (check == 0)
                    0 // a damaged spring is found but the current survey has been fulfilled
                  else
                    checkRow( // expand on the current damage survey
                      rs,
                      (check - 1) +: currentSurvey.tail,
                      true
                    )
              )
            case FunctionalSpring =>
              currentSurvey.headOption.fold(
                checkRow(rs, currentSurvey, false)
              )(check =>
                if (check == 0)
                  checkRow(
                    rs,
                    currentSurvey.tail,
                    false
                  )
                else if (prevDamaged) 0
                else checkRow(rs, currentSurvey, false)
              )
            case Unknown =>
              if (currentSurvey.isEmpty) // shortcut?
                checkRow(
                  rs,
                  currentSurvey,
                  false
                ) // survey done => Functional
              else {
                val current = currentSurvey.head
                if (current == 0)
                  checkRow(
                    rs,
                    currentSurvey.tail,
                    false
                  ) // survey head 0 => Functional
                else {
                  checkRow(
                    rs,
                    (current - 1) +: currentSurvey.tail,
                    true
                  ) + // try Damaged
                    (if (!prevDamaged)
                       checkRow(
                         rs,
                         currentSurvey,
                         false
                       ) // try Functional
                     else 0)
                }
              }
          }
      }

    val sol = checkRow(row, survey, false)
    index.foreach(i => println(s"Done $i"))
    sol
  }
// if all in a row are ? or # it relies on evaluate => inefficient
  def allPossibilities(mult: Int, rowIndex: Int): Long = {
    val multiplied = SpringRow.multiplyBy(this, mult)
    val multipliedSurvey = multiplied.survey
    // split the rows into mult slices so that they do not break sequences of ?#

    def prepareRows(
        r: List[Spring],
        acc: List[List[Spring]]
    ): List[List[Spring]] =
      r match {
        case Nil => acc
        case springRow =>
          val firstPart = r.take(row.length)
          val remaining = r.drop(row.length)
          val extras = remaining.takeWhile(spring =>
            spring match {
              case FunctionalSpring => false
              case _                => true
            }
          )
          val rems = remaining.drop(extras.length)
          val extrasPlus = rems.headOption.fold(extras)(x => extras :+ x)
          if (rems.isEmpty) acc :+ (firstPart ++ extrasPlus)
          else prepareRows(rems.tail, acc :+ (firstPart ++ extrasPlus))
      }

    val rows = prepareRows(multiplied.row, List.empty)

    def checkRows(atIndex: Int, remainingSurvey: List[Int]): Long = {
      if (rows.length == 1) SpringRow(rows.head, remainingSurvey).evaluate()
      else if (atIndex == rows.length) if (remainingSurvey.nonEmpty) 0L else 1L
      else {
        (0 to remainingSurvey.length).map { i =>
          // all indexes except for the last one get an extra "?"
          val solutionsForI = SpringRow(
            rows(atIndex),
            remainingSurvey.take(i)
          ).evaluate() // how many solutions with i survey length
          if (solutionsForI != 0)
            solutionsForI * checkRows(
              atIndex + 1,
              remainingSurvey.drop(i)
            ) // how many can i create further from here
          else 0L
        }.sum
      }
    }
    val sol = checkRows(0, multipliedSurvey)
    println(s"DONE with $rowIndex")
    sol
  }

  def fastishEval(i: Option[Int] = None): Long = {
    def eval(
        springs: List[Spring],
        surveys: List[Int],
        acc: Long
    ): Long = {
      if (surveys.isEmpty)
        if (
          springs.exists {
            case DamagedSpring => true
            case _             => false
          }
        ) 0L
        else 1L
      else {
        attemptGroup(springs, surveys) match {
          case Valid(maybeGrouping, remaining, next) =>
            if (maybeGrouping.startsWith("#")) // no more shifting possible
              acc + eval(remaining, surveys.tail, 0)
            else {
              val rest = eval(remaining, surveys.tail, 0)
              eval(
                next,
                surveys,
                acc + rest
              ) // valid grouping add to the solution then shift
            }
          case Invalid(InvalidGrouping(remaining)) =>
            eval(
              remaining,
              surveys,
              acc
            ) // invalid grouping, skip ahead to the next potential grouping
          case Invalid(_) =>
            acc // premature ending - either the rest are not enough or we reached an un-shiftable place
        }
      }
    }
    val s = eval(row, survey, 0)
    i.foreach(x => println(s"Done with $x"))
    s
  }

  def pootentialScenarios: Long = {

    @tailrec
    def eval(
        springs: List[Spring],
        surveys: List[Int],
        potential: List[List[Spring]]
    ): List[List[Spring]] = {
      attemptGroup(springs, surveys) match {
        case Valid(maybeGrouping, remaining, next) =>
          if (maybeGrouping.startsWith("#")) // no more shifting possible
            potential :+ remaining // done
          else
            eval(next, surveys, potential :+ remaining)
        case Invalid(InvalidGrouping(remaining)) =>
          eval(remaining, surveys, potential)
        // invalid grouping, skip ahead to the next potential grouping
        case Invalid(_) =>
          // println("DONE")
          potential // premature ending - either the rest are not enough or we reached an un-shiftable place
      }
    }

    def recAtts(surs: List[Int], springs: List[Spring]): Long = {
      val potentials = eval(springs, surs, List.empty)
      if (surs.length == 1) {
        potentials.count(l =>
          !l.exists {
            case DamagedSpring => true
            case _             => false
          }
        )
      } else {
        potentials.map(s => recAtts(surs.tail, s)).sum
      }
    }
    recAtts(survey, row)
    // val p = eval(row, survey, List.empty)
    // p.foreach { l =>
    //   println(l.map(_.symbol).mkString)
    //   println("===============")
    //   eval(l, survey.tail, List.empty).foreach(l =>
    //     println(l.map(_.symbol).mkString)
    //   )
    //   scala.io.StdIn.readLine()
    //   ()
    // }

  }

  private def attemptGroup(
      springs: List[Spring],
      surveys: List[Int]
  ): Grouping = {
    val clearFunctional = springs.dropWhile {
      case FunctionalSpring => true
      case _                => false
    }
    val currentSurvey = surveys.head
    val grouping = clearFunctional.take(currentSurvey + 1)
    val maybeGrouping = grouping.map(_.symbol).mkString
    val remaining = clearFunctional.drop(currentSurvey + 1)
    val validReg = ("""[\?|\#]{""" + currentSurvey + """}[\.|\?]*$""").r
    val fatalReg = """.*\#+.*\.+.*""".r

    if (validReg.matches(maybeGrouping)) {
      Valid(maybeGrouping, remaining, clearFunctional.tail)
    } else {
      val skip = grouping.dropWhile {
        case FunctionalSpring => false
        case _                => true
      }
      val futureRemaining =
        if (skip.nonEmpty) skip.tail ++ remaining else remaining
      val minToFinish = surveys.sum + surveys.length - 1
      // can't finish the survey with what we have left
      if (
        maybeGrouping.size < currentSurvey || futureRemaining.length < minToFinish || !futureRemaining
          .exists {
            case DamagedSpring | Unknown => true
            case FunctionalSpring        => false
          }
      ) {
        Invalid(InsufficientElements)
      }
      // would shifting still be possible?
      else if (
        fatalReg.matches(maybeGrouping) || maybeGrouping.startsWith("#")
      ) {
        Invalid(InvalidGroupingFatal)
      } else {
        // not fatal, skip ahead
        Invalid(InvalidGrouping(clearFunctional.tail))
      }
    }
  }
}

object SpringRow {
  def apply(s: String): SpringRow = {
    val parts = s.split("\\s")
    SpringRow(
      parts(0).map(Spring(_)).toList,
      parts(1).split(",").map(_.toInt).toList
    )
  }

  def multiplyBy(springRow: SpringRow, i: Int): SpringRow = {
    val newSprings: String =
      s"${springRow.row.map(_.symbol).mkString}?".repeat(i).dropRight(1)
    val newSurvey: String =
      s"${springRow.survey.mkString(",")},".repeat(i).dropRight(1)
    apply(s"$newSprings $newSurvey")
  }

  def valid(springRow: SpringRow): Boolean = {
    def checkRow(row: List[Spring], acc: List[Int], prevDamaged: Int): Boolean =
      row match {
        case Nil =>
          val updatedAcc = if (prevDamaged == 0) acc else acc :+ prevDamaged
          updatedAcc.length == springRow.survey.length && updatedAcc
            .zip(springRow.survey)
            .forall(p => p._1 == p._2)
        case r :: rs =>
          r match {
            case DamagedSpring => checkRow(rs, acc, prevDamaged + 1)
            case FunctionalSpring if prevDamaged == 0 =>
              checkRow(rs, acc, prevDamaged)
            case FunctionalSpring => checkRow(rs, acc :+ prevDamaged, 0)
            case _                => false
          }
      }
    checkRow(springRow.row, List.empty, 0)
  }
}
