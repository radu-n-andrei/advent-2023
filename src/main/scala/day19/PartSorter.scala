package day19

object PartSorter {

  val startingRuleName = "in"

  def sortParts(parts: List[MachinePart], rules: List[Rule]): Unit = {
    val ruleMap = rules.groupBy(_.name).view.mapValues(_.head).toMap

    def sortSinglePart(
        part: MachinePart,
        evaluating: Rule
    ): FinalResult = {
      evaluating.conditions.collectFirst {
        case c if c.evaluate(part) => c.outcome
      } match {
        case Some(Accepted) => Accepted
        case Some(Rejected) => Rejected
        case None => throw new RuntimeException(s"No outcome for $evaluating")
        case Some(Reevaluate(s)) =>
          sortSinglePart(part, ruleMap(s))
      }
    }

    val (accepted, rejected) = parts.partition(mp =>
      sortSinglePart(mp, ruleMap(startingRuleName)) match {
        case Accepted => true
        case Rejected => false
      }
    )
    println(s"SOL1: ${accepted.map(_.score).sum}")
  }

  def possibilities(rules: List[Rule]): Unit = {
    val ruleMap = rules.groupBy(_.name).view.mapValues(_.head).toMap
    val start = RangedRule(startingRuleName, PartPossibilities.initial)
    // ruleMap(startingRuleName).evaluateRange(PartPossibilities.initial).print

    def process(
        toCheck: List[RangedRule],
        solutions: List[PartPossibilities]
    ): List[PartPossibilities] = {
      toCheck match {
        case Nil => solutions
        case check :: checks =>
          val processed =
            ruleMap(check.rule).evaluateRange(check.partPossibilities)
          process(checks ++ processed.toJump, solutions ++ processed.solutions)
      }
    }

    println(
      s"SOL2: ${process(List(start), List.empty).map(_.total).reduce((_ + _))}"
    )
  }

}
