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

}
