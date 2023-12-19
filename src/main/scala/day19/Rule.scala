package day19

final case class Rule(name: String, conditions: List[Condition]) {

  def evaluateRange(partPossibilities: PartPossibilities): RuleOutcome = {

    def goThrough(
        buffer: List[RangedCondition],
        toJump: List[RangedRule],
        solutions: List[PartPossibilities],
        conditionList: List[Condition]
    ): RuleOutcome = {
      buffer match {
        case Nil => RuleOutcome(solutions, toJump)
        case cond :: conds =>
          val condOutcome = cond.evaluate
          val further = conditionList.headOption
            .map(h => condOutcome.checkFurther.map(p => RangedCondition(h, p)))
            .getOrElse(List.empty)
          goThrough(
            conds ++ further,
            toJump ++ condOutcome.jump,
            condOutcome.solution.fold(solutions)(s => solutions :+ s),
            if (conditionList.isEmpty) List.empty else conditionList.tail
          )
      }
    }

    goThrough(
      List(RangedCondition(conditions.head, partPossibilities)),
      List.empty,
      List.empty,
      conditions.tail
    )
  }
}

object Rule {
  def apply(str: String): Rule = {
    val rulePattern = """([a-z]+)\{(.*)\}""".r
    str match {
      case rulePattern(n, conds) =>
        Rule(n, conds.split(",").map(Condition(_)).toList)
      case _ => throw new RuntimeException(s"Illegal rule: $str")
    }
  }
}

case class RangedRule(rule: String, partPossibilities: PartPossibilities)
case class RuleOutcome(
    solutions: List[PartPossibilities],
    toJump: List[RangedRule]
) {

  def print: Unit = {
    println(s"SOLUTIONS:")
    solutions.foreach(pp => println(s"\t $pp"))
    println(s"JUMPING:")
    toJump.foreach(rr =>
      println(s"\t Jumping to ${rr.rule} to evaluate ${rr.partPossibilities}")
    )
  }
}
