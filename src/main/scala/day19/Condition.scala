package day19

sealed trait Condition {
  def evaluate(machinePart: MachinePart): Boolean
  val outcome: Result
  def evaluateRange(
      partPossibilities: PartPossibilities
  ): (Option[PartPossibilities], List[PartPossibilities])
  def processRange(partPossibilities: PartPossibilities): ConditionOutcome = {
    val (valid, invalid) = evaluateRange(partPossibilities)
    outcome match { // solution, leftToProcess, toJump
      case Accepted =>
        ConditionOutcome(
          valid,
          invalid,
          List.empty
        ) // if the outcome is A, valid is a solution, invalid is to check next and nothing to jump to
      case Rejected =>
        ConditionOutcome(
          None,
          invalid,
          List.empty
        ) // if the outcome is R, no solution, invalid to check next and nothing to jump to
      case Reevaluate(flow) =>
        ConditionOutcome(
          None,
          invalid,
          valid.map(pp => RangedRule(flow, pp)).toList
        ) // if the outcome is Jump, no solution, invalid to check next, valid to jump to
    }
  }
}

case class LessThan(
    machineCharacteristic: PartCharacteristic,
    limit: Int,
    outcome: Result
) extends Condition {
  override def evaluate(machinePart: MachinePart): Boolean =
    machinePart.characteristic(machineCharacteristic) < limit
  override def evaluateRange(
      partPossibilities: PartPossibilities
  ): (Option[PartPossibilities], List[PartPossibilities]) = {
    val range = partPossibilities.parts(
      machineCharacteristic
    ) // the range i am working with
    val validRange = Range(1, limit - 1) // the range where this applies
    val intersection =
      range.intersection(validRange) // from the range these would pass the test
    val diff = range.diff(validRange)
    (
      intersection.map(i =>
        partPossibilities.updateWith(machineCharacteristic, i)
      ),
      diff.map(r => partPossibilities.updateWith(machineCharacteristic, r))
    )
  }

}

case class GreaterThan(
    machineCharacteristic: PartCharacteristic,
    limit: Int,
    outcome: Result
) extends Condition {
  override def evaluate(machinePart: MachinePart): Boolean =
    machinePart.characteristic(machineCharacteristic) > limit
  override def evaluateRange(
      partPossibilities: PartPossibilities
  ): (Option[PartPossibilities], List[PartPossibilities]) = {
    val range = partPossibilities.parts(
      machineCharacteristic
    ) // the range i am working with
    val validRange = Range(limit + 1, 4000) // the range where this applies
    val intersection =
      range.intersection(validRange) // from the range these would pass the test
    val diff = range.diff(validRange)
    (
      intersection.map(i =>
        partPossibilities.updateWith(machineCharacteristic, i)
      ),
      diff.map(r => partPossibilities.updateWith(machineCharacteristic, r))
    )
  }
}

case class Default(outcome: Result) extends Condition {
  override def evaluate(machinePart: MachinePart): Boolean = true
  override def evaluateRange(
      partPossibilities: PartPossibilities
  ): (Option[PartPossibilities], List[PartPossibilities]) =
    (Some(partPossibilities), List.empty)
}

object Condition {
  def apply(s: String): Condition = {
    val condPat = """([x|m|a|s])([<|>])([0-9]+):(.*)""".r
    s match {
      case condPat(m, "<", l, o) =>
        LessThan(PartCharacteristic(m.head), l.toInt, Result(o))
      case condPat(m, ">", l, o) =>
        GreaterThan(PartCharacteristic(m.head), l.toInt, Result(o))
      case s => Default(Result(s))
    }
  }
}

case class ConditionOutcome(
    solution: Option[PartPossibilities],
    checkFurther: List[PartPossibilities],
    jump: List[RangedRule]
)
case class RangedCondition(
    condition: Condition,
    partPossibilities: PartPossibilities
) {
  def evaluate: ConditionOutcome = condition.processRange(partPossibilities)
}
