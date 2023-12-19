package day19

sealed trait Condition {
    def evaluate(machinePart: MachinePart): Boolean
    val outcome: Result
}

case class LessThan(machineCharacteristic: PartCharacteristic, limit: Int, outcome: Result) extends Condition {
    override def evaluate(machinePart: MachinePart): Boolean = machinePart.characteristic(machineCharacteristic) < limit
}

case class GreaterThan(machineCharacteristic: PartCharacteristic, limit: Int, outcome: Result) extends Condition {
    override def evaluate(machinePart: MachinePart): Boolean = machinePart.characteristic(machineCharacteristic) > limit
}

case class Default(outcome: Result) extends Condition {
    override def evaluate(machinePart: MachinePart): Boolean = true
}

object Condition {
    def apply(s: String): Condition = {
        val condPat = """([x|m|a|s])([<|>])([0-9]+):(.*)""".r
        s match {
            case condPat(m, "<", l, o) => LessThan(PartCharacteristic(m.head), l.toInt, Result(o))
            case condPat(m, ">", l, o) => GreaterThan(PartCharacteristic(m.head), l.toInt, Result(o))
            case s => Default(Result(s))
        }
    }
}


