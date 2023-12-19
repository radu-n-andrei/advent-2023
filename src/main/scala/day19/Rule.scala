package day19

final case class Rule(name: String, conditions: List[Condition])

object Rule {
    def apply(str: String): Rule = {
        val rulePattern = """([a-z]+)\{(.*)\}""".r
        str match {
            case rulePattern(n, conds) => Rule(n, conds.split(",").map(Condition(_)).toList)
            case _ => throw new RuntimeException(s"Illegal rule: $str")
        }
    }
}
