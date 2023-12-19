package day19

import scala.io.Source

object Aplenty {
  
    def main(args: Array[String]): Unit = {
        val file = Source.fromResource("day19.input")
        val input = file.getLines().toList
        val (rules, parts) = readInput(input, List.empty, List.empty, false)
        PartSorter.sortParts(parts, rules)
        file.close()
    }

    private def readInput(lines: List[String], rules: List[Rule], parts: List[MachinePart], foundNL: Boolean): (List[Rule], List[MachinePart]) = {
        lines match {
            case Nil => (rules, parts)
            case l :: ls if l.isBlank() => readInput(ls, rules, parts, true)
            case l :: ls if !foundNL => readInput(ls, rules :+ Rule(l), parts, foundNL)
            case l :: ls => readInput(ls, rules, parts :+ MachinePart(l), foundNL)
        }
    }
}
