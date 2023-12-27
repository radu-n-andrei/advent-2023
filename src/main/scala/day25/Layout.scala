package day25

final case class Layout(wires: Map[String, List[String]]) {

    def componentGroups: List[List[String]] = {
        def checkComps(toCheck: List[String], groups: List[List[String]], building: List[String]): List[List[String]] = 
            toCheck match {
                case Nil => 
                    val notChecked = wires.keySet.filter(k => !groups.flatten.contains(k) && !building.contains(k))
                    if(notChecked.isEmpty) groups :+ building
                    else {
                        checkComps(List(notChecked.head), groups :+ building, List.empty)
                    }
                case c :: cs => 
                    val next = wires(c).diff(building).diff(cs)
                    checkComps(cs ++ next, groups, building :+ c)
            }

        checkComps(List(wires.keySet.head), List.empty, List.empty)    
    }

}

object Layout {
  def apply(configs: List[WireConfiguration]): Layout = {
    def expandSchematic(
        config: List[WireConfiguration],
        expanded: Map[String, List[String]]
    ): Map[String, List[String]] = {
      config match {
        case Nil => expanded
        case s :: ss =>
          val pairs = s.toPairs
          val updatedMap = pairs.foldLeft(expanded) { case (m, p) =>
            val m1 =
              m + (p._1 -> m.get(p._1).fold(List(p._2))(ex => ex :+ p._2))
            m1 + (p._2 -> m1.get(p._2).fold(List(p._1))(ex => ex :+ p._1))
          }
          expandSchematic(ss, updatedMap)
      }

    }
    Layout(expandSchematic(configs, Map.empty))
  }

}
