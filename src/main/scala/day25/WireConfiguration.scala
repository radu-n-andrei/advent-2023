package day25

final case class WireConfiguration(main: String, connections: List[String]) {
    def toPairs: List[(String, String)] = 
        connections.map(c => main -> c)
}

object WireConfiguration {
  def apply(s: String): WireConfiguration = {
    val pat = """([a-z]+):\s([a-z|\s]+)""".r
    s match {
      case pat(main, rest) =>
        val others = rest.split("\\s").filter(_.nonEmpty).toList
        WireConfiguration(main, others)
      case _ => throw new RuntimeException(s"Illegal wire configuration: $s")
    }
  }
}
