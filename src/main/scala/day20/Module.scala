package day20

sealed abstract class Module(val name: String, val outputs: List[String]) {
  def pulseReceived(pulse: PendingPulse): (Module, List[PendingPulse])
  def print: Unit
}

final case object Button extends Module("start", List("broadcaster")) {
  override def pulseReceived(
      pulse: PendingPulse
  ): (Module, List[PendingPulse]) =
    (this, outputs.map(m => PendingPulse(pulse.pulse, m, name)))

  override def print: Unit = println("StartButton")
}

final case class Broadcast(o: List[String]) extends Module("broadcaster", o) {
  override def pulseReceived(
      pulse: PendingPulse
  ): (Module, List[PendingPulse]) =
    (this, outputs.map(m => PendingPulse(pulse.pulse, m, name)))

  override def print: Unit = println("Broadcaster")
}

final case class FlipFlop(n: String, o: List[String], state: Boolean)
    extends Module(n, o) {
  override def pulseReceived(
      pulse: PendingPulse
  ): (Module, List[PendingPulse]) =
    pulse.pulse match {
      case HighPulse => (this, List.empty)
      case LowPulse =>
        val signal = if (state) LowPulse else HighPulse
        (
          FlipFlop(n, o, !state),
          outputs.map(m => PendingPulse(signal, m, name))
        )
    }

  override def print: Unit = {
    val status = if (state) "ON" else "OFF"
    println(s"FlipFlop $name; status $status")
  }
}

final case class Conjunction(n: String, o: List[String], in: Map[String, Pulse])
    extends Module(n, o) {
  override def pulseReceived(
      pulse: PendingPulse
  ): (Module, List[PendingPulse]) = {
    val updatedMap = in + (pulse.input -> pulse.pulse)
    val signal =
      if (updatedMap.values.forall(_ == HighPulse)) LowPulse else HighPulse
    (
      Conjunction(n, o, updatedMap),
      outputs.map(m => PendingPulse(signal, m, name))
    )
  }

  override def print: Unit = {
    val (high, low) = in.values.partition {
      case HighPulse => true
      case LowPulse  => false
    }
    println(s"Conjunction $name; HIGH=${high.size}; LOW=${low.size}")
  }

}

object Module {
  def apply(in: String): Module = {
    val pat = """(.*)\s->\s(.*)""".r
    in match {
      case pat("broadcaster", o) =>
        Broadcast(o.split(",").map(_.trim).filter(_.nonEmpty).toList)
      case pat(in, o) =>
        val outs = o.split(",").map(_.trim).filter(_.nonEmpty).toList
        in.head match {
          case '%' => FlipFlop(in.tail, outs, false)
          case '&' => Conjunction(in.tail, outs, Map.empty)
          case _ =>
            throw new RuntimeException(s"Illegal module type ${in.head}")
        }
      case _ => throw new RuntimeException(s"Illegal module description $in")
    }
  }
}
