package day20

sealed trait Pulse
case object LowPulse extends Pulse
case object HighPulse extends Pulse

final case class PendingPulse(pulse: Pulse, output: String, input: String)
