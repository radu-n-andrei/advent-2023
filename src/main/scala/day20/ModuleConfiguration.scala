package day20

import scala.annotation.tailrec

final case class ModuleConfiguration(moduleMap: Map[String, Module]) {

  def print: Unit =
    moduleMap.values.foreach(_.print)

  def propagate(targetModule: Option[String] = None): PulseSurvey = {
    val button = moduleMap(ModuleConfiguration.startButtonName)
    val initialSignal = PendingPulse(LowPulse, button.name, "")
    @tailrec
    def pulseSequence(
        pulses: List[PendingPulse],
        configuration: Map[String, Module],
        highs: Long,
        lows: Long
    ): PulseSurvey = {
      pulses match {
        case Nil =>
          PulseSurvey(
            ModuleConfiguration(configuration),
            PulseReading(highs, lows)
          )
        case p :: ps =>
          val mod = configuration(p.output) // current module
          val (transformedModule, pending) = mod.pulseReceived(p)
          val (h, l) = pending
            .filter(pend => targetModule.forall(_ == pend.output))
            .map(_.pulse)
            .partition {
              case HighPulse => true
              case LowPulse  => false
            }
          val filteredPending =
            pending.filter(p => moduleMap.isDefinedAt(p.output))
          pulseSequence(
            ps ++ filteredPending,
            configuration + (mod.name -> transformedModule),
            highs + h.length,
            lows + l.length
          )
      }
    }

    pulseSequence(List(initialSignal), moduleMap, 0, 0)
  }

  def subNet(l: List[String]): ModuleConfiguration = {
     val m = l.foldLeft(Map.empty[String, Module]){
      case (acc, node) => acc + (node -> moduleMap(node))
     }
     ModuleConfiguration(m)
  }

}

object ModuleConfiguration {

  val startButtonName = "start"

  def apply(ins: List[String]): ModuleConfiguration = {
    val startButton = Button
    val modules = ins.map(Module(_))
    val initialMap =
      modules
        .groupBy(_.name)
        .view
        .mapValues(_.head)
        .toMap + (startButtonName -> Button)
    val conjunctions = modules.collect { case c: Conjunction =>
      c
    }
    val updatedConjunctions = conjunctions.map { c =>
      val inputs = modules.filter(m => m.outputs.contains(c.name)).map(_.name)
      c.copy(in = inputs.map(i => (i -> LowPulse)).toMap)
    }
    val updatedMap = updatedConjunctions.foldLeft(initialMap) { case (m, con) =>
      m + (con.name -> con)
    }
    ModuleConfiguration(updatedMap)
  }
}

final case class PulseReading(highs: Long, lows: Long) {
  def +(other: PulseReading) =
    PulseReading(highs + other.highs, lows + other.lows)
}
final case class PulseSurvey(
    configuration: ModuleConfiguration,
    reading: PulseReading
)
