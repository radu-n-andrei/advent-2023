package day20

import scala.annotation.tailrec

object ModuleExecution {

  def execute(initial: ModuleConfiguration, times: Int): Unit = {
    @tailrec
    def keepPushingStart(
        times: Int,
        configuration: ModuleConfiguration,
        pulseReading: PulseReading,
        executionHistory: Map[ModuleConfiguration, PulseSurvey]
    ): PulseReading = {
      if (times == 0) pulseReading
      else {
        val survey =
          executionHistory.getOrElse(configuration, configuration.propagate())
        keepPushingStart(
          times - 1,
          survey.configuration,
          pulseReading + survey.reading,
          executionHistory + (configuration -> survey)
        )
      }
    }

    val reading =
      keepPushingStart(times, initial, PulseReading(0, 0), Map.empty)

    println(s"SOL1: ${reading.highs * reading.lows}")
  }

  /** The network happens to be made out of 4 different subnets all converging
    * into a single conjunction ll, rx's single parent Each subnets repeats a
    * HighPulse on its ll in the same period, so the smallest common multiple is
    * the first time they would all land on High, producing a Low for rx...
    *
    * This is such a custom solution i won't event attempt to consider
    * implementing a generic scenario... One such scenario would be: split the
    * network into subnets -> keep subnets that connect to the parent(s) of rx
    * -*-> run those subnets until you reach a favourable value (depening if the
    * parents are % or &) -> if the iterations match => solution else bring all
    * subnets to the max iteration and check if that is a solution if it is =>
    * done else repeat from (*)
    *
    * @param initial
    * @param moduleName
    */
  def minToModule(initial: ModuleConfiguration, moduleName: String): Unit = {
    val parent = initial.moduleMap
      .filter { case (k, v) =>
        v.outputs.contains(moduleName)
      }
      .head
      ._1

    def gcd(a: Long, b: Long): Long = {
      val (first, second) = if (a < b) (a, b) else (b, a)
      if (second % first == 0) first
      else gcd(first, second % first)
    }

    @tailrec
    def keepPushingStart(
        times: Long,
        configuration: ModuleConfiguration
    ): (Long, ModuleConfiguration) = {
      val survey =
        configuration.propagate(Some(parent))
      if (survey.reading.highs == 1) (times, survey.configuration)
      else
        keepPushingStart(
          times + 1,
          survey.configuration
        )

    }

    def subNet(connection: List[String], acc: List[String]): List[String] = {
      connection match {
        case Nil => acc
        case c :: cs =>
          val outputs = initial
            .moduleMap(c)
            .outputs
            .filterNot(acc.contains)
            .filter(initial.moduleMap.isDefinedAt)
          subNet(cs ++ outputs, acc ++ outputs)
      }
    }

    val subnets = initial
      .moduleMap("broadcaster")
      .outputs
      .map(o => subNet(List(o), List("start", "broadcaster")))
    val subs = subnets.map(initial.subNet)
    println(s"SOL2: ${subs.map(mc => keepPushingStart(1, mc)._1).reduce {
        (a, b) => a * b / gcd(a, b)
      }}")
  }

}
