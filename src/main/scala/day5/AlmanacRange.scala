package day5

import scala.annotation.tailrec

/**
  * Closed interval
  *
  * @param rangeStart
  * @param rangeEnd
  */
final case class AlmanacRange(rangeStart: BigInt, rangeEnd: BigInt)

object AlmanacRange {

  def apply(
      al: Almanac,
      start: BigInt,
      stop: BigInt
  ): List[AlmanacRange] = {
    val soilRanges = evolveRange(al.seed2Soil, AlmanacRange(start, stop))
    val fertilizerRanges = soilRanges.flatMap(ra => evolveRange(al.soil2Fertilizer, ra))
    val waterRanges = fertilizerRanges.flatMap(ra => evolveRange(al.fertilizer2Water, ra))
    val lightRanges = waterRanges.flatMap(ra => evolveRange(al.water2Light, ra))
    val temperatureRanges = lightRanges.flatMap(ra => evolveRange(al.light2Temperature, ra))
    val humidityRanges = temperatureRanges.flatMap(ra => evolveRange(al.temperature2Humidity, ra))
    val locationRanges = humidityRanges.flatMap(ra => evolveRange(al.humidity2Location, ra))
    locationRanges
  }

  private def evolveRange(mappings: List[AlmanacMap], internval: AlmanacRange): List[AlmanacRange] = {
    val applicableMappings = mappings.filter { am =>
      val mappingRange = (am.sourceStart, am.sourceStart + am.range)
      val outOfRange = mappingRange._2 < internval.rangeStart || internval.rangeEnd < mappingRange._1
      !outOfRange
    }
    rangesFromApplicableMaps(applicableMappings, internval.rangeStart, internval.rangeEnd)
  }

  private def rangesFromApplicableMaps(ams: List[AlmanacMap], start: BigInt, stop: BigInt): List[AlmanacRange] = {
    val sorted = ams.sortBy(_.sourceStart)
    // if there are no applicable maps, pass through completely
    if (sorted.isEmpty) {
      List(AlmanacRange(start, stop))
    } else {
      val r = applicableRanges(sorted, start, stop, List.empty)
      // last mapping -> stop
      val after =
        if (sorted.last.sourceStart + sorted.last.range <= stop)
          Some(
            AlmanacRange(
              sorted.last.sourceStart + sorted.last.range,
              stop
            )
          )
        else None
      after.fold(r)(a => r :+ a)
    }
  }

  @tailrec
  private def applicableRanges(
      sortedAlmanacMaps: List[AlmanacMap],
      start: BigInt,
      stop: BigInt,
      result: List[AlmanacRange]
  ): List[AlmanacRange] =
    sortedAlmanacMaps match {
      // end of mappings
      case Nil => result
      // gap in mapping `start` is not `sourceStart` - pass through
      case l if l.head.sourceStart > start =>
        applicableRanges(
          l,
          l.head.sourceStart,
          stop,
          result :+ AlmanacRange(start, l.head.sourceStart - 1)
        )
      // no gap - apply the mapping to get the next leveel
      case l :: ls =>
        // how many displaced from beginning
        val firstValueDiff = start - l.sourceStart
        // last source value for this mapping
        val lastValue = l.sourceStart + l.range - 1
        // last processable source value
        val lastProcessedValue = if (stop < lastValue) stop else lastValue
        // source range for this mapping
        val diff = lastProcessedValue - start
        // constructed range
        applicableRanges(
          ls,
          lastProcessedValue + 1,
          stop,
          result :+ AlmanacRange(
            l.destinationStart + firstValueDiff,
            l.destinationStart + firstValueDiff + diff
          )
        )
    }

}
