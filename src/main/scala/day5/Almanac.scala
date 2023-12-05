package day5

final case class Almanac(
    seeds: List[BigInt],
    seed2Soil: List[AlmanacMap],
    soil2Fertilizer: List[AlmanacMap],
    fertilizer2Water: List[AlmanacMap],
    water2Light: List[AlmanacMap],
    light2Temperature: List[AlmanacMap],
    temperature2Humidity: List[AlmanacMap],
    humidity2Location: List[AlmanacMap]
) {

  private def translate(
      source: BigInt,
      destinationMap: List[AlmanacMap]
  ): BigInt =
    destinationMap
      .flatMap(am => am.translate(source))
      .headOption
      .getOrElse(source)

  private def singleSeedLocation(s: BigInt): BigInt = {
    val soil = translate(s, seed2Soil)
    val fertilizer = translate(soil, soil2Fertilizer)
    val water = translate(fertilizer, fertilizer2Water)
    val light = translate(water, water2Light)
    val temp = translate(light, light2Temperature)
    val humidity = translate(temp, temperature2Humidity)
    translate(humidity, humidity2Location)
  }

  def updateMap(line: String, headerName: AlmanacHeader): Almanac = {
    val parsedLine = AlmanacMap(line.trim.split("\\s").map(BigInt(_)).toList)
    parsedLine.fold(this) { pl =>
      headerName match {
        case SeedToSoil       => copy(seed2Soil = seed2Soil :+ pl)
        case Soil2Fertilizer  => copy(soil2Fertilizer = soil2Fertilizer :+ pl)
        case Fertilizer2Water => copy(fertilizer2Water = fertilizer2Water :+ pl)
        case Water2Light      => copy(water2Light = water2Light :+ pl)
        case Light2Temperature =>
          copy(light2Temperature = light2Temperature :+ pl)
        case Temperature2Humidity =>
          copy(temperature2Humidity = temperature2Humidity :+ pl)
        case Humidity2Location =>
          copy(humidity2Location = humidity2Location :+ pl)
      }
    }

  }

  val seedLocations: List[BigInt] = seeds.map(singleSeedLocation)

}

object Almanac {
  val empty: Almanac = Almanac(
    List.empty,
    List.empty,
    List.empty,
    List.empty,
    List.empty,
    List.empty,
    List.empty,
    List.empty
  )

  

}
