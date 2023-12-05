package day5

sealed abstract class  AlmanacHeader(val headerName: String)

case object SeedToSoil extends AlmanacHeader("seed-to-soil map:")
case object Soil2Fertilizer extends AlmanacHeader("soil-to-fertilizer map:")
case object Fertilizer2Water extends AlmanacHeader("fertilizer-to-water map:")
case object Water2Light extends AlmanacHeader("water-to-light map:")
case object Light2Temperature extends AlmanacHeader("light-to-temperature map:")
case object Temperature2Humidity extends AlmanacHeader("temperature-to-humidity map:")
case object Humidity2Location extends AlmanacHeader("humidity-to-location map:")


object AlmanacHeader {
    def unapply(s: String): Option[AlmanacHeader] = 
        s match {
            case SeedToSoil.headerName => Some(SeedToSoil)
            case Soil2Fertilizer.headerName => Some(Soil2Fertilizer)
            case Fertilizer2Water.headerName => Some(Fertilizer2Water)
            case Water2Light.headerName => Some(Water2Light)
            case Light2Temperature.headerName => Some(Light2Temperature)
            case Temperature2Humidity.headerName => Some(Temperature2Humidity)
            case Humidity2Location.headerName => Some(Humidity2Location)
            case _ => None
        } 
}