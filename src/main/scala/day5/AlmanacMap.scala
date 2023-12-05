package day5

final case class AlmanacMap(destinationStart: BigInt, sourceStart: BigInt, range: BigInt) {

    def translate(source: BigInt): Option[BigInt] = {
        val index = source - sourceStart
        if(index >= range || index < 0) None
        else Some(destinationStart + index)
    }
        
}

object AlmanacMap {
    def apply(str: List[BigInt]): Option[AlmanacMap] = 
        if(str.length != 3) None
        else Some(AlmanacMap(str(0), str(1), str(2)))

    
}