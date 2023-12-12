package day12

sealed abstract class Spring(val symbol: Char, val isUnknown: Boolean)
case object FunctionalSpring extends Spring('.', false)
case object DamagedSpring extends Spring('#', false)
case object Unknown extends Spring('?', true)

object Spring {
    def apply(c: Char): Spring = 
        c match {
            case '.' => FunctionalSpring
            case '?' => Unknown
            case '#' => DamagedSpring
            case x => throw new RuntimeException(s"Illegal spring type: $x")
        }

}
