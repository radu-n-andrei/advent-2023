package day14

sealed abstract class Rock(val symbol: Char)


case object RoundRock extends Rock('O')
case object SquareRock extends Rock('#')
case object NoRock extends Rock('.')

object Rock {
    def apply(c: Char): Rock = 
        c match {
            case '#' => SquareRock
            case 'O' => RoundRock
            case _ => NoRock
        }
}