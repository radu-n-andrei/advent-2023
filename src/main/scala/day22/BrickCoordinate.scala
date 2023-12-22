package day22

final case class BrickCoordinate(x: Int, y: Int, z: Int)

object BrickCoordinate {
    def apply(s: String): BrickCoordinate = {
        val pat = "([0-9]+),([0-9]+),([0-9]+)".r
        s match {
            case pat(x,y,z) => BrickCoordinate(x.toInt, y.toInt, z.toInt)
            case _ => throw new RuntimeException(s"Illegal brick coordinate: $s")
        }
    }
        
}