package day19

final case class MachinePart(x: Int, m: Int, a: Int, s: Int) {
  val score: Int = x + m + a + s
  def characteristic(p: PartCharacteristic): Int =
    p match {
      case X => x
      case M => m
      case A => a
      case S => s
    }
}

object MachinePart {
  def apply(str: String): MachinePart = {
    val pat = """\{x=([0-9]+),m=([0-9]+),a=([0-9]+),s=([0-9]+)\}""".r
    str match {
      case pat(x, m, a, s) => MachinePart(x.toInt, m.toInt, a.toInt, s.toInt)
      case _ => throw new RuntimeException(s"Illegal part description $str")
    }
  }
}

sealed trait PartCharacteristic
case object X extends PartCharacteristic
case object M extends PartCharacteristic
case object A extends PartCharacteristic
case object S extends PartCharacteristic

object PartCharacteristic {
  def apply(c: Char): PartCharacteristic = {
    c match {
      case 'x' => X
      case 'm' => M
      case 'a' => A
      case 's' => S
      case _   => throw new RuntimeException(s"Illegal part characteristic: $c")
    }
  }
}
