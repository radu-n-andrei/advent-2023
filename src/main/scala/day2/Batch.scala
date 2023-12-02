package day2

final case class Batch(red: Int, green: Int, blue: Int)

object Batch {
  def isValid(batch: Batch, rules: GameRules): Boolean =
    batch.red <= rules.red &&
      batch.green <= rules.green &&
      batch.blue <= rules.blue

  private def extraction(color: String, input: String): Int = {
    val regex = s".*\\s([0-9]+)\\s$color.*".r
    input match {
      case regex(r) => r.toInt
      case _        => 0
    }
  }

  def apply(str: String): Batch =
    Batch(
      extraction("red", str),
      extraction("green", str),
      extraction("blue", str)
    )

}
