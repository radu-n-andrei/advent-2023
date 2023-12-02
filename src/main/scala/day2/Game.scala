package day2

final case class Game(id: Int, batches: List[Batch]) {

  val minBatch: Batch = batches.foldLeft(Batch(0, 0, 0))((acc, b) =>
    Batch(
      Math.max(acc.red, b.red),
      Math.max(acc.green, b.green),
      Math.max(acc.blue, b.blue)
    )
  )

  val power: Int =
    minBatch.red * minBatch.blue * minBatch.green

}

object Game {

  val empty: Game = Game(0, List.empty)

  def apply(input: String): Game = {
    val template = "Game\\s([0-9]+):(.*)".r
    input match {
      case template(id, batches) =>
        Game(id.toInt, batches.split(";").toList.map(Batch(_)))
      case _ => Game.empty
    }
  }

  def isValid(g: Game, rules: GameRules): Boolean =
    g.batches.nonEmpty &&
      g.batches.forall(b => Batch.isValid(b, rules))
}
