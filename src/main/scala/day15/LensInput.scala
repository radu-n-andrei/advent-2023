package day15

final case class LensInput(
    box: Int,
    label: String,
    operation: Operation,
    hash: Int
)

object LensInput {

  def apply(s: String): LensInput = {
    val addPat = "([a-z]+)=([0-9])".r
    val remPat = "([a-z]+)-".r
    s match {
      case addPat(l, fs) =>
        LensInput(Hash.hash(l), l, Add(fs.toInt), Hash.hash(s))
      case remPat(l) => LensInput(Hash.hash(l), l, Remove, Hash.hash(s))
      case _         => throw new RuntimeException(s"Illegal lens input: $s")
    }
  }
}
