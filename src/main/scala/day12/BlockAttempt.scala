package day12

final case class BlockAttempt(index: Int, nextUncheckedDmgIndex: Option[Int])

final case class BlockAttemptSave(startIndex: Int, survey: List[Int])
