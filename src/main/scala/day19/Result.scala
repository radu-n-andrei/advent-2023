package day19

sealed trait Result 

sealed trait FinalResult extends Result
case object Accepted extends FinalResult
case object Rejected extends FinalResult
case class Reevaluate(flow: String) extends Result

object Result {
    def apply(s: String): Result = 
        s match {
            case "A" => Accepted
            case "R" => Rejected
            case next => Reevaluate(next)
        }
}
