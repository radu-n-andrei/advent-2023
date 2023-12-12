package day12

sealed trait Grouping
final case class Valid(grouping: String, remaining: List[Spring], next: List[Spring]) extends Grouping
final case class Invalid(reason: Reason) extends Grouping

sealed trait Reason
final case class InvalidGrouping(remaining: List[Spring]) extends Reason
final case object InvalidGroupingFatal extends Reason
final case object InsufficientElements extends Reason
