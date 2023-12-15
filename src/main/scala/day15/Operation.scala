package day15

sealed trait Operation

final case class Add(focalStrength: Int) extends Operation
case object Remove extends Operation
