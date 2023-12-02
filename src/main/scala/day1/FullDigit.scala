package day1

import scala.util.matching.Regex

final case class FullDigit(i: Int)

object FullDigit {

  private val one = "one.*".r
  private val two = "two.*".r
  private val three = "three.*".r
  private val four = "four.*".r
  private val five = "five.*".r
  private val six = "six.*".r
  private val seven = "seven.*".r
  private val eight = "eight.*".r
  private val nine = "nine.*".r
  private val zero = "zero.*".r

  def unapply(s: List[Char]): Option[FullDigit] =
    s.mkString match {
      case one()   => Some(FullDigit(1))
      case two()   => Some(FullDigit(2))
      case three() => Some(FullDigit(3))
      case four()  => Some(FullDigit(4))
      case five()  => Some(FullDigit(5))
      case six()   => Some(FullDigit(6))
      case seven() => Some(FullDigit(7))
      case eight() => Some(FullDigit(8))
      case nine()  => Some(FullDigit(9))
      case zero()  => Some(FullDigit(0))
      case _       => None
    }
}
