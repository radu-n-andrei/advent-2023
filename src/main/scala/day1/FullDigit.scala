package day1

import scala.util.matching.Regex

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

  def unapply(s: List[Char]): Option[String] =
    s.mkString match {
      case one()   => Some("1")
      case two()   => Some("2")
      case three() => Some("3")
      case four()  => Some("4")
      case five()  => Some("5")
      case six()   => Some("6")
      case seven() => Some("7")
      case eight() => Some("8")
      case nine()  => Some("9")
      case zero()  => Some("0")
      case _       => None
    }
}
