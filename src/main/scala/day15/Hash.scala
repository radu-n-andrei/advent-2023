package day15

object Hash {

  def hash(s: String): Int = {
    def recHash(str: String, acc: Int): Int = {
      if (str.isEmpty()) acc
      else {
        val h = str.head.toInt
        recHash(str.tail, ((acc + h) * 17) % 256)
      }
    }
    recHash(s, 0)
  }
}
