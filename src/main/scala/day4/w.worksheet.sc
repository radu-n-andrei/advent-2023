val template = s"Card (\\d+): (.*)\\|(.*)".r
val input = "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

input match {
    case template(id, w, y) => 
        println(s"$id has (${w.trim()}) winning numbers. Yours are (${y.trim()})")
    case _ => println("NO MATCH")
}

val s  ="83 86  6 31 17  9 48 53"
val t = s.trim.split(" ").filter(_.nonEmpty)

val l = List(1, 2, 3)
(3 until 3).foldLeft(l)((acc, i) => acc :+ i)