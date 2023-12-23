val l = List(1, 2, 3, 4)
val c = l.last
val p = List(5, 6, 7, 8, 9, 4, 11)
val i = p.indexOf(c)

val newPath = l ++ p.drop(i + 1)
l.span(_ == 3)