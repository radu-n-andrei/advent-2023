val i = 3
val reg = ("""[\?|\#]{""" + i +"""}[\.|\?]*$""").r

val s = "?#.?"
reg.matches(s)

val fatalReg = """.*\#+.*\.+.*""".r
fatalReg.matches(s)