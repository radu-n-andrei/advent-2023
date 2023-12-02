package day1

import scala.io.Source

object Trebuchet {
    

    def main(args: Array[String]): Unit = {
        val file = Source.fromResource("day1.txt")
        println(s"Sol 1: ${totalCoord(file.getLines(), 0)}")
    }  

    private def findCoordinate(s: List[Char], firstCoord: Option[String], lastCoord: Option[String]): Int = 
        s match {
            case Nil => (firstCoord.getOrElse("0") ++ lastCoord.getOrElse("0")).toInt
            case x :: xs if x >= '0' && x <= '9' => 
                findCoordinate(xs, firstCoord.orElse(Some(x.toString())).map(identity), Some(x.toString()))
            case FullDigit(n) => findCoordinate(s.tail, firstCoord.orElse(Some(n.i.toString)).map(identity), Some(n.i.toString))
            case _ :: xs => findCoordinate(xs, firstCoord = firstCoord, lastCoord = lastCoord)    
        }

    private def totalCoord(it: Iterator[String], total: Int): Int = 
        it.nextOption().fold(total){i => 
            println(s"original: $i")
            println(s"coord: ${findCoordinate(i.toList, None, None)}")
            totalCoord(it, total + findCoordinate(i.toList, None, None))
        }

   
}
