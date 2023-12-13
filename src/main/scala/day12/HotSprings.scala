package day12

import scala.io.Source

object HotSprings {
  def main (args: Array[String]): Unit = {
    val file = Source.fromResource("day12.input")
    val input = file.getLines().toList
    val springRows = readInput(input, List.empty)
    val multiplied = springRows.map(s => SpringRow.multiplyBy(s, 5))
    
    println(multiplied.map(_.evaluateWithMaps).sum)
    //println(springRows.head.evaluateAsString())
    //println(multiplied.map(_.pootentialScenarios))
    // println(springRows.zipWithIndex.map {
    //   case (s, i) => s.allPossibilities(5, i)
    // }.sum)
    
    //println(s"SOL1: ${springRows.zipWithIndex.map(x => x._1.allPossibilities(5, x._2)).sum}")
    //println(multiplied.head.pootentialScenarios)

    //println(s"SOL2: ${multiplied.zipWithIndex.map{ case(r, i) => r.fastishEval(Some(i))}.sum}")
    
    //m.pootentialScenarios
    // println(springRows.zipWithIndex.map {
    //     case(r, i) => r.allPossibilities(5, i)
    // }.sum)
   file.close()
  }

  private def readInput(input: List[String], acc: List[SpringRow]): List[SpringRow] = 
    input match {
        case Nil => acc
        case in :: inputs => readInput(inputs, acc :+ SpringRow(in))
    }

  private def print(springRows: List[SpringRow]): Unit = 
    springRows.foreach(r => println(r.toString))  
}
