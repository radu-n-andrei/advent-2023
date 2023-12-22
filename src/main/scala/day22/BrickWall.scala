package day22

final case class BrickWall(bricks: List[Brick]) {

  def lowerBricks: Unit = {
    val sortedZ = bricks.sorted

    def lower(
        bricks: List[Brick],
        arranged: List[SupportingBrick]
    ): List[SupportingBrick] = {
      bricks match {
        case Nil => arranged
        case b :: bs =>
          val fit =
            fitIn(b, List.empty, arranged.sortBy(_.brick)(Brick.reverseOrder))
          lower(bs, fit)
      }
    }

    def fitIn(
        brick: Brick,
        arranged: List[SupportingBrick],
        toCheck: List[SupportingBrick]
    ): List[SupportingBrick] =
      toCheck match {
        case Nil =>
          arranged :+ SupportingBrick(
            brick.moveByZ(brick.lowestPoint - 1),
            List.empty
          )
        case b :: bs if b.brick.canSupport(brick) =>
          val droppedBrick =
              brick.moveByZ(brick.lowestPoint - b.brick.highestPoint - 1)
          val allValidSupports = bs.filter(sb =>
            sb.brick.highestPoint == b.brick.highestPoint && sb.brick
              .canSupport(brick)
          ) :+ b
          (arranged :+ SupportingBrick(droppedBrick, List.empty)) ++
            allValidSupports.map(b =>
              b.copy(supports = b.supports :+ droppedBrick)
            ) ++ bs.filterNot(allValidSupports.contains)
        case b :: bs =>
          fitIn(brick, arranged :+ b, bs)
      }

    val l = lower(sortedZ, List.empty)
    println(
      s"SOL1: ${l.filter(sb => sb.supports.isEmpty || sb.supports.forall(sup => l.exists(ob => ob.brick != sb.brick && ob.supports.contains(sup)))).length}"
    )
    val supportMap = l.map(sb => sb.brick -> sb.supports).toMap
    val supportedMap =
      l.map(sb => sb.brick -> l.filter(sup => sup.supports.contains(sb.brick)).map(_.brick)).toMap

    val procOrder = l.sortBy(_.brick)(Brick.lowZorderingD)

    def chain(
        bricks: List[Brick],
        total: Int
    ): Unit = {
      bricks match {
        case Nil => println(s"SOL2: ${total}")
        case b :: bs => 
            val curentChain = singleChain(List(b), List.empty)
            chain(bs,  curentChain + total)
      }
    }

    def singleChain(cascade: List[Brick], destroyed: List[Brick]): Int = {
        cascade match {
            case Nil => 
                destroyed.length - 1 //remove the start
            case cs => 
                // bricks supported by current brick
                val affected = cs.flatMap(supportMap(_)).distinct
                // bricks supported by current
                val dest = affected.filter(
                    maybe => supportedMap(maybe).diff(destroyed ++ cascade).isEmpty
                )
                singleChain(dest, destroyed ++ cascade)
        }
    }

    chain(l.map(_.brick).sorted, 0)
    
  }
}
