package day3

sealed abstract class EngineSchematicObject(lineIndex: Int, columnIndex: Int)

final case class EnginePart(lineIndex: Int, columnIndex: Int, number: String)
    extends EngineSchematicObject(lineIndex, columnIndex) {
  def isSymbolAdjacent(symbol: Symbol): Boolean = {
    // on the same line
    (lineIndex == symbol.lineIndex && (columnIndex == symbol.columnIndex + 1 || symbol.columnIndex == columnIndex + number
      .length())) ||
    // on adjacent lines
    ((lineIndex == symbol.lineIndex - 1 || lineIndex == symbol.lineIndex + 1) &&
      symbol.columnIndex >= columnIndex - 1 && symbol.columnIndex <= columnIndex + number
        .length())

  }
}

sealed abstract class Symbol(
    val lineIndex: Int,
    val columnIndex: Int,
    val isGear: Boolean
) extends EngineSchematicObject(lineIndex, columnIndex)

final case class Gear(
    override val lineIndex: Int,
    override val columnIndex: Int
) extends Symbol(lineIndex, columnIndex, isGear = true)

final case class JunkPart(
    override val lineIndex: Int,
    override val columnIndex: Int
) extends Symbol(lineIndex, columnIndex, isGear = false)

object Symbol {
  def apply(lineIndex: Int, columnIndex: Int, c: Char): Symbol =
    if (c == '*') Gear(lineIndex, columnIndex)
    else JunkPart(lineIndex, columnIndex)
}
