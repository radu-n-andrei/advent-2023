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

final case class Symbol(
    lineIndex: Int,
    columnIndex: Int,
    isGear: Boolean
) extends EngineSchematicObject(lineIndex, columnIndex)

object Symbol {
  def apply(lineIndex: Int, columnIndex: Int, c: Char): Symbol =
    Symbol(lineIndex, columnIndex, isGear = c == '*')
}
