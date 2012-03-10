package scala.tools.nsc
package symtab

import scala.tools.nsc.util.{ SourceFile, Position, OffsetPosition, NoPosition }
import scala.reflect.internal

trait Positions extends internal.Positions {
  self: SymbolTable =>

  def rangePos(source: SourceFile, start: Int, point: Int, end: Int) =
    new OffsetPosition(source, point)

  def validatePositions(tree: Tree) {}

  type Position = scala.tools.nsc.util.Position
  val NoPosition = scala.tools.nsc.util.NoPosition

  def focusPos(pos: Position): Position = pos.focus
  def isRangePos(pos: Position): Boolean = pos.isRange
  def showPos(pos: Position): String = pos.show

}
