package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_ColorLoops

object L4_ColorLoops {
  def apply(colors : List[L4_Expression], stmts : List[L4_Statement]) =
    new L4_ColorLoops(colors.to[ListBuffer], stmts.to[ListBuffer])
}

case class L4_ColorLoops(var colors : ListBuffer[L4_Expression], var stmts : ListBuffer[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "color with {\n" <<< (colors, ",\n") << ",\n" <<< (stmts, "\n") << "\n}"

  def generateStmtsForColor(color : L4_Expression) = {
    val newStmts = Duplicate(stmts)
    newStmts.transform {
      case loop : L4_LoopOverField =>
        if (loop.condition.isDefined)
          loop.condition = Some(L4_AndAnd(loop.condition.get, color))
        else
          loop.condition = Some(color)
        loop

      case other =>
        Logger.warn("Ignoring statement while coloring: " + other)
        other
    }
    newStmts
  }

  override def progress : IR_Scope = ProgressLocation {
    // TODO: extract loop duplication to separate transformation
    val newStmts = colors.flatMap(generateStmtsForColor)
    IR_Scope(newStmts.map(_.progress : IR_Statement))
  }
}
