package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.prettyprinting.PpStream

/// L4_ColorLoops

object L4_ColorLoops {
  def apply(colors : List[L4_Expression], loop : L4_LoopOverField) =
    new L4_ColorLoops(colors.to[ListBuffer], loop)
}

case class L4_ColorLoops(var colors : ListBuffer[L4_Expression], var loop : L4_LoopOverField) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "color with {\n" <<< (colors, ",\n") << ",\n" << loop << "}\n"

  override def progress : IR_Scope = {
    // TODO: extract loop duplication to separate transformation
    val loops = colors.map(color => {
      val newLoop = Duplicate(loop)
      if (newLoop.condition.isDefined)
        newLoop.condition = Some(L4_AndAndExpression(newLoop.condition.get, color))
      else
        newLoop.condition = Some(color)
      newLoop
    })

    IR_Scope(loops.map(_.progress : IR_Statement))
  }
}
