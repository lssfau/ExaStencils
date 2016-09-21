package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils._
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.baseExt.l4.L4_LoopOverField
import exastencils.core._
import exastencils.datastructures.ir._
import exastencils.field.l4.L4_FieldAccess
import exastencils.logger._
import exastencils.prettyprinting._

trait HasIdentifier {
  var identifier : Identifier
}

case class AssignmentStatement(var dest : Access, var src : L4_Expression, var op : String) extends L4_Statement {
  override def prettyprint(out : PpStream) = { out << dest << ' ' << op << ' ' << src << '\n' }

  override def progress : IR_Assignment = {
    IR_Assignment(dest.progress, src.progress, op)
  }
}

case class ColorWithStatement(var colors : List[L4_Expression], var loop : L4_LoopOverField) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "color with {\n"
    out <<< (colors, ",\n") << ",\n"
    out << loop
    out << "}\n"
  }

  override def progress : IR_Scope = {
    // TODO: think about extracting loop duplication to separate transformation
    var loops = colors.map(color => {
      var newLoop = Duplicate(loop)
      if (newLoop.condition.isDefined)
        newLoop.condition = Some(L4_AndAndExpression(newLoop.condition.get, color))
      else
        newLoop.condition = Some(color)
      newLoop
    })

    IR_Scope(loops.map(_.progress : IR_Statement).to[ListBuffer])
  }
}

case class AdvanceStatement(var field : Access) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "advance "
    field.prettyprint(out)
    out << '\n'
  }

  override def progress = {
    data.AdvanceSlotStatement(iv.CurrentSlot(field.asInstanceOf[L4_FieldAccess].progress.fieldSelection.field,
      IR_StringLiteral(IR_LoopOverFragments.defIt)))
  }
}

case class LeveledScopeStatement(var level : LevelSpecification, var statements : List[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << level << " {\n"
    statements.foreach(_.prettyprint(out))
    out << "\n}\n"
  }
  override def progress = {
    Logger.error("cannot progress LeveledScopeStatement to IR")
  }
}
