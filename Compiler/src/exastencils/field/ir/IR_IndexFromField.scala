package exastencils.field.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.interfacing.ir.IR_ExternalFieldCollection
import exastencils.prettyprinting._

/// IR_IV_IndexFromField

case class IR_IV_IndexFromField(var layoutIdentifier : String, var level : IR_Expression, var indexId : String, var dim : Int, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, true, true, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, layoutIdentifier, level, IR_NullExpression)

  override def usesFieldArrays : Boolean = false
  override def usesLevelArrays : Boolean = true

  override def resolveName() = s"idx$indexId" + resolvePostfix(fragmentIdx.prettyprint, "", layoutIdentifier, level.prettyprint, "") + s"_dim"
  override def resolveDatatype() = IR_IntegerDatatype

  override def getCtor() : Option[IR_Statement] = {
    // TODO: refactor -> looking up a field by its layout is not suitable
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val oldLev = level
    for (l <- Knowledge.minLevel to Knowledge.maxLevel) {
      level = l
      val field = IR_FieldCollection.getByLayoutIdentifier(layoutIdentifier, l, true)
      if (field.isDefined) {
        statements += IR_Assignment(resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, layoutIdentifier, level, IR_NullExpression),
          field.get.layout.defIdxById(indexId, dim))
      } else {
        // no field found -> try external fields
        val extField = IR_ExternalFieldCollection.getByLayoutIdentifier(layoutIdentifier, l, true)
        if (extField.isDefined) {
          statements += IR_Assignment(resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, layoutIdentifier, level, IR_NullExpression),
            extField.get.fieldLayout.defIdxById(indexId, dim))
        } else {
          // doesn't exist on this level
        }
      }
    }
    level = oldLev
    Some(new IR_LoopOverFragments(statements))
  }
}
