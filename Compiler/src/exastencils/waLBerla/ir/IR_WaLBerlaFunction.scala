package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Function
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_Statement
import exastencils.prettyprinting.PpStream
import exastencils.prettyprinting.PrettyPrintable


trait IR_WaLBerlaFunction extends IR_Function with PrettyPrintable {
  def name : String
  var parameters : ListBuffer[IR_FunctionArgument]
  def body : ListBuffer[IR_Statement]

  allowInlining = false

  def context = IR_WaLBerlaFunctionGenerationContext(this)

  override def prettyprint(out : PpStream) : Unit = {
    if (!functionQualifiers.isEmpty) out << functionQualifiers << ' '
    out << datatype << ' ' << name << ' ' << '(' <<< (modifiedParameters, ", ") << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }

  override def prettyprint_decl() : String = {
    var decl = ""
    if (!functionQualifiers.isEmpty) decl += functionQualifiers + ' '
    decl += datatype.prettyprint + ' ' + name + '(' + modifiedParameters.map(_.prettyprint()).mkString(", ") + ");\n"
    decl
  }

  def modifiedParameters : ListBuffer[IR_FunctionArgument] = context.newFunctionParams.map(p => IR_FunctionArgument(IR_WaLBerlaUtil.getGeneratedName(p.name), p.datatype))
}

case class IR_WaLBerlaLeveledFunction(
    var basename : String,
    var level : Int,
    //var maxLevel : Int,
    var datatype: IR_Datatype,
    var parameters : ListBuffer[IR_FunctionArgument],
    var body : ListBuffer[IR_Statement]) extends IR_WaLBerlaFunction {

  override var name : String = basename + '_' + level
}

case class IR_WaLBerlaPlainFunction(
    var name : String,
    var datatype: IR_Datatype,
    var parameters : ListBuffer[IR_FunctionArgument],
    var body : ListBuffer[IR_Statement]) extends IR_WaLBerlaFunction