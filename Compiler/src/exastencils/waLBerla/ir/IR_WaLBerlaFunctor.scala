package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Function
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_LeveledDslFunctionReference
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_UnitDatatype
import exastencils.prettyprinting.PpStream
import exastencils.prettyprinting.PrettyPrintable


trait IR_WaLBerlaFunctor extends IR_Function with PrettyPrintable {
  def name : String
  def parameters : ListBuffer[IR_FunctionArgument]
  def body : ListBuffer[IR_Statement]

  var datatype : IR_Datatype = IR_UnitDatatype
}

case class IR_WaLBerlaLeveledFunctor(
    var name : String,
    var level : Int,
    var maxLevel : Int,
    var parameters : ListBuffer[IR_FunctionArgument],
    var body : ListBuffer[IR_Statement]) extends IR_WaLBerlaFunctor {

  withNoInline() // disable inlining

  // currently implemented such that the () operator corresponds to the func decl with max level, others are a leveled function
  def funcName = if (level == maxLevel) "operator()" else IR_LeveledDslFunctionReference(name, level, IR_UnitDatatype).name

  override def prettyprint(out : PpStream) : Unit = {
    out << s"void $name::$funcName() {\n"
    for (stmt <- body)
      out << stmt.prettyprint() << "\n"
    out << "}\n"
  }
}

case class IR_WaLBerlaPlainFunctor(
    var name : String,
    var parameters : ListBuffer[IR_FunctionArgument],
    var body : ListBuffer[IR_Statement]) extends IR_WaLBerlaFunctor {

  withNoInline() // disable inlining

  override def prettyprint(out : PpStream) : Unit = {
    out << s"void $name::operator()() {\n"
    for (stmt <- body)
      out << stmt.prettyprint() << "\n"
    out << "}\n"
  }
}