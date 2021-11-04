package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Function
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_FutureFunction
import exastencils.base.ir.IR_LeveledDslFunctionReference
import exastencils.base.ir.IR_LeveledFunction
import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_PlainDslFunctionReference
import exastencils.base.ir.IR_PlainFunction
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_FieldAccessLike
import exastencils.prettyprinting.PpStream
import exastencils.prettyprinting.PrettyPrintable
import exastencils.util.ir.IR_StackCollector


trait IR_WaLBerlaFunction extends IR_Function with PrettyPrintable {
  def name : String
  var parameters : ListBuffer[IR_FunctionArgument]
  def body : ListBuffer[IR_Statement]

  var isInterfaceFunction = true

  override def prettyprint(out : PpStream) : Unit = {
    if (!functionQualifiers.isEmpty) out << functionQualifiers << ' '
    out << datatype << ' ' << (if (isInterfaceFunction) IR_WaLBerlaInterface.interfaceName + "::" else "") << name << ' ' << '(' <<< (modifiedParameters, ", ") << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }

  override def prettyprint_decl() : String = {
    var decl = ""
    if (!functionQualifiers.isEmpty) decl += functionQualifiers + ' '
    decl += datatype.prettyprint + ' ' + name + '(' + modifiedParameters.map(_.prettyprint()).mkString(", ") + ");\n"
    decl
  }

  def modifiedParameters : ListBuffer[IR_FunctionArgument] = parameters.map(p => IR_FunctionArgument(IR_WaLBerlaUtil.getGeneratedName(p.name), p.datatype))
}

case class IR_WaLBerlaLeveledFunction(
    var basename : String,
    var level : Int,
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


object IR_WaLBerlaSetupFunctions extends DefaultStrategy("Transform functions accessing wb data structures to wb functions.") {
  var stackCollector = new IR_StackCollector
  this.register(stackCollector)

  var plainFunctions : ListBuffer[IR_PlainFunction] = ListBuffer()
  var leveledFunctions : ListBuffer[IR_LeveledFunction] = ListBuffer()

  override def apply(applyAtNode : Option[Node]) : Unit = {
    plainFunctions.clear()
    leveledFunctions.clear()

    super.apply(applyAtNode)
  }

  def findEnclosingFunction(stack : List[IR_Node]) : Unit = {
    val enclosingFunction = stack.collectFirst { case f : IR_Function => f }
    if (enclosingFunction.isDefined) {
      enclosingFunction.get match {
        case plain : IR_PlainFunction     => plainFunctions += plain
        case leveled : IR_LeveledFunction => leveledFunctions += leveled
        case _ : IR_WaLBerlaFunction      =>
      }
    }
  }

  // candidates are functions accessing waLBerla data structures
  this += Transformation("Find candidate functions", {
    case fAcc : IR_FieldAccessLike if IR_WaLBerlaFieldCollection.contains(fAcc) =>
      findEnclosingFunction(stackCollector.stack)
      fAcc
    case fAcc : IR_WaLBerlaFieldAccess                                          =>
      findEnclosingFunction(stackCollector.stack)
      fAcc
    case vAcc : IR_VariableAccess if vAcc == IR_WaLBerlaUtil.getBlockForest =>
      findEnclosingFunction(stackCollector.stack)
      vAcc
    case iv : IR_IV_WaLBerlaFieldData                                       =>
      findEnclosingFunction(stackCollector.stack)
      iv
  })

  // find functions that call one of the candidates and make them candidates as well
  this += Transformation("Find functions calling candidates", {
    case funcCall : IR_FunctionCall =>
      val isCandidate = funcCall.function match {
        case _ : IR_PlainDslFunctionReference if plainFunctions.exists { f : IR_PlainFunction => f.name == funcCall.name }       =>
          true
        case _ : IR_LeveledDslFunctionReference if leveledFunctions.exists { f : IR_LeveledFunction => f.name == funcCall.name } =>
          true
        case _                                                                                                                   =>
          false
      }

      if (isCandidate)
        findEnclosingFunction(stackCollector.stack)

      funcCall
  })

  this += Transformation("Replace candidates with wb functions", {
    case plain @ IR_PlainFunction(name, dt, param, body) if plainFunctions.contains(plain)                  =>
      val func = IR_WaLBerlaPlainFunction(name, dt, param, body)
      func.allowInlining = plain.allowInlining
      IR_WaLBerlaCollection.get.functions += func
      None
    case leveled @ IR_LeveledFunction(baseName, lvl, dt, param, body) if leveledFunctions.contains(leveled) =>
      val func = IR_WaLBerlaLeveledFunction(baseName, lvl, dt, param, body)
      func.allowInlining = leveled.allowInlining
      IR_WaLBerlaCollection.get.functions += func
      None
  })
}

trait IR_WaLBerlaFutureFunction extends IR_FutureFunction {
  allowInlining = false
}

trait IR_WaLBerlaFuturePlainFunction extends IR_WaLBerlaFutureFunction {
  override def generateFct() : IR_WaLBerlaPlainFunction
}

trait IR_WaLBerlaFutureLeveledFunction extends IR_WaLBerlaFutureFunction {
  def level : Int
  override def generateFct() : IR_WaLBerlaLeveledFunction
}