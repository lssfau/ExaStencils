package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_FieldAccessLike
import exastencils.parallelization.api.cuda.CUDA_KernelFunctions
import exastencils.prettyprinting.PpStream
import exastencils.prettyprinting.PrettyPrintable
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlocks
import exastencils.waLBerla.ir.field._
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes._
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

trait IR_WaLBerlaFunction extends IR_Function with PrettyPrintable {
  def name : String
  var parameters : ListBuffer[IR_FunctionArgument]
  def body : ListBuffer[IR_Statement]

  var isInterfaceFunction = true
  var isUserFunction = true

  override def prettyprint(out : PpStream) : Unit = {
    if (!functionQualifiers.isEmpty) out << functionQualifiers << ' '
    if (isInterfaceFunction) out << s"template< typename $WB_StencilTemplate >\n"
    out << datatype << ' ' << (if (isInterfaceFunction) IR_WaLBerlaInterface.interfaceName + s"<$WB_StencilTemplate>" + "::" else "") << name << ' ' << '(' <<< (modifiedParameters, ", ") << ") {\n"
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
    var baseName : String,
    var level : Int,
    var datatype : IR_Datatype,
    var parameters : ListBuffer[IR_FunctionArgument],
    var body : ListBuffer[IR_Statement]) extends IR_WaLBerlaFunction with IR_LeveledFunctionLike {

  override var name : String = baseName + '_' + level
}

case class IR_WaLBerlaPlainFunction(
    var name : String,
    var datatype : IR_Datatype,
    var parameters : ListBuffer[IR_FunctionArgument],
    var body : ListBuffer[IR_Statement]) extends IR_WaLBerlaFunction with IR_PlainFunctionLike

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
    // don't transform generated CUDA kernel functions
    val enclosingFunction = stack.collectFirst { case f : IR_Function if Knowledge.cuda_enabled && !CUDA_KernelFunctions.get.functions.contains(f) => f }
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
    case acc : IR_Access if acc == IR_WaLBerlaBlockForest().resolveAccess()     =>
      findEnclosingFunction(stackCollector.stack)
      acc
    case loop : IR_WaLBerlaLoopOverBlocks                                       =>
      findEnclosingFunction(stackCollector.stack)
      loop
    case bf : IR_WaLBerlaBlockForest                                            =>
      findEnclosingFunction(stackCollector.stack)
      bf
    case iv : IR_IV_WaLBerlaGetField                                            =>
      findEnclosingFunction(stackCollector.stack)
      iv
    case iv : IR_IV_WaLBerlaFieldData                                           =>
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

  def isInterfaceFunction : Boolean
}

trait IR_WaLBerlaFuturePlainFunction extends IR_WaLBerlaFutureFunction {
  def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction

  override def generateFct() : IR_WaLBerlaPlainFunction = {
    val f = generateWaLBerlaFct()
    f.isUserFunction = false
    f.isInterfaceFunction = isInterfaceFunction
    f
  }
}

trait IR_WaLBerlaFutureLeveledFunction extends IR_WaLBerlaFutureFunction {
  def level : Int

  def generateWaLBerlaFct() : IR_WaLBerlaLeveledFunction

  override def generateFct() : IR_WaLBerlaLeveledFunction = {
    val f = generateWaLBerlaFct()
    f.isUserFunction = false
    f.isInterfaceFunction = isInterfaceFunction
    f
  }
}