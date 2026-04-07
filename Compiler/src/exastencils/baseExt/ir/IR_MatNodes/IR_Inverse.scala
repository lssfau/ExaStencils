package exastencils.baseExt.ir.IR_MatNodes

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatOperations.IR_EvalMOpRuntimeExe
import exastencils.baseExt.ir.IR_MatOperations.IR_GenerateRuntimeInversion
import exastencils.baseExt.ir._
import exastencils.config.Settings
import exastencils.datastructures.Transformation.Output
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.fieldlike.ir.IR_MultiDimFieldLikeAccess
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_GeneralSimplify
import exastencils.prettyprinting.PpStream

// pre calculation inverse node: parse arguments(structure information and time of execution) and extract, will be transformed to IR_InverseCT or IR_InverseRT
object IR_IntermediateInv {
  def apply(
      arg : IR_Expression,
      structInfo : IR_MatShape,
      determineStructure : String
  ) = {
    val argexpr = arg match {
      case x : IR_MatrixExpression                               => x
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_MatNodeUtils.accessToMatExpr(va)
      case _                                                     => Logger.error(s"argument of unexpected type: ${ arg }")
    }

    new IR_IntermediateInv(argexpr, structInfo, determineStructure, IR_EvalMOpRuntimeExe("inverse", argexpr.rows) == "Runtime")
  }
  def apply(args : ListBuffer[IR_Expression]) = {

    val mat = args.remove(0)

    // read args to argmap and produce matrix structure
    var msi = IR_MatShape(args)
    var n : Int = 0

    val detShape = msi.shape("detShape")
    if (detShape == "compiletime") {
      Logger.warn("determining matrix structure for inversion at compiletime")
      msi = mat match {
        case _ @ IR_VariableAccess(name, IR_MatrixDatatype(_, rows, _)) =>
          n = rows
          var initVal = IR_PreItMOps.variableCollector.getConstInitVal(name)
          if (!initVal.isDefined) Logger.error("could not retrieve const init value of matrix to classify")
          initVal.get match {
            case me : IR_MatrixExpression => IR_ClassifyMatShape(me)
            case va : IR_VariableAccess   => IR_ClassifyMatShape(IR_MatNodeUtils.accessToMatExpr(va))
            case _                        => Logger.error(s"can not classify initial value of type: ${ initVal.get }")
          }

        case x : IR_MatrixExpression =>
          n = x.rows
          IR_ClassifyMatShape(x)
        case _                       => Logger.error(s"unexpected argument ${ args(0) }, expected matrix as variable or anonymous value")
      }
    }
    val timeOfExe = IR_EvalMOpRuntimeExe("inverse", n)
    Logger.warn(s"Inverting with the following configuration: ${ timeOfExe }, " + msi.toStringList())
    new IR_IntermediateInv(mat, msi, detShape, timeOfExe == "Runtime")
  }
}

case class IR_IntermediateInv(
    arg : IR_Expression,
    msi : IR_MatShape,
    detShape : String,
    resolveAtRuntime : Boolean
) extends IR_RuntimeMNode {
  override def datatype : IR_Datatype = arg.datatype
  override def prettyprint(out : PpStream) : Unit = Logger.error("internal node no resolved!")
  //override def prettyprint(out : PpStream) : Unit = out << "inverseMM(" << arg << ")"
  override def name : String = "inverse"
  override def isExtractable() : Boolean = true
}

// inverse node for compiletime execution
object IR_InverseCT {
  def apply(inv : IR_RuntimeMNode) = {
    val tmp = inv match {
      case i : IR_IntermediateInv => i
      case _                      => Logger.error(s"unexpected type ${ inv }, expected IR_IntermediateInv")
    }
    new IR_InverseCT(tmp.arg, tmp.msi)
  }
}

case class IR_InverseCT(
    arg : IR_Expression,
    msi : IR_MatShape
) extends IR_Expression with IR_ResolvableMNode {
  override def datatype = arg.datatype
  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def resolve() : Output[IR_Expression] = {
    var argexpr = arg match {
      case x : IR_MatrixExpression                                                                                                => x
      case va : IR_VariableAccess if (va.datatype.isInstanceOf[IR_MatrixDatatype] || va.datatype.isInstanceOf[IR_ScalarDatatype]) =>
        // get initial expression to use LU optimization
        //IR_MatNodeUtils.accessToExpression(va)
        val initOp = IR_ResolveMatFuncs.variableCollector.getConstInitVal(va.name)
        if (initOp.isDefined) {
          Logger.warn("inverting initial expression")
        }
        initOp.getOrElse(IR_MatNodeUtils.accessToMatExpr(va)).asInstanceOf[IR_MatrixExpression]
      case fa : IR_FieldLikeAccess if (fa.datatype.isInstanceOf[IR_MatrixDatatype])                                               => IR_MatNodeUtils.accessToMatExpr(fa)
      case fa : IR_MultiDimFieldLikeAccess if (fa.datatype.isInstanceOf[IR_MatrixDatatype])                                       => IR_MatNodeUtils.accessToMatExpr(fa)
      case _                                                                                                                      => Logger.error(s"argument of unexpected type: ${ arg }")
    }
    var tmp = IR_CompiletimeMatOps.inverse(argexpr, msi)
    IR_GeneralSimplify.doUntilDoneStandalone(tmp)
    tmp
  }
  override def isResolvable() : Boolean = IR_MatNodeUtils.isEvaluatable(arg)
}

// inverse node for runtime execution
object IR_InverseRT {
  def apply(dest : IR_Access, inv : IR_RuntimeMNode) = {
    val tmp = inv match {
      case i : IR_IntermediateInv => i
      case _                      => Logger.error(s"unexpected type ${ inv }, expected IR_IntermediateInv")
    }
    new IR_InverseRT(dest, tmp.arg, tmp.msi, tmp.detShape == "runtime")
  }
}

case class IR_InverseRT(
    dest : IR_Access,
    arg : IR_Expression,
    msi : IR_MatShape,
    determineStructureAtRuntime : Boolean
) extends IR_Statement with IR_ResolvableMNode {
  //  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def prettyprint(out : PpStream) = out << "inverseRT(" << arg << ")"
  override def resolve() : Output[IR_Scope] = {
    // add header to find different inverse functions and std::string
    if (!Settings.additionalIncludes.contains("Util/Util.h"))
      Settings.additionalIncludes += "Util/Util.h"
    if (!Settings.additionalIncludes.contains("string"))
      Settings.additionalIncludes += "string"
    if (!Settings.additionalIncludes.contains("iostream"))
      Settings.additionalIncludes += "iostream"

    var newstmts = ListBuffer[IR_Statement]()
    var inMatrix = arg match {
      case x @ IR_MatrixExpression(_, _, _, _) =>
        var decl = IR_MatNodeUtils.expressionToDeclaration(x, "inverse_tmp_")
        newstmts += decl
        IR_VariableAccess(decl)
      case va : IR_VariableAccess              => va
      case fa : IR_FieldLikeAccess             => fa
      case _                                   => Logger.error(s"unexpected argument type: ${ arg }")
    }

    if (determineStructureAtRuntime) {
      // runtime inversion with structure classification
      val destname = dest match {
        case va : IR_VariableAccess  => va.name
        case fa : IR_FieldLikeAccess => fa.name
        case _                       => Logger.error("unexpected type")
      }
      newstmts += IR_GenerateRuntimeInversion.inverseBranchAtRuntime(inMatrix, destname, dest)
    }
    else {
      // plain runtime inversion
      newstmts += IR_GenerateRuntimeInversion.inverse(inMatrix, dest, msi)
    }
    IR_Scope(newstmts)
  }
  override def isResolvable() : Boolean = IR_MatNodeUtils.isEvaluatable(arg)
}