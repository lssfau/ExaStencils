package exastencils.baseExt.ir.IR_MatrixFunctionNodes

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Scope
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_CompiletimeInversion
import exastencils.baseExt.ir.IR_DetermineMatrixStructure
import exastencils.baseExt.ir.IR_GenerateRuntimeInversion
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.baseExt.ir.IR_MatrixNodeUtilities
import exastencils.baseExt.ir.IR_PreItMOps
import exastencils.baseExt.ir.matStructInfo
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.core.Duplicate
import exastencils.core.StateManager
import exastencils.datastructures.Transformation.Output
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream


abstract class IR_Inverse(
    arg : IR_Expression,
    msi : matStructInfo
) {
}

// pre calculation inverse node: parse arguments(structure information and time of execution) and extract, will be transformed to IR_InverseCT or IR_InverseRT
object IR_IntermediateInv {
  def apply(
      arg : IR_Expression,
      structInfo : matStructInfo,
      determineStructure : String
  ) = {
    var argexpr = arg match {
      case x : IR_MatrixExpression                               => x
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_MatrixNodeUtilities.accessToExpression(va)
      case _                                                     => Logger.error(s"argument of unexpected type: ${ arg }")
    }
    new IR_IntermediateInv(argexpr, structInfo, determineStructure, Knowledge.experimental_resolveInverseFunctionCall == "Runtime")
  }
  def apply(args : ListBuffer[IR_Expression]) = {
    var determineStructure = "no"
    var msi = matStructInfo("Filled", -1, "-1", -1)
    // check arguments 2 to 5
    args.length match {
      case 0 =>
        Logger.error("no argument passed, expected a matrix!")
      case 1 =>
      case _ =>
        args(1) match {
          case IR_StringConstant(s @ ("DetermineRuntime" | "DetermineCompiletime")) => determineStructure = s
          case IR_StringConstant(s @ ("Diagonal" | "Filled"))                       => msi.structure = s
          case IR_StringConstant(s @ "Blockdiagonal")                               =>
            msi.structure = s
            if (args.length != 3)
              Logger.error("Blockdiagonal matrix specified but no blocksize given!")
            args(2) match {
              case IR_VariableAccess(_, d @ IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
              case IR_IntegerConstant(c)                        => msi.blocksize = c.toInt
              case _                                            => Logger.error(s"blocksize of unexpected type: ${ args }, expected integer constant or access to integer variable")
            }
          case IR_StringConstant(s @ "Schur")                                       =>
            msi.structure = s
            if (args.length < 3)
              Logger.error("schur matrix specified but no blocksize given!")
            args(2) match {
              case IR_VariableAccess(_, IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
              case IR_IntegerConstant(c)                    => msi.blocksize = c.toInt
              case _                                        => Logger.error(s"blocksize is of unexpected type: ${ args }, expected integer constant or access to integer variable")
            }
            if (args.length < 4) {
              msi.structureA = "Filled"
            }
            else {
              args(3) match {
                case IR_StringConstant(s @ ("Filled" | "Diagonal")) => msi.structureA = s
                case IR_StringConstant(s @ "Blockdiagonal")         =>
                  msi.structureA = s
                  if (args.length != 5)
                    Logger.error("Blockdiagonal specified for A matrix in schur structure matrix but no blocksize given!")
                  args(4) match {
                    case IR_VariableAccess(_, d @ IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
                    case IR_IntegerConstant(c)                        => msi.blocksizeA = c.toInt
                    case _                                            => Logger.error(s"blocksize for blockdiagonal matrix A is of unexpected type: ${ args }, expected integer constant or access to integer variable")

                  }
                case _                                              => Logger.error(s"unexpected type for upper left matrix in schur structure matrix: ${ args(3) }, expected Filled or Diagonal or Blockdiagonal")
              }
            }
          case _                                                                    => Logger.error(s"unexpected argument combination: ${ args }, expected: 'Determine' or 'Filled' or 'Diagonal' without additional arguments or 'Blockdiagonal' with a blocksize as 3rd argument or 'Schur' with a blocksize which specifies the width of the lower right matrix D as 3rd argument and additionally the structure of the upper left matrix A as 4th argument and its blocksize as 5th in case of a blockdiagonal matrix for A;" +
            "in short: " +
            "inverse(mat) or inverse(mat, Filled) or inverse(mat, Determine) or inverse(mat,Diagonal) or inverse(mat, Blockdiagonal, blocksize) or inverse(mat, Schur, blocksize) or inverse(mat, Schur, blocksize, Filled) or inverse(mat, Schur, blocksize, Diagonal) or inverse(mat, Schur, blocksize, Blockdiagonal, blocksize_A")
        }
    }

    if (determineStructure == "DetermineCompiletime") {
      Logger.warn("determining matrix structure for inversion at compiletime")
      msi = args(0) match {
        case va @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)) =>
          var foundDecls = StateManager.findAll[IR_VariableDeclaration]().filter(d => d.name == name)
          val decl = IR_PreItMOps.variableCollector.lastDecl(name).getOrElse(Logger.error("declaration not found"))
          decl.initialValue match {
            case None                                   => Logger.error("trying to classify not initialized matrix variable at compiletime!")
            case Some(x @ IR_MatrixExpression(_, _, _)) =>
              //if (IR_MatrixNodeUtilities.notWrittenTo(name)) {
              if(!IR_PreItMOps.variableCollector.writeInScope(name)) {
                IR_DetermineMatrixStructure.isOfStructure(x)
              }
              else
                Logger.error("found assignment to matrix input that was to classify, cannot classify non compiletime constant matrices!")
            case _                                      => Logger.error(s"unexpected initialization value: ${ decl.initialValue }, expected matrix expression!")
          }
        case x : IR_MatrixExpression =>
          IR_DetermineMatrixStructure.isOfStructure(x)
      }
    }
    Logger.warn(s"Inverting with the following configuration: ${ Knowledge.experimental_resolveInverseFunctionCall }, ${ (msi.structure,msi.blocksize,msi.structureA,msi.blocksizeA) }")
    new IR_IntermediateInv(args(0), msi, determineStructure, Knowledge.experimental_resolveInverseFunctionCall == "Runtime")
  }
}
case class IR_IntermediateInv(
    arg : IR_Expression,
    msi : matStructInfo,
    determineStructure : String,
    resolveAtRuntime : Boolean
) extends IR_Inverse(arg, msi) with IR_ExtractableMNode {
  override def datatype : IR_Datatype = arg.datatype
  //override def prettyprint(out : PpStream) : Unit = Logger.error("internal node no resolved!")
  override def prettyprint(out : PpStream) : Unit = out << "inverseMM(" << arg << ")"
  override def isExtractable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}

// inverse node for compiletime execution
object IR_InverseCT {
  def apply(inv : IR_IntermediateInv) = {
    new IR_InverseCT(inv.arg, inv.msi)
  }
  def apply(arg : IR_Expression, structureInformation : matStructInfo) = {
    new IR_InverseCT(arg, structureInformation)
  }
}
case class IR_InverseCT(
    arg : IR_Expression,
    msi : matStructInfo = matStructInfo("Filled", -1, "-1", -1)
) extends IR_Inverse(arg, msi) with IR_ResolvableMNode {
  override def datatype = arg.datatype
  //  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def prettyprint(out : PpStream) : Unit = out << "inverseCT(" << arg << ")"
  override def resolve() : Output[IR_Expression] = {
    var argexpr = arg match {
      case x : IR_MatrixExpression                               => x
      case va @ IR_VariableAccess(_, _) => IR_MatrixNodeUtilities.accessToExpression(va)
      case _                                                     => Logger.error(s"argument of unexpected type: ${ arg }")
    }
    var tmp = IR_CompiletimeInversion.inverse(argexpr, msi)
    //IR_GeneralSimplify.doUntilDoneStandalone(tmp)
    tmp
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}

// inverse node for runtime execution
object IR_InverseRT {
  def apply(dest : IR_VariableAccess, inv : IR_IntermediateInv) = {
    new IR_InverseRT(dest, inv.arg, inv.msi, inv.determineStructure == "DetermineRuntime")
  }
}
case class IR_InverseRT(
    dest : IR_VariableAccess,
    arg : IR_Expression,
    msi : matStructInfo,
    determineStructureAtRuntime : Boolean
) extends IR_Inverse(arg,msi) with IR_ResolvableMNode {
  override def datatype = arg.datatype
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
      case x @ IR_MatrixExpression(_, _, _) =>
        var decl = IR_MatrixNodeUtilities.expressionToDeclaration(x)
        newstmts += decl
        IR_VariableAccess(Duplicate(decl))
      case va : IR_VariableAccess           => va
      case _                                => Logger.error(s"unexpected argument type: ${ arg }")
    }

    if (determineStructureAtRuntime) {
      // runtime inversion with structure classification
      newstmts += IR_GenerateRuntimeInversion.inverseBranchAtRuntime(inMatrix, dest.name, dest)
    }
    else {
      // plain runtime inversion
      newstmts += IR_GenerateRuntimeInversion.inverse(inMatrix, dest, msi)
    }
    IR_Scope(newstmts)
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}