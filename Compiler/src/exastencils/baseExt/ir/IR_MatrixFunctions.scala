package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Scope
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_UnitDatatype
import exastencils.base.ir.IR_UnknownDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.core.StateManager
import exastencils.logger.Logger
import exastencils.optimization.ir.EvaluationException
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.prettyprinting.PpStream

trait IR_ResolvableMNode extends IR_Expression {
  def isResolvable() : Boolean
}

trait IR_MExpressionFunction extends IR_ResolvableMNode {
  def resolve() : IR_Expression
}

trait IR_MStatementFunction extends IR_ResolvableMNode {
  def resolve() : IR_Scope
}

trait IR_ExtractableMNode extends IR_Expression {
  def isExtractable() : Boolean
}

//TODO necessary?
trait IR_InlinableMNode extends IR_Expression {
  def isInlineable() : Boolean
}

object IR_InlineableDeclaration {
  def apply(datatype : IR_Datatype, name : String, initialValue : IR_Expression) = {
    new IR_InlineableDeclaration(datatype, name, initialValue)
  }
}

case class IR_InlineableDeclaration(
    datatype : IR_Datatype,
    name : String,
    initialValue : IR_Expression
) extends IR_Statement {

  def isInlineable() : Boolean = {
      (initialValue match {
        case inv : IR_Inverse     => IR_BasicMatrixOperations.getSize(inv.arg)._1 < 4 && !inv.resolveAtRuntime
        case det : IR_Determinant => false//getSize(det.arg)._1 < 4 && !det.resolveAtRuntime
        case gs : IR_GetSlice     => false//!gs.resolveAtRuntime
        case _                    => false
      })
  }

  override def prettyprint(out : PpStream) : Unit = Logger.error("internal node not resolved")
}


object IR_Inverse {
  def apply(
      arg : IR_Expression,
      structureInformation : (String, Int, String, Int),
      determineStructure : String
  ) = {
    var argexpr = arg match {
      case x : IR_MatrixExpression                               => x
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_MatrixNodeUtilities.accessToExpression(va)
      case _                                                     => Logger.error(s"argument of unexpected type: ${ arg }")
    }
    new IR_Inverse(argexpr, structureInformation, determineStructure, Knowledge.experimental_resolveInverseFunctionCall == "Runtime")
  }

  def apply(args : ListBuffer[IR_Expression]) = {
    var determineStructure = "no"
    var matrixStructure = "Filled"
    var blocksize = -1
    var matrixStructure_A = "no schur"
    var blocksize_A = -1
    // check arguments 2 to 5
    args.length match {
      case 0 =>
        Logger.error("no argument passed, expected a matrix!")
      case 1 =>
      case _ =>
        args(1) match {
          case IR_StringConstant(s @ ("DetermineRuntime" | "DetermineCompiletime")) => determineStructure = s
          case IR_StringConstant(s @ ("Diagonal" | "Filled"))                       => matrixStructure = s
          case IR_StringConstant(s @ "Blockdiagonal")                               =>
            matrixStructure = s
            if (args.length != 3)
              Logger.error("Blockdiagonal matrix specified but no blocksize given!")
            args(2) match {
              case IR_VariableAccess(_, d @ IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
              case IR_IntegerConstant(c)                        => blocksize = c.toInt
              case _                                            => Logger.error(s"blocksize of unexpected type: ${ args }, expected integer constant or access to integer variable")
            }
          case IR_StringConstant(s @ "Schur")                                       =>
            matrixStructure = s
            if (args.length < 3)
              Logger.error("schur matrix specified but no blocksize given!")
            args(2) match {
              case IR_VariableAccess(_, IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
              case IR_IntegerConstant(c)                    => blocksize = c.toInt
              case _                                        => Logger.error(s"blocksize is of unexpected type: ${ args }, expected integer constant or access to integer variable")
            }
            if (args.length < 4) {
              matrixStructure_A = "Filled"
            }
            else {
              args(3) match {
                case IR_StringConstant(s @ ("Filled" | "Diagonal")) => matrixStructure_A = s
                case IR_StringConstant(s @ "Blockdiagonal")         =>
                  matrixStructure_A = s
                  if (args.length != 5)
                    Logger.error("Blockdiagonal specified for A matrix in schur structure matrix but no blocksize given!")
                  args(4) match {
                    case IR_VariableAccess(_, d @ IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
                    case IR_IntegerConstant(c)                        => blocksize_A = c.toInt
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
      var structureInformation = args(0) match {
        case va @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)) =>
          var foundDecls = StateManager.findAll[IR_VariableDeclaration]().filter(d => d.name == name)
          foundDecls.length match {
            case 0 => Logger.error(s"could not localize declaration of: $va")
            case 1 =>
              var decl = foundDecls(0)

              decl.initialValue match {
                case None                                   => Logger.error("trying to classify not initialized matrix variable at compiletime!")
                case Some(x @ IR_MatrixExpression(_, _, _)) =>
                  //TODO what if there are assignments between declaration and inverse call -> not compiletime constant, inverse call executes nevertheless
                  // -> need to find all accesses to that matrix variable to make sure its compiletime constant -> there must not be any modifications to that matrix variable
                  //TODO collector check
                  //if (IR_MatrixNodeUtilities.notWrittenTo(name)) {
                  if(!IR_PreItMOps.writeCollector.writeInScope(name)) {
                    IR_DetermineMatrixStructure.isOfStructure(x)
                  }
                  else
                    Logger.error("found assignment to matrix input that was to classify, cannot classify non compiletime constant matrices!")
                case _                                      => Logger.error(s"unexpected initialization value: ${ decl.initialValue }, expected matrix expression!")
              }
            case _ => Logger.error(s"thought declarations should be unique, found multiple declarations with name: $name")
          }

        case x : IR_MatrixExpression =>
          IR_DetermineMatrixStructure.isOfStructure(x)
      }
      matrixStructure = structureInformation._1
      blocksize = structureInformation._2
      matrixStructure_A = structureInformation._3
      blocksize_A = structureInformation._4
    }
    Logger.warn(s"Inverting with the following configuration: ${ Knowledge.experimental_resolveInverseFunctionCall }, ${ (matrixStructure, blocksize, matrixStructure_A, blocksize_A) }")
    new IR_Inverse(args(0), (matrixStructure, blocksize, matrixStructure_A, blocksize_A), determineStructure, Knowledge.experimental_resolveInverseFunctionCall == "Runtime")
  }
}

case class IR_Inverse(
    arg : IR_Expression,
    structureInformation : (String, Int, String, Int),
    determineStructure : String,
    resolveAtRuntime : Boolean
) extends IR_ExtractableMNode {
  override def datatype : IR_Datatype = arg.datatype
  //override def prettyprint(out : PpStream) : Unit = Logger.error("internal node no resolved!")
  override def prettyprint(out : PpStream) : Unit = out << "inverseMM(" << arg << ")"
  override def isExtractable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}

object IR_GetSlice {
  def apply(args : ListBuffer[IR_Expression]) = {
    var evaluatable = true
    var args_asInts = ListBuffer[Int]()
    try {
      for(i <- 1 until 5)
        args_asInts += IR_SimplifyExpression.evalIntegral(args(i)).toInt
    } catch {
      case e : EvaluationException => evaluatable = false
      case t : Throwable           => Logger.error(s"unexpected exception: $t")
    }
    if (evaluatable)
      new IR_GetSlice(args, false, IR_MatrixDatatype(args(0).datatype.resolveBaseDatatype, args_asInts(2), args_asInts(3)))
    else {
      new IR_GetSlice(args, true)
    }
  }
}

case class IR_GetSlice(
    var arguments : ListBuffer[IR_Expression],
    resolveAtRuntime : Boolean,
    datatype : IR_Datatype = IR_UnknownDatatype
) extends IR_ExtractableMNode {
  // cant define datatype if length of slice is runtime dependent
  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def isExtractable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arguments(0)) && !datatype.equals(IR_UnknownDatatype)
}

object IR_Determinant {
  def apply(args : IR_Expression) = {
    var inMatrix = args match {
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_MatrixNodeUtilities.accessToExpression(va)
      case x @ IR_MatrixExpression(_, _, _)                      => x
      case _                                                     => Logger.error(s"unexpected argument ${ args }, expected matrix expression or variable")
    }
    if (inMatrix.columns > 5)
      new IR_Determinant(inMatrix, true)
    else
      new IR_Determinant(inMatrix, false)
  }
}

case class IR_Determinant(
    var arg : IR_Expression,
    resolveAtRuntime : Boolean = false
) extends IR_ExtractableMNode {
  override def datatype = arg.datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def isExtractable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}

object IR_InverseCT {
  def apply(inv : IR_Inverse) = {
    new IR_InverseCT(inv.arg, inv.structureInformation)
  }
  def apply(arg : IR_Expression, structureInformation : (String, Int, String, Int)) = {
    new IR_InverseCT(arg, structureInformation)
  }
}

case class IR_InverseCT(
    arg : IR_Expression,
    structureInformation : (String, Int, String, Int) = ("Filled", -1, "-1", -1)
) extends IR_MExpressionFunction {
  override def datatype = arg.datatype
  //  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def prettyprint(out : PpStream) : Unit = out << "inverseCT(" << arg << ")"
  override def resolve() : IR_Expression = {
    var argexpr = arg match {
      case x : IR_MatrixExpression                               => x
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_MatrixNodeUtilities.accessToExpression(va)
      case _                                                     => Logger.error(s"argument of unexpected type: ${ arg }")
    }
    IR_CompiletimeInversion.inverse(argexpr, structureInformation)
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}

object IR_InverseRT {
  def apply(dest : IR_VariableAccess, inv : IR_Inverse) = {
    new IR_InverseRT(dest, inv.arg, inv.structureInformation, inv.determineStructure == "DetermineRT")
  }
}

case class IR_InverseRT(
    dest : IR_VariableAccess,
    arg : IR_Expression,
    structureInformation : (String, Int, String, Int),
    determineStructureAtRuntime : Boolean
) extends IR_Expression with IR_MStatementFunction {
  override def datatype = arg.datatype
  //  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def prettyprint(out : PpStream) = out << "inverseRT(" << arg << ")"
  override def resolve() : IR_Scope = {
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
      newstmts += IR_GenerateRuntimeInversion.inverseBranchAtRuntime(inMatrix, dest.name, dest)
    }
    else {
      newstmts += IR_GenerateRuntimeInversion.inverse(inMatrix, dest, structureInformation)
    }
    IR_Scope(newstmts)
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}

object IR_GetSliceCT {
  def apply(inMatrix : IR_Expression, args : ListBuffer[Int]) = {
    new IR_GetSliceCT(inMatrix, args)
  }
  def apply(gs : IR_GetSlice) = {
    var params = ListBuffer[Int]()
    for (i <- 1 until 5) params += IR_SimplifyExpression.evalIntegral(gs.arguments(i)).toInt
    new IR_GetSliceCT(gs.arguments(0), params)
  }
}

case class IR_GetSliceCT(
    inMatrix : IR_Expression,
    params : ListBuffer[Int]
) extends IR_Expression with IR_MExpressionFunction {
  override def datatype = IR_MatrixDatatype(inMatrix.datatype.resolveBaseDatatype, params(2), params(3))
  override def prettyprint(out : PpStream) = out << "getSliceCT(" << inMatrix << "," << params << ")"
  override def resolve() : IR_Expression = {
    IR_BasicMatrixOperations.copySubMatrix(inMatrix, params(0), params(1), params(2), params(3))
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(inMatrix)
}

object IR_GetSliceRT {
  def apply(dest : IR_VariableAccess, args : IR_Expression*) = new IR_GetSliceRT(dest, args.to[ListBuffer])
}

case class IR_GetSliceRT(dest : IR_VariableAccess, var args : ListBuffer[IR_Expression]) extends IR_Expression with IR_MStatementFunction {
  // cant define datatype if length of slice is runtime dependent
  override def datatype = IR_UnknownDatatype
  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def resolve() : IR_Scope = {
    IR_GenerateBasicMatrixOperations.loopCopySubmatrix(args(0), dest, args(1), args(2), args(3), args(4))
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(args(0))
}

object IR_DeterminantCT {
  def apply(det : IR_Determinant) = new IR_DeterminantCT(det.arg)
}

case class IR_DeterminantCT(arg : IR_Expression) extends IR_Expression with IR_MExpressionFunction {
  override def datatype = arg.datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def resolve() : IR_Expression = {
    IR_BasicMatrixOperations.smallMatrixDeterminant(arg.asInstanceOf[IR_MatrixExpression])
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}

object IR_DeterminantRT {
  def apply(dest : IR_VariableAccess, arg : IR_Expression) = new IR_DeterminantRT(dest, arg)
}

case class IR_DeterminantRT(dest : IR_VariableAccess, var arg : IR_Expression) extends IR_Expression with IR_MStatementFunction {
  override def datatype = arg.datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def resolve() : IR_Scope = {
    arg match {
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        IR_GenerateBasicMatrixOperations.determinant(va, dest)
      case x @ IR_MatrixExpression(_, _, _)                      =>
        var stmts = IR_Scope(Nil)
        var tmp_access = IR_VariableAccess("detTmp_" + IR_MatrixExpression.matTmpCounter, x.datatype)
        stmts.body += IR_VariableDeclaration(tmp_access, x)
        stmts.body += IR_GenerateBasicMatrixOperations.determinant(tmp_access, dest)
        stmts
      case _                                                     => Logger.error(s"argument type not supported: ${ arg }, expected matrix expression or variable")
    }
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}

object IR_Transpose {
  def apply(args : IR_Expression) = new IR_Transpose(args)
}

case class IR_Transpose(var arg : IR_Expression)
  extends IR_ExtractableMNode with IR_MExpressionFunction {
  def name = "transpose"
  override def datatype = arg.datatype
  override def prettyprint(out : PpStream) = out << "transpose" << '(' << arg << ')'
  override def resolve() : IR_Expression = {
    IR_BasicMatrixOperations.transpose(arg.asInstanceOf[IR_VariableAccess])
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
  override def isExtractable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}

object IR_DotProduct {
  def apply(args : IR_Expression*) = new IR_DotProduct(args.to[ListBuffer])
}

case class IR_DotProduct(
    var arguments : ListBuffer[IR_Expression]
) extends IR_ExtractableMNode with IR_MExpressionFunction {
  def name = "dotProduct"
  override def datatype = arguments(0).datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = out << "DotProduct" << '(' <<< (arguments, ", ") << ')'
  override def resolve() : IR_Expression = {
    IR_BasicMatrixOperations.dotProduct(arguments(0), arguments(1))
  }
  override def isResolvable() : Boolean = !this.hasAnnotation(IR_ResolveMOps.potentialInline) && arguments.forall(arg => IR_MatrixNodeUtilities.isEvaluatable(arg))
  override def isExtractable() : Boolean = arguments.forall(arg => IR_MatrixNodeUtilities.isEvaluatable(arg))
}

object IR_CrossProduct {
  def apply(args : IR_Expression*) = new IR_CrossProduct(args.to[ListBuffer])
}

case class IR_CrossProduct(
    var arguments : ListBuffer[IR_Expression]
) extends IR_ExtractableMNode with IR_MExpressionFunction {
  def name = "crossProduct"
  override def datatype = IR_MatrixDatatype(arguments(0).datatype.resolveBaseDatatype, 3, 3)
  override def prettyprint(out : PpStream) = out << "CrossProduct" << '(' <<< (arguments, ", ") << ')'
  override def resolve() : IR_Expression = {
    IR_BasicMatrixOperations.crossProduct(arguments(0), arguments(1))
  }
  override def isResolvable() : Boolean = arguments.forall(arg => IR_MatrixNodeUtilities.isEvaluatable(arg))
  override def isExtractable() : Boolean = arguments.forall(arg => IR_MatrixNodeUtilities.isEvaluatable(arg))
}

object IR_Trace {
  def apply(args : IR_Expression) = new IR_Trace(args)
}

case class IR_Trace(
    var arg : IR_Expression
) extends IR_ExtractableMNode with IR_MExpressionFunction {
  def name = "trace"
  override def datatype = arg.datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = out << "Trace" << '(' << arg << ')'
  override def resolve() : IR_Expression = {
    IR_BasicMatrixOperations.trace(arg)
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
  override def isExtractable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}

object IR_SetElement {
  def apply(args : IR_Expression*) = new IR_SetElement(args.to[ListBuffer])
}

case class IR_SetElement(
    var arguments : ListBuffer[IR_Expression]
) extends IR_ExtractableMNode with IR_MStatementFunction {
  def name = "setElement"
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) = out << "SetElement" << '(' <<< (arguments, ", ") << ')'
  override def resolve() : IR_Scope = {
    IR_Scope(IR_Assignment(IR_HighDimAccess(arguments(0), IR_ExpressionIndex(arguments(1), arguments(2))), arguments(3)))
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arguments(0))
  override def isExtractable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arguments(0))
}

object IR_GetElement {
  def apply(args : IR_Expression*) = new IR_GetElement(args.to[ListBuffer])
}

case class IR_GetElement(
    var arguments : ListBuffer[IR_Expression]
) extends IR_ExtractableMNode with IR_MExpressionFunction {
  def name = "getElement"
  override def datatype = arguments(0).datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = out << "getElement" << '(' <<< (arguments, ", ") << ')'
  override def resolve() : IR_Expression = {
    IR_HighDimAccess(arguments(0), IR_ExpressionIndex(arguments(1), arguments(2)))
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arguments(0))
  override def isExtractable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arguments(0))
}

object IR_SetSlice {
  def apply(args : IR_Expression*) = new IR_SetSlice(args.to[ListBuffer])
}

case class IR_SetSlice(
    var arguments : ListBuffer[IR_Expression]
) extends IR_ExtractableMNode with IR_MStatementFunction {
  def name = "setSlice"
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) = out << "setSlice" << '(' <<< (arguments, ", ") << ')'
  override def resolve() : IR_Scope = {
    var matrix = arguments(0)
    var offsetRows = arguments(1)
    var offsetCols = arguments(2)
    var nRows = arguments(3)
    var nCols = arguments(4)
    var newValue = arguments(5)
    if (IR_MatrixNodeUtilities.isScalar(newValue))
      IR_Scope(IR_GenerateBasicMatrixOperations.loopSetSubmatrixSc(matrix.asInstanceOf[IR_VariableAccess], offsetRows, offsetCols, nRows, nCols, newValue))
    else {
      var insize = IR_BasicMatrixOperations.getSize(newValue)
      newValue match {
        case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
          IR_Scope(IR_GenerateBasicMatrixOperations.loopSetSubmatrixMat(va, matrix.asInstanceOf[IR_VariableAccess], IR_IntegerConstant(insize._1), IR_IntegerConstant(insize._2), offsetRows, offsetCols))
        case x @ IR_MatrixExpression(_, _, _)                      =>
          var decl = IR_MatrixNodeUtilities.expressionToDeclaration(x)
          IR_Scope(decl, IR_GenerateBasicMatrixOperations.loopSetSubmatrixMat(IR_VariableAccess(decl), matrix.asInstanceOf[IR_VariableAccess], IR_IntegerConstant(insize._1), IR_IntegerConstant(insize._2), offsetRows, offsetCols))
        case _                                                     => Logger.error(s"form of newValue matrix not supported: ${ newValue }, expected variable access to matrix variable")
      }
    }
  }
  override def isResolvable() : Boolean = arguments.forall(arg => IR_MatrixNodeUtilities.isEvaluatable(arg))
  override def isExtractable() : Boolean = arguments.forall(arg => IR_MatrixNodeUtilities.isEvaluatable(arg))
}

/*
object IR_MatMult {
  def apply(mult : IR_Multiplication) = {
    new IR_MatMult(mult)
  }
}
case class IR_MatMult(mult : IR_Multiplication) extends IR_MExpressionFunction {
  override def resolve() : IR_Expression = IR_BasicMatrixOperations.mult(mult)
  override def isResolvable() : Boolean =  mult.factors.forall(f => IR_MatrixNodeUtilities.isEvaluatable(f))
  override def datatype : IR_Datatype = mult.datatype
  override def prettyprint(out : PpStream) : Unit = Logger.error("internal node not resolved")
}
*/