package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_Scope
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_UnitDatatype
import exastencils.base.ir.IR_UnknownDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ResolveMatrixOperations.isScalar
import exastencils.core.Duplicate
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

trait IR_MatrixExpressionFunction {
  def execute() : IR_Expression
}

trait IR_MatrixStatementFunction {
  def execute() : IR_Scope
}

object IR_Inverse {
  def apply(args : IR_Expression) = new IR_Inverse(args)
}

case class IR_Inverse(var arg : IR_Expression, var structureInformation : (String, Int, String, Int) = ("Filled", -1, "-1", -1), determineStructure : String = "no") extends IR_Expression {
  def name = "inverse"
  override def datatype = arg.datatype
  override def prettyprint(out : PpStream) = out << "inverse" << '(' << (arg, ", ") << ')'
}

object IR_InverseRT {
  def apply(dest : IR_VariableAccess, arg : IR_Expression) = new IR_InverseRT(dest, arg)
}

case class IR_InverseRT(dest : IR_VariableAccess, var arg : IR_Expression, var structureInformation : (String, Int, String, Int) = ("Filled", -1, "-1", -1), determineStructureAtRuntime : Boolean = false) extends IR_Expression with IR_MatrixStatementFunction {
  def name = "inverse"
  override def datatype = arg.datatype
  override def prettyprint(out : PpStream) = out << "inverse" << '(' << (arg, ", ") << ')'
  override def execute() : IR_Scope = {
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
      IR_GenerateRuntimeInversion.inverseBranchAtRuntime(inMatrix, name, dest)
    }
    else {
      IR_GenerateRuntimeInversion.inverse(inMatrix, dest, structureInformation)
    }
  }
}

object IR_GetSlice {
  def apply(args : IR_Expression*) = new IR_GetSlice(args.to[ListBuffer])
}

case class IR_GetSlice(var arguments : ListBuffer[IR_Expression]) extends IR_Expression {
  def name = "getSlice"
  // cant define datatype if length of slice is runtime dependent
  override def datatype = IR_UnknownDatatype
  override def prettyprint(out : PpStream) = out << "GetSlice" << '(' <<< (arguments, ", ") << ')'
}

object IR_GetSliceRT {
  def apply(dest : IR_VariableAccess, args : IR_Expression*) = new IR_GetSliceRT(dest, args.to[ListBuffer])
}

case class IR_GetSliceRT(dest : IR_VariableAccess, var args : ListBuffer[IR_Expression]) extends IR_Expression with IR_MatrixStatementFunction {
  def name = "getSlice"
  // cant define datatype if length of slice is runtime dependent
  override def datatype = IR_UnknownDatatype
  override def prettyprint(out : PpStream) = out << "GetSlice" << '(' <<< (args, ", ") << ')'
  override def execute() : IR_Scope = {
    IR_GenerateBasicMatrixOperations.loopCopySubmatrix(args(0), dest, args(1), args(2), args(3), args(4))
  }
}

object IR_Determinant {
  def apply(args : IR_Expression) = new IR_Determinant(args)
}

case class IR_Determinant(var arg : IR_Expression, resolveAtRuntime : Boolean = false) extends IR_Expression {
  def name = "determinant"
  override def datatype = arg.datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = out << "determinant" << '(' << arg << ')'
}

object IR_DeterminantRT {
  def apply(dest : IR_VariableAccess, arg : IR_Expression) = new IR_DeterminantRT(dest, arg)
}

case class IR_DeterminantRT(dest : IR_VariableAccess, var arg : IR_Expression) extends IR_Expression with IR_MatrixStatementFunction {
  def name = "determinant"
  override def datatype = arg.datatype
  override def prettyprint(out : PpStream) = out << "determinant" << '(' << (arg, ", ") << ')'
  override def execute() : IR_Scope = {
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
}

object IR_Transpose {
  def apply(args : IR_Expression) = new IR_Transpose(args)
}

case class IR_Transpose(var arg : IR_Expression) extends IR_Expression with IR_MatrixExpressionFunction {
  def name = "transpose"
  override def datatype = arg.datatype
  override def prettyprint(out : PpStream) = out << "transpose" << '(' << arg << ')'
  override def execute() : IR_Expression = {
    IR_BasicMatrixOperations.transpose(arg.asInstanceOf[IR_VariableAccess])
  }
}

object IR_DotProduct {
  def apply(args : IR_Expression*) = new IR_DotProduct(args.to[ListBuffer])
}

case class IR_DotProduct(var arguments : ListBuffer[IR_Expression]) extends IR_Expression with IR_MatrixExpressionFunction {
  def name = "dotProduct"
  override def datatype = arguments(0).datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = out << "DotProduct" << '(' <<< (arguments, ", ") << ')'
  override def execute() : IR_Expression = {
    IR_BasicMatrixOperations.dotProduct(arguments(0), arguments(1))
  }
}

object IR_CrossProduct {
  def apply(args : IR_Expression*) = new IR_CrossProduct(args.to[ListBuffer])
}

case class IR_CrossProduct(var arguments : ListBuffer[IR_Expression]) extends IR_Expression with IR_MatrixExpressionFunction {
  def name = "crossProduct"
  override def datatype = IR_MatrixDatatype(arguments(0).datatype.resolveBaseDatatype, 3, 3)
  override def prettyprint(out : PpStream) = out << "CrossProduct" << '(' <<< (arguments, ", ") << ')'
  override def execute() : IR_Expression = {
    IR_BasicMatrixOperations.crossProduct(arguments(0), arguments(1))
  }
}

object IR_Trace {
  def apply(args : IR_Expression) = new IR_Trace(args)
}

case class IR_Trace(var arg : IR_Expression) extends IR_Expression with IR_MatrixExpressionFunction {
  def name = "trace"
  override def datatype = arg.datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = out << "Trace" << '(' << arg << ')'
  override def execute() : IR_Expression = {
    IR_BasicMatrixOperations.trace(arg)
  }
}

object IR_SetElement {
  def apply(args : IR_Expression*) = new IR_SetElement(args.to[ListBuffer])
}

case class IR_SetElement(var arguments : ListBuffer[IR_Expression]) extends IR_Expression with IR_MatrixStatementFunction {
  def name = "setElement"
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) = out << "SetElement" << '(' <<< (arguments, ", ") << ')'
  override def execute() : IR_Scope = {
    IR_Scope(IR_Assignment(IR_HighDimAccess(arguments(0), IR_ExpressionIndex(arguments(1), arguments(2))), arguments(3)))
  }
}

object IR_GetElement {
  def apply(args : IR_Expression*) = new IR_GetElement(args.to[ListBuffer])
}

case class IR_GetElement(var arguments : ListBuffer[IR_Expression]) extends IR_Expression with IR_MatrixExpressionFunction {
  def name = "getElement"
  override def datatype = arguments(0).datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = out << "getElement" << '(' <<< (arguments, ", ") << ')'
  override def execute() : IR_Expression = {
    IR_HighDimAccess(arguments(0), IR_ExpressionIndex(arguments(1), arguments(2)))
  }
}

object IR_SetSlice {
  def apply(args : IR_Expression*) = new IR_SetSlice(args.to[ListBuffer])
}

case class IR_SetSlice(var arguments : ListBuffer[IR_Expression]) extends IR_Expression with IR_MatrixStatementFunction {
  def name = "setSlice"
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) = out << "setSlice" << '(' <<< (arguments, ", ") << ')'
  override def execute() : IR_Scope = {
    var matrix = arguments(0)
    var offsetRows = arguments(1)
    var offsetCols = arguments(2)
    var nRows = arguments(3)
    var nCols = arguments(4)
    var newValue = arguments(5)
    if (isScalar(newValue))
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
}