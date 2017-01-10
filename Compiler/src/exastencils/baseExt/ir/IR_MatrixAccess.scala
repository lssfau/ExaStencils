package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config._
import exastencils.datastructures.{ DefaultStrategy, Transformation }
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

/// IR_HackMatComponentAccess

// FIXME: update with actual accessors
case class IR_HackMatComponentAccess(var mat: IR_VariableAccess, var i: IR_Expression, var j: IR_Expression) extends IR_Expression {
  override def datatype = mat.datatype
  override def prettyprint(out: PpStream): Unit = out << mat << "(" << i << ", " << j << ")"
}

/// IR_MatrixExpression
object IR_MatrixExpression {
  //def apply(innerDatatype : Option[IR_Datatype], rows : Integer, columns : Integer) : IR_MatrixExpression = new IR_MatrixExpression(innerDatatype, rows, columns)
  def apply(innerDatatype : IR_Datatype, rows : Integer, columns : Integer) : IR_MatrixExpression = new IR_MatrixExpression(Some(innerDatatype), rows, columns)
  def apply(innerDatatype: Option[IR_Datatype], rows: Integer, columns: Integer, expressions: Array[IR_Expression]): IR_MatrixExpression = {
    val tmp = new IR_MatrixExpression(innerDatatype, rows, columns)
    tmp.expressions = expressions
    tmp
  }
  def apply(innerDatatype: Option[IR_Datatype], expressions: ListBuffer[ListBuffer[IR_Expression]]): IR_MatrixExpression = {
    val rows = expressions.size
    val columns = expressions(0).size
    val tmp = new IR_MatrixExpression(innerDatatype, rows, columns)
    for (row <- 0 until rows) {
      for (col <- 0 until columns) {
        tmp.set(row, col, expressions(row)(col))
      }
    }
    tmp
  }
}

// FIXME: to be replaced/ updated
case class IR_MatrixExpression(var innerDatatype: Option[IR_Datatype], var rows: Integer, var columns: Integer) extends IR_Expression {
  var expressions: Array[IR_Expression] = Array.ofDim[IR_Expression](rows * columns)

  override def datatype = {
    if (innerDatatype.isEmpty) {
      var ret = expressions(0).datatype
      expressions.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
      innerDatatype = Some(ret)
    }
    IR_MatrixDatatype(innerDatatype.getOrElse(IR_RealDatatype), this.rows, this.columns)
  }

  def prettyprintInner(out: PpStream): Unit = {
    out << (if (Platform.targetCompiler == "GCC") "std::move((" else "((")
    innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
    out << "[]){" << (expressions.map(_.prettyprint).mkString(",")) << "})"
  }
  override def prettyprint(out: PpStream): Unit = {
    val prec = if (Knowledge.useDblPrecision) "double" else "float"

    out << "Matrix<" << (if (isInteger) "int" else prec) << ", " << rows << ", " << columns << "> ("
    prettyprintInner(out)
    out << ")"
  }

  def isConstant = expressions.forall(e => e.isInstanceOf[IR_Number])
  def isInteger = expressions.forall(e => e.isInstanceOf[IR_IntegerConstant])
  def isReal = expressions.forall(e => e.isInstanceOf[IR_RealConstant])
  def get(row: Integer, column: Integer) = expressions(row * columns + column)
  def set(row: Integer, column: Integer, exp: IR_Expression) = expressions(row * columns + column) = exp
  override def toString: String = {"IR_MatrixExpression(" + innerDatatype + ", " + rows + ", " + columns + "); Items: " + expressions.mkString(", ")}
}

case object IR_ResolveMatrices extends DefaultStrategy("Resolve matrices into scalars") {
  this += new Transformation("declarations 1/2", {
    case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, Some(func : IR_FunctionCall)) => {
      var newDecls = ListBuffer[IR_Statement]()
      // split declaration and definition so each part can be handled by subsequent transformations
      newDecls += IR_VariableDeclaration(matrix, decl.name, None)
      newDecls += IR_Assignment(IR_VariableAccess(decl.name, matrix), func)
      newDecls
    }
  })
  this += new Transformation("declarations 2/2", { // This code might look duplicated, but should be faster for different types of initial values
    case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, None) => {
      var newDecls = ListBuffer[IR_VariableDeclaration]()
      for(row <- 0 until matrix.sizeM) {
        for(col <- 0 until matrix.sizeN) {
          newDecls += new IR_VariableDeclaration(matrix.datatype, "_matrix_" + decl.name + "_" + row + "_" + col, None)
        }
      }
      newDecls
    }
    case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, Some(x : IR_MatrixExpression)) => {
      var newDecls = ListBuffer[IR_VariableDeclaration]()
      for(row <- 0 until matrix.sizeM) {
        for(col <- 0 until matrix.sizeN) {
          var value : Option[IR_Expression] = None
          if(decl.initialValue.isDefined) {
            value = Some(x.get(row, col))
          }
          newDecls += new IR_VariableDeclaration(matrix.datatype, "_matrix_" + decl.name + "_" + row + "_" + col, value)
        }
      }
      newDecls
    }
  })

  this += new Transformation("function argument types", {
    case func :  IR_Function => { // Resolve matrix types for function declarations
      func.parameters = func.parameters.flatMap(param => {
        param.datatype match {
          case matrix : IR_MatrixDatatype => {
            var resolvedParams = ListBuffer[IR_FunctionArgument]()
            for(row <- 0 until matrix.sizeM) {
              for(col <- 0 until matrix.sizeN) {
                resolvedParams += IR_FunctionArgument("_matrix_" + param.name + "_" + row + "_" + col, matrix.datatype)
              }
            }
            resolvedParams
          }
          case _ => List(param)
        }
      })
      func
    }
    case expstmt @ IR_ExpressionStatement(func : IR_FunctionCall) => { // Resolve matrix types for standalone function calls
      if(func.function.datatype.isInstanceOf[IR_MatrixDatatype]) { // change function access type
        func.function = IR_UserFunctionAccess(func.function.name, func.function.datatype.asInstanceOf[IR_MatrixDatatype].datatype)
      }
      func.arguments = func.arguments.flatMap(param => {
        var resolvedParams = ListBuffer[IR_Expression]()
        param match {
          case matrix : IR_MatrixExpression => {
            for(row <- 0 until matrix.rows) {
              for(col <- 0 until matrix.columns) {
                resolvedParams += matrix.get(row, col)
              }
            }
          }
          case access : IR_VariableAccess if(access.datatype.isInstanceOf[IR_MatrixDatatype]) => {
            val matrix = access.datatype.asInstanceOf[IR_MatrixDatatype]
            for (row <- 0 until matrix.sizeM) {
              for (col <- 0 until matrix.sizeN) {
                resolvedParams += IR_VariableAccess("_matrix_" + access.name + "_" + row + "_" + col, matrix.datatype)
              }
            }
          }
          // No other IR_Expression with Datatype == IR_MatrixDatatype should exist here
          case _ => resolvedParams += param
        }
        resolvedParams
      })
      expstmt
    }
  })

  this += new Transformation("return types", {
    case func : IR_Function if(func.returntype.isInstanceOf[IR_MatrixDatatype]) => {
      var matrix = func.returntype.asInstanceOf[IR_MatrixDatatype]
      for(row <- 0 until matrix.sizeM) {
        for(col <- 0 until matrix.sizeN) {
          var arg = IR_FunctionArgument("_matrix_return_" + row + "_" + col, IR_ReferenceDatatype(matrix.datatype))
          func.parameters += arg
        }
      }
      func.returntype = IR_UnitDatatype
      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp : IR_MatrixExpression)) => {
          if(exp.rows != matrix.sizeM || exp.columns != matrix.sizeN) {
            Logger.error("Matrix dimension mismatch in function " + func.name)
          }
          var stmts = ListBuffer[IR_Statement]()
          for(row <- 0 until matrix.sizeM) {
            for (col <- 0 until matrix.sizeM) {
              stmts += IR_Assignment(IR_VariableAccess("_matrix_return_" + row + "_" + col, exp.innerDatatype.getOrElse(matrix.datatype)), exp.get(row, col))
            }
          }
          stmts
        }
        case IR_Return(Some(access : IR_VariableAccess)) if(access.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          var stmts = ListBuffer[IR_Statement]()
          for(row <- 0 until matrix.sizeM) {
            for (col <- 0 until matrix.sizeM) {
              stmts += IR_Assignment(IR_VariableAccess("_matrix_return_" + row + "_" + col, matrix.datatype), IR_VariableAccess("_matrix_" + access.name + "_" + row + "_" + col, matrix.datatype))
            }
          }
          stmts
        }
        case _ => List(stmt)
      })
      func
    }
  })

  this += new Transformation("assignments", {
    case IR_Assignment(dst : IR_VariableAccess, src : IR_VariableAccess, _) if(dst.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      var dstdt = dst.datatype.asInstanceOf[IR_MatrixDatatype]
      var srcdt = src.datatype.asInstanceOf[IR_MatrixDatatype]
      if(dstdt.sizeM != srcdt.sizeM || dstdt.sizeN != srcdt.sizeN) {
        Logger.error("Matrix dimension mismatch in assignment")
      }
      var newStmts = ListBuffer[IR_Assignment]()
      for(row <- 0 until dstdt.sizeM) {
        for(col <- 0 until dstdt.sizeN) {
          newStmts += IR_Assignment(IR_VariableAccess("_matrix_" + dst.name + "_" + row + "_" + col, dstdt.datatype), IR_VariableAccess("_matrix_" + src.name + "_" + row + "_" + col, srcdt.datatype))
        }
      }
      newStmts
    }
    case IR_Assignment(dst : IR_VariableAccess, func : IR_FunctionCall, _) if(dst.datatype.isInstanceOf[IR_MatrixDatatype] && func.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      var dstdt = dst.datatype.asInstanceOf[IR_MatrixDatatype]
      var funcdt = func.datatype.asInstanceOf[IR_MatrixDatatype]
      if(dstdt.sizeM != funcdt.sizeM || dstdt.sizeN != funcdt.sizeN) {
        Logger.error("Matrix dimension mismatch in assignment")
      }

      func.function = IR_UserFunctionAccess(func.function.name, funcdt.datatype)
      func.arguments = func.arguments.flatMap(param => {
        var resolvedParams = ListBuffer[IR_Expression]()
        param match {
          case matrix : IR_MatrixExpression => {
            for(row <- 0 until matrix.rows) {
              for(col <- 0 until matrix.columns) {
                resolvedParams += matrix.get(row, col)
              }
            }
          }
          case access : IR_VariableAccess if(access.datatype.isInstanceOf[IR_MatrixDatatype]) => {
            val matrix = access.datatype.asInstanceOf[IR_MatrixDatatype]
            for (row <- 0 until matrix.sizeM) {
              for (col <- 0 until matrix.sizeN) {
                resolvedParams += IR_VariableAccess("_matrix_" + access.name + "_" + row + "_" + col, matrix.datatype)
              }
            }
          }
          // Leave other arguments alone
          case _ => resolvedParams += param
        }
        resolvedParams
      })

      // add return variable to function call
      for(row <- 0 until dstdt.sizeM) {
        for(col <- 0 until dstdt.sizeN) {
          var arg = IR_VariableAccess("_matrix_" + dst.name + "_" + row + "_" + col, dstdt.datatype)
          func.arguments += arg
        }
      }

      // FIXME this will result in invalid code if Function returns something, but is called without assignment!
      // e.g.:  Function foo(bar : Matrix<Real,2,2>) : Matrix<Real,2,2> { ... }
      //        Function Application() { foo(...); }

      IR_ExpressionStatement(func)
    }
  })


}