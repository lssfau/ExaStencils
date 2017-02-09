package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.core.StateManager
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_MultiDimFieldAccess
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

//  implicit def +(a : IR_MatrixExpression, b : IR_MatrixExpression) = {
//    val ret = new IR_MatrixExpression(Some(IR_ResultingDatatype(a.innerDatatype.get, b.innerDatatype.get)), a.rows, a.columns)
//    var xx = a.expressions.zip(b.expressions).map(x => x._1 + x._2).asInstanceOf[Array[IR_Expression]]
//    ret.expressions = xx
//    ret
//  }
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

object IR_ResolveMatrices extends DefaultStrategy("Resolve matrices into scalars") {
  val annotationFctCallCounter = "IR_ResolveMatrices.fctCallCounter"
  var fctCallCounter = 0 // temporary variable used to replace function calls in expressions
  val annotationMatExpCounter = "IR_ResolveMatrices.matrixExpressionCounter"
  var matExpCounter = 0 // temporary variable used to replace matrix expressions in expressions
  val annotationMatrixRow = "IR_ResolveMatrices.matrixRow"
  val annotationMatrixCol = "IR_ResolveMatrices.matrixCol"

  this += new Transformation("declarations", {
    case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, Some(exp : IR_Expression)) => {
      var newStmts = ListBuffer[IR_Statement]()
      // split declaration and definition so each part can be handled by subsequent transformations
      newStmts += IR_VariableDeclaration(matrix, decl.name, None)
      newStmts += IR_Assignment(IR_VariableAccess(decl), exp)
      newStmts
    }
  })

  this += new Transformation("modify assignments 1/3", {
    case stmt @ IR_Assignment(dest, src : IR_FunctionCall, _) if (dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      src.arguments += dest
      IR_ExpressionStatement(src)
    }
  })

  this += new Transformation("modify assignments 2/3", {
    case stmt @ IR_Assignment(dest, src, _) => {
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_FunctionCall](src).filter(_.datatype.isInstanceOf[IR_MatrixDatatype]).foreach(exp => {
        newStmts += IR_VariableDeclaration(exp.function.datatype, "_fct" + fctCallCounter + "_" + exp.function.name, Some(Duplicate(exp)))
        exp.annotate(annotationFctCallCounter, fctCallCounter)
        fctCallCounter += 1
      })
      StateManager.findAll[IR_MatrixExpression](src).foreach(exp => {
        var decl = IR_VariableDeclaration(exp.datatype, "_matrixExp" + matExpCounter, None)
        newStmts += decl
        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
        exp.annotate(annotationMatExpCounter, matExpCounter)
        matExpCounter += 1
      })
      newStmts += stmt
      newStmts
    }
    case stmt @ IR_ExpressionStatement(src : IR_FunctionCall) => {
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_MatrixExpression](src).foreach(exp => {
        var decl = IR_VariableDeclaration(exp.datatype, "_matrixExp" + matExpCounter, None)
        newStmts += decl
        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
        exp.annotate(annotationMatExpCounter, matExpCounter)
        matExpCounter += 1
      })
      newStmts += stmt
      newStmts
    }
  })

  this += new Transformation("modify assignments 3/3", {
    case exp : IR_FunctionCall if(exp.hasAnnotation(annotationFctCallCounter)) => {
      IR_VariableAccess("_fct" + exp.popAnnotation(annotationFctCallCounter).get.asInstanceOf[Int] + "_" + exp.function.name, exp.function.datatype.asInstanceOf[IR_MatrixDatatype].datatype)
    }
    case exp : IR_MatrixExpression if(exp.hasAnnotation(annotationMatExpCounter)) => {
      IR_VariableAccess("_matrixExp" + exp.popAnnotation(annotationMatExpCounter).get.asInstanceOf[Int], exp.datatype)
    }
  })

  this += new Transformation("return types", {
    case func : IR_Function if(func.returntype.isInstanceOf[IR_MatrixDatatype]) => {
      val matrix = func.returntype.asInstanceOf[IR_MatrixDatatype]
      func.parameters += IR_FunctionArgument("_matrix_return", IR_ReferenceDatatype(matrix))
      func.returntype = IR_UnitDatatype

      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp)) if(exp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          List(
            IR_Assignment(IR_VariableAccess("_matrix_return", matrix), exp),
            IR_Return())
        }
        case _ => List(stmt)
      })
      func
    }
  })

  this += new Transformation("expressions 1/2", {
    case stmt : IR_Assignment if(stmt.dest.datatype.isInstanceOf[IR_MatrixDatatype]
                                  && !stmt.hasAnnotation(annotationMatrixRow)
                                  && !stmt.dest.isInstanceOf[IR_HighDimAccess]) => {
      // annotate all nodes of this expression
      val matrix = stmt.dest.datatype.asInstanceOf[IR_MatrixDatatype]
      var newStmts = ListBuffer[IR_Statement]()
      for (row <- 0 until matrix.sizeM) {
        for (col <- 0 until matrix.sizeN) {
          var cloned = Duplicate(stmt)
          StateManager.findAll[IR_Expression](cloned).foreach(exp => exp match {
            case x : IR_FunctionArgument                                                                                                    => // do not mark function arguments to be resolved into indivual accesses
            case x @ (_  : IR_VariableAccess | _ : IR_MatrixExpression | _ : IR_FieldAccess | _ : IR_MultiDimFieldAccess) if(x.datatype.isInstanceOf[IR_MatrixDatatype]) => {
              x.annotate(annotationMatrixRow, row)
              x.annotate(annotationMatrixCol, col)
            }
            case _                                                                                                                          =>
          })
          newStmts += cloned
        }
      }
      newStmts
    }
  }, false)

  // FIXME correctly multiply IR_MatrixDatatype

  this += new Transformation("expressions 2/2", {
    case exp : IR_MatrixExpression if(exp.hasAnnotation(annotationMatrixRow)) => {
      exp.get(exp.popAnnotation(annotationMatrixRow).get.asInstanceOf[Int], exp.popAnnotation(annotationMatrixCol).get.asInstanceOf[Int])
    }
    case exp : IR_Expression if(exp.hasAnnotation(annotationMatrixRow)) => {
      IR_HighDimAccess(exp, IR_ConstIndex(Array(exp.popAnnotation(annotationMatrixRow).get.asInstanceOf[Int], exp.popAnnotation(annotationMatrixCol).get.asInstanceOf[Int])))
    }
  })

//  this += new Transformation("linearize HighDimAccesses", {
//    case access @ IR_HighDimAccess(base : IR_VariableAccess, idx : IR_ConstIndex) => {
//      var matrix = access.datatype.asInstanceOf[IR_MatrixDatatype]
//      var myidx = idx.toExpressionIndex
//      base + IR_IntegerConstant((matrix.sizeM - 1)) * myidx.indices(0) + myidx.indices(1)
//    }
//  })

}
//
//
//object IR_LinearizeMatrices extends DefaultStrategy("Linearize matrices") {
//    this += new Transformation("linearize HighDimAccesses", {
//      case access @ IR_HighDimAccess(base : IR_VariableAccess, idx : IR_ConstIndex) => {
//        var matrix = access.datatype.asInstanceOf[IR_MatrixDatatype]
//        var myidx = idx.toExpressionIndex
//        base + IR_IntegerConstant((matrix.sizeM - 1)) * myidx.indices(0) + myidx.indices(1)
//      }
//    })
//}