package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.core._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

/// IR_HackMatComponentAccess

// FIXME: update with actual accessors
case class IR_HackMatComponentAccess(var mat : IR_VariableAccess, var i : IR_Expression, var j : IR_Expression) extends IR_Expression {
  override def datatype = mat.datatype
  override def prettyprint(out : PpStream) : Unit = out << mat << "(" << i << ", " << j << ")"
}

/// IR_MatrixExpression
object IR_MatrixExpression {
  //def apply(innerDatatype : Option[IR_Datatype], rows : Integer, columns : Integer) : IR_MatrixExpression = new IR_MatrixExpression(innerDatatype, rows, columns)
  def apply(innerDatatype : IR_Datatype, rows : Integer, columns : Integer) : IR_MatrixExpression = new IR_MatrixExpression(Some(innerDatatype), rows, columns)
  def apply(innerDatatype : Option[IR_Datatype], rows : Integer, columns : Integer, expressions : Array[IR_Expression]) : IR_MatrixExpression = {
    val tmp = new IR_MatrixExpression(innerDatatype, rows, columns)
    tmp.expressions = expressions
    tmp
  }
  def apply(innerDatatype : Option[IR_Datatype], expressions : ListBuffer[ListBuffer[IR_Expression]]) : IR_MatrixExpression = {
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

case class IR_MatrixExpression(var innerDatatype : Option[IR_Datatype], var rows : Int, var columns : Int) extends IR_Expression {
  var expressions : Array[IR_Expression] = Array.ofDim[IR_Expression](rows * columns)

  override def datatype = {
    if (innerDatatype.isEmpty) {
      var ret = expressions(0).datatype
      expressions.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
      innerDatatype = Some(ret)
    }
    IR_MatrixDatatype(innerDatatype.getOrElse(IR_RealDatatype), this.rows, this.columns)
  }

  def prettyprintInner(out : PpStream) : Unit = {
    if (Knowledge.experimental_internalHighDimTypes) {
      out << "INVALID: IR_MatrixExpression"
    } else {
      out << (if (Platform.targetCompiler == "GCC") "std::move((" else "((")
      innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
      out << "[]){" << (expressions.map(_.prettyprint).mkString(",")) << "})"
    }
  }
  override def prettyprint(out : PpStream) : Unit = {
    if (Knowledge.experimental_internalHighDimTypes) {
      out << "INVALID: IR_MatrixExpression"
    } else {
      val prec = if (Knowledge.useDblPrecision) "double" else "float"
      out << "Matrix<" << (if (isInteger) "int" else prec) << ", " << rows << ", " << columns << "> ("
      prettyprintInner(out)
      out << ")"
    }
  }

  def isConstant = expressions.forall(e => e.isInstanceOf[IR_Number])
  def isInteger = expressions.forall(e => e.isInstanceOf[IR_IntegerConstant])
  def isReal = expressions.forall(e => e.isInstanceOf[IR_RealConstant])
  def get(row : Integer, column : Integer) = expressions(row * columns + column)
  def set(row : Integer, column : Integer, exp : IR_Expression) = expressions(row * columns + column) = exp
  override def toString : String = { "IR_MatrixExpression(" + innerDatatype + ", " + rows + ", " + columns + "; Items: " + expressions.mkString(", ") + ")" }
}

object IR_ResolveMatrices extends DefaultStrategy("Resolve matrices into scalars") {
  val annotationFctCallCounter = "IR_ResolveMatrices.fctCallCounter"
  var fctCallCounter = 0
  // temporary variable used to replace function calls in expressions
  val annotationMatExpCounter = "IR_ResolveMatrices.matrixExpressionCounter"
  var matExpCounter = 0
  // temporary variable used to replace matrix expressions in expressions
  val annotationMatrixRow = "IR_ResolveMatrices.matrixRow"
  val annotationMatrixCol = "IR_ResolveMatrices.matrixCol"
  val builtInFunctions = List("dotProduct", "dot", "inverse", "det")

  def calculateDeterminant(m : IR_MatrixExpression) : IR_Expression = {
    if (m.rows != m.columns) {
      Logger.error("determinant for non-quadratic matrices not implemented")
      // FIXME Nullzeilen/-spalten ergänzen
    }
    if (m.rows <= 0) {
      Logger.error("MatrixExpression of size <= 0")
    } else if (m.rows == 1) {
      return Duplicate(m.get(0, 0))
    } else if (m.rows == 2) {
      return Duplicate(m.get(0, 0) * m.get(1, 1) - m.get(0, 1) * m.get(1, 0))
    } else if (m.rows == 3) {
      return Duplicate(m.get(0, 0) * m.get(1, 1) * m.get(2, 2) +
        m.get(0, 1) * m.get(1, 2) * m.get(2, 0) +
        m.get(0, 2) * m.get(1, 0) * m.get(2, 1) -
        m.get(2, 0) * m.get(1, 1) * m.get(0, 2) -
        m.get(2, 1) * m.get(1, 2) * m.get(0, 0) -
        m.get(2, 2) * m.get(1, 0) * m.get(0, 1))
    } else {
      var det : IR_Expression = IR_IntegerConstant(0)
      val tmp = IR_MatrixExpression(Some(m.innerDatatype.getOrElse(IR_RealDatatype)), m.rows - 1, m.columns - 1)
      // laplace expansion
      for (i <- 0 until m.rows) {
        var tmpRow = 0
        for (row <- 0 until m.rows) {
          if (row != i) {
            for (col <- 1 until m.columns) {
              tmp.set(tmpRow, col - 1, Duplicate(m.get(row, col)))
            }
            tmpRow += 1
          }
        }
        det += m.get(i, 0) * calculateDeterminant(tmp) * IR_DoubleConstant(math.pow(-1, i))
      }
      return det
    }
  }

  def calculateMatrixOfMinorsElement(m : IR_MatrixExpression, forRow : Integer, forColumn : Integer) : IR_Expression = {
    if (m.rows != m.columns) {
      Logger.error("matrix of minors for non-quadratic matrices not implemented ")
    }
    var matrixExps = ListBuffer[ListBuffer[IR_Expression]]()
    var tmp = IR_MatrixExpression(Some(m.innerDatatype.getOrElse(IR_RealDatatype)), m.rows - 1, m.columns - 1)
    var tmpRow = 0
    for (row <- 0 until m.rows) {
      if (row != forRow) {
        var tmpCol = 0
        for (col <- 0 until m.columns) {
          if (col != forColumn) {
            tmp.set(tmpRow, tmpCol, m.get(row, col))
            tmpCol += 1
          }
        }
        tmpRow += 1
      }
    }
    return calculateDeterminant(tmp)
  }

  this += new Transformation("declarations", {
    // Definition of matrix variable including initialisation -> split into decl and assignment
    case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, Some(exp : IR_Expression)) => {
      var newStmts = ListBuffer[IR_Statement]()
      // split declaration and definition so each part can be handled by subsequent transformations
      newStmts += IR_VariableDeclaration(matrix, decl.name, None)
      newStmts += IR_Assignment(IR_VariableAccess(decl), exp)
      newStmts
    }
  })

  this += new Transformation("extract function calls 1/2", {
    case stmt @ IR_Assignment(dest, src, op) if (src.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      // Extract all function calls into separate variables since any function could have unwanted side effects if called more than once
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_FunctionCall](src).foreach(exp => {
        var decl = IR_VariableDeclaration(exp.datatype, "_fct" + fctCallCounter, None)
        newStmts += decl
        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
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
    case stmt @ IR_ExpressionStatement(src) if (src.datatype.isInstanceOf[IR_MatrixDatatype])  => {
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_FunctionCall](src).foreach(exp => {
        var decl = IR_VariableDeclaration(exp.datatype, "_fct" + fctCallCounter, None)
        newStmts += decl
        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
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
  })

  this += new Transformation("extract function calls 2/2", {
    case exp : IR_FunctionCall if (exp.hasAnnotation(annotationFctCallCounter))    => {
      IR_VariableAccess("_fct" + exp.popAnnotation(annotationFctCallCounter).get.asInstanceOf[Int] + "_" + exp.function.name, exp.function.datatype)
    }
    case exp : IR_MatrixExpression if (exp.hasAnnotation(annotationMatExpCounter)) => {
      IR_VariableAccess("_matrixExp" + exp.popAnnotation(annotationMatExpCounter).get.asInstanceOf[Int], exp.datatype)
    }
  })

  this += new Transformation("return types", {
    case func : IR_Function if (func.returntype.isInstanceOf[IR_MatrixDatatype]) => {
      val matrix = func.returntype.asInstanceOf[IR_MatrixDatatype]
      func.parameters += IR_FunctionArgument("_matrix_return", IR_ReferenceDatatype(matrix))
      func.returntype = IR_UnitDatatype

      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp)) if (exp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          List(
            IR_Assignment(IR_VariableAccess("_matrix_return", matrix), exp),
            IR_Return())
        }
        case _                                                                      => List(stmt)
      })
      func
    }
  })

  this += new Transformation("function call returns", {
    case stmt @ IR_Assignment(dest, src : IR_FunctionCall, "=") if (dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      // FIXME resolve IR_Assignments with operator += before this
      src.arguments += dest
      IR_ExpressionStatement(src)
    }
  })

  this += new Transformation("simplify function call arguments", {
    case stmt @ IR_ExpressionStatement(exp : IR_FunctionCall)                                               => {
      var newStmts = ListBuffer[IR_Statement]()

      exp.arguments = exp.arguments.map(arg => arg match {
        case argexp : IR_MultiDimFieldAccess                                             => argexp
        case argexp : IR_VariableAccess                                                  => argexp
        case argexp : IR_Expression if (argexp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          var decl = IR_VariableDeclaration(argexp.datatype, "_matrixExp" + matExpCounter, None)
          newStmts += decl
          newStmts += IR_Assignment(IR_VariableAccess(decl), argexp)
          IR_VariableAccess(decl)
        }
        case _                                                                           => arg
      })
      newStmts += stmt
      newStmts
    }
    case stmt @ IR_Assignment(_, exp : IR_FunctionCall, _) if !builtInFunctions.contains(exp.function.name) => {
      // FIXME try to move this into case statement above
      var newStmts = ListBuffer[IR_Statement]()

      exp.arguments = exp.arguments.map(arg => arg match {
        case argexp : IR_MultiDimFieldAccess                                             => argexp
        case argexp : IR_VariableAccess                                                  => argexp
        case argexp : IR_Expression if (argexp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          var decl = IR_VariableDeclaration(argexp.datatype, "_matrixExp" + matExpCounter, None)
          newStmts += decl
          newStmts += IR_Assignment(IR_VariableAccess(decl), argexp)
          IR_VariableAccess(decl)
        }
        case _                                                                           => arg
      })
      newStmts += stmt
      newStmts
    }
  })

//  this += new Transformation("resolution of built-in functions 1/2", {
//    case call : IR_FunctionCall if (builtInFunctions.contains(call.name)) => {
//      call.arguments = call.arguments.map(arg => arg match {
//        case _ : IR_VariableAccess | _ : IR_MultiDimFieldAccess => {
//          // FIXME: map fieldAccesses and the like to MatrixExpression in preparatory strategy?
//          val matrix = arg.datatype.asInstanceOf[IR_MatrixDatatype]
//          var exps = ListBuffer[IR_Expression]()
//          for (row <- 0 until matrix.sizeM) {
//            for (col <- 0 until matrix.sizeN) {
//              exps += IR_HighDimAccess(Duplicate(arg), IR_ConstIndex(row, col))
//            }
//          }
//          IR_MatrixExpression(Some(matrix.resolveBaseDatatype), matrix.sizeM, matrix.sizeN, exps.toArray)
//        }
//        case _                                                  => arg
//      })
//      call
//    }
//  })

  this += new Transformation("resolution of built-in functions 2/2", {
    case call : IR_FunctionCall if (call.name == "dotProduct" || call.name == "dot") => {
      if (call.arguments.length != 2) {
        Logger.error(s"dotProduct() must have two arguments; has ${ call.arguments.length }")
      }
      val left = call.arguments(0) match {
        case me : IR_MatrixExpression => me
        case other                    => Logger.error(s"Dot product argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }
      val right = call.arguments(1) match {
        case me : IR_MatrixExpression => me
        case other                    => Logger.error(s"Dot product argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }
      if (left.rows != right.rows || left.columns != right.columns) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for dotProduct() - attempting ${ left.rows }x${ left.columns } * ${ right.rows }x${ right.columns }")
      }
      var additions = ListBuffer[IR_Expression]()
      for (row <- 0 until left.rows) {
        for (col <- 0 until left.columns) {
          additions += IR_Multiplication(left.get(row, col), right.get(row, col))
        }
      }
      IR_Addition(additions)
    }
    case call : IR_FunctionCall if (call.name == "inverse")                          => {
      if (call.arguments.length != 1) {
        Logger.error("inverse() must have one argument")
      }
      val m = call.arguments(0).asInstanceOf[IR_MatrixExpression]
      val ret = m.rows match {
        case 1 => IR_MatrixExpression(m.innerDatatype, 1, 1, Array(IR_Division(IR_RealConstant(1.0), m.get(0, 0))))
        case 2 => {
          val a = m.get(0, 0)
          val b = m.get(0, 1)
          val c = m.get(1, 0)
          val d = m.get(1, 1)
          val det : IR_Expression = IR_Division(IR_RealConstant(1.0), (a * d) - (b * c))
          IR_MatrixExpression(m.innerDatatype, 2, 2, Array(Duplicate(det) * Duplicate(d), Duplicate(det) * Duplicate(b) * IR_IntegerConstant(-1), Duplicate(det) * Duplicate(c) * IR_IntegerConstant(-1), Duplicate(det) * Duplicate(a)))
        }
        case 3 => {
          val a = m.get(0, 0)
          val b = m.get(0, 1)
          val c = m.get(0, 2)
          val d = m.get(1, 0)
          val e = m.get(1, 1)
          val f = m.get(1, 2)
          val g = m.get(2, 0)
          val h = m.get(2, 1)
          val i = m.get(2, 2)
          val A = Duplicate(e) * Duplicate(i) - Duplicate(f) * Duplicate(h)
          val B = IR_IntegerConstant(-1) * (Duplicate(d) * Duplicate(i) - Duplicate(f) * Duplicate(g))
          val C = Duplicate(d) * Duplicate(h) - Duplicate(e) * Duplicate(g)
          val D = IR_IntegerConstant(-1) * (Duplicate(b) * Duplicate(i) - Duplicate(c) * Duplicate(h))
          val E = Duplicate(a) * Duplicate(i) - Duplicate(c) * Duplicate(g)
          val F = IR_IntegerConstant(-1) * (Duplicate(a) * Duplicate(h) - Duplicate(b) * Duplicate(g))
          val G = Duplicate(b) * Duplicate(f) - Duplicate(c) * Duplicate(e)
          val H = IR_IntegerConstant(-1) * (Duplicate(a) * Duplicate(f) - Duplicate(c) * Duplicate(d))
          val I = Duplicate(a) * Duplicate(e) - Duplicate(b) * Duplicate(d)
          val det = Duplicate(a) * A + Duplicate(b) * B + Duplicate(c) * C
          IR_MatrixExpression(m.innerDatatype, 3, 3, Array(Duplicate(A) / Duplicate(det), Duplicate(D) / Duplicate(det), Duplicate(G) / Duplicate(det), Duplicate(B) / Duplicate(det), Duplicate(E) / Duplicate(det), Duplicate(H) / Duplicate(det), Duplicate(C) / Duplicate(det), Duplicate(F) / Duplicate(det), Duplicate(I) / Duplicate(det)))
        }
        case _ => {
          val inv_det = IR_IntegerConstant(1) / calculateDeterminant(m)
          val tmp = IR_MatrixExpression(Some(m.innerDatatype.getOrElse(IR_RealDatatype)), m.rows, m.columns)
          for (row <- 0 until m.rows) {
            for (col <- 0 until m.columns) {
              tmp.set(col, row, calculateMatrixOfMinorsElement(m, row, col) * IR_DoubleConstant(math.pow(-1, row + col)) * inv_det)
            }
          }
          tmp
        }
      }
      ret
    }
    case call : IR_FunctionCall if (call.name == "det")                              => {
      if (call.arguments.length != 1) {
        Logger.error("det() must have one argument")
      }
      val m = call.arguments(0).asInstanceOf[IR_MatrixExpression]
      calculateDeterminant(m)
    }
  })

  //  this += new Transformation("put into matrix expressions", {
  //    case stmt @ IR_Assignment(dest, src, _) if(dest.datatype.isInstanceOf[IR_MatrixDatatype]) => {
  //      val matrix = dest.datatype.asInstanceOf[IR_MatrixDatatype]
  //      dest match {
  //        case v : IR_VariableAccess => {
  //          var dexp = ListBuffer[IR_Expression]()
  //          for (row <- 0 until matrix.sizeM) {
  //            for (col <- 0 until matrix.sizeN) {
  //              dexp += IR_HighDimAccess(v, IR_ConstIndex(row, col))
  //            }
  //          }
  //          stmt.dest = IR_MatrixExpression(Some(matrix.resolveBaseDatatype), matrix.sizeM, matrix.sizeN, dexp.toArray)
  //        }
  //        case _ => Logger.warn("Unknown type in 'put into matrix expressions': " + dest)
  //      }
  //  })

  this += new Transformation("scalarize 1/2", {
    case stmt @ IR_Assignment(dest, src, op) if (dest.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      val matrix = dest.datatype.asInstanceOf[IR_MatrixDatatype]
      var newStmts = ListBuffer[IR_Statement]()
      for (row <- 0 until matrix.sizeM) {
        for (col <- 0 until matrix.sizeN) {
          var cloned = Duplicate(stmt)
          StateManager.findAll[IR_Expression](cloned).foreach(exp => exp match {
            case x : IR_FunctionArgument                                                                                                            => // do not mark function arguments to be resolved into indivual accesses
            case x @ (_ : IR_VariableAccess | _ : IR_MatrixExpression | _ : IR_MultiDimFieldAccess) if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => {
              x.annotate(annotationMatrixRow, row)
              x.annotate(annotationMatrixCol, col)
            }
            case _                                                                                                                                  => Logger.info("Not annotated: " + exp)
          })
          newStmts += cloned
        }
      }
      newStmts
    }
  })

  this += new Transformation("expressions 2/3", {
    case exp : IR_MatrixExpression if (exp.hasAnnotation(annotationMatrixRow)) =>
      exp.get(exp.popAnnotation(annotationMatrixRow).get.asInstanceOf[Int], exp.popAnnotation(annotationMatrixCol).get.asInstanceOf[Int])

    case exp : IR_Expression if (exp.hasAnnotation(annotationMatrixRow)) =>
      IR_HighDimAccess(Duplicate(exp), IR_ConstIndex(Array(exp.popAnnotation(annotationMatrixRow).get.asInstanceOf[Int], exp.popAnnotation(annotationMatrixCol).get.asInstanceOf[Int])))
  }, false)

//  this += new Transformation("linearize into array accesses", {
//    case access @ IR_HighDimAccess(base : IR_VariableAccess, idx : IR_ConstIndex) => {
//      val matrix = base.datatype.asInstanceOf[IR_MatrixDatatype]
//      val myidx = idx.toExpressionIndex
//      IR_ArrayAccess(base, IR_IntegerConstant(matrix.sizeM) * myidx.indices(0) + myidx.indices(1))
//    }
//    case access @ IR_HighDimAccess(base : IR_MultiDimFieldAccess, idx : IR_ConstIndex) => {
//    // FIXME
//    }
//  })
//  this += Transformation("linearize HighDimAccesses", {
//    case access @ IR_HighDimAccess(base, idx : IR_ConstIndex) if (idx.indices.length == 2) => {
//      val matrix = base.datatype.asInstanceOf[IR_MatrixDatatype]
//      idx.indices = List(matrix.sizeM * idx.indices(0) + idx.indices(1)).toArray
//      access
//    }
//  })
}

object IR_SetupMatrixExpressions extends DefaultStrategy("Convert accesses to matrices and vectors to MatrixExpressions") {
  def duplicateExpressions(access : IR_Expression, dt : IR_MatrixDatatype) = {
    var expressions = ListBuffer[IR_Expression]()

    for (row <- 0 until dt.sizeM)
      for (col <- 0 until dt.sizeN)
        expressions += IR_HighDimAccess(Duplicate(access), IR_ConstIndex(row, col))

    expressions.toArray
  }

  this += Transformation("Wrap", {
    case m : IR_MatrixExpression => m // no need to process further
    case hda : IR_HighDimAccess  => hda // no need to process further

    case access @ IR_VariableAccess(_, matrixDT : IR_MatrixDatatype) =>
      IR_MatrixExpression(Some(matrixDT.datatype), matrixDT.sizeM, matrixDT.sizeN, duplicateExpressions(access, matrixDT))

    case access : IR_MultiDimFieldAccess if access.fieldSelection.field.gridDatatype.isInstanceOf[IR_MatrixDatatype] =>
      val matrixDT = access.datatype.asInstanceOf[IR_MatrixDatatype]
      IR_MatrixExpression(Some(matrixDT.datatype), matrixDT.sizeM, matrixDT.sizeN, duplicateExpressions(access, matrixDT))

    // FIXME: add support for stencil fields

  }, false)
}

object IR_LinearizeMatrices extends DefaultStrategy("Linearize matrices") {
  this += Transformation("Linearize", {
//    case access @ IR_HighDimAccess(base : IR_ArrayAccess, idx : IR_ConstIndex) =>
//      val myidx = idx.toExpressionIndex
//      base.index = base.index + myidx(0)
//      base

    case access @ IR_HighDimAccess(base : IR_MultiDimFieldAccess, idx : IR_Index) =>
      val hoIdx = idx.toExpressionIndex
      val fieldLayout = base.fieldSelection.field.fieldLayout
      for (dim <- fieldLayout.numDimsGrid until fieldLayout.numDimsData) {
        if (base.index.indices.length <= dim)
          base.index.indices :+= hoIdx(dim - fieldLayout.numDimsGrid)
        else
          base.index.indices(dim) += hoIdx(dim - fieldLayout.numDimsGrid)
      }
      base

    case access @ IR_HighDimAccess(base, idx : IR_Index) if idx.indices.length == 2 =>
      val matrix = base.datatype.asInstanceOf[IR_MatrixDatatype]
      IR_ArrayAccess(base, matrix.sizeN * idx.indices(0) + idx.indices(1))
  }, false)
}
