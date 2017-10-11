package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.core._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.prettyprinting._
import exastencils.util.ir._

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

  def apply(datatype : IR_MatrixDatatype, expressions : ListBuffer[IR_Expression]) : IR_MatrixExpression = {
    val tmp = IR_MatrixExpression(datatype.datatype, datatype.sizeM, datatype.sizeN)
    tmp.expressions = expressions.toArray
    tmp
  }
}

case class IR_MatrixExpression(var innerDatatype : Option[IR_Datatype], var rows : Int, var columns : Int) extends IR_Expression {
  var expressions : Array[IR_Expression] = Array.ofDim[IR_Expression](rows * columns)

  override def datatype = {
    if (innerDatatype.isEmpty) {
      var ret = expressions(0).datatype
      expressions.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
      innerDatatype = Some(ret)
    } else if (innerDatatype.get.isInstanceOf[IR_MatrixDatatype]) {
      innerDatatype = Some(innerDatatype.get.asInstanceOf[IR_MatrixDatatype].resolveBaseDatatype)
    }
    IR_MatrixDatatype(innerDatatype.getOrElse(IR_RealDatatype), this.rows, this.columns)
  }

  def prettyprintInner(out : PpStream) : Unit = {
    if (Knowledge.experimental_internalHighDimTypes) {
      //out << "INVALID: IR_MatrixExpression"
      out << '{' << expressions.map(_.prettyprint).mkString(", ") << '}'
    } else {
      out << (if (Platform.targetCompiler == "GCC") "std::move((" else "((")
      innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
      out << "[]){" << (expressions.map(_.prettyprint).mkString(",")) << "})"
    }
  }
  override def prettyprint(out : PpStream) : Unit = {
    if (Knowledge.experimental_internalHighDimTypes) {
      //out << "INVALID: IR_MatrixExpression"
      out << "__matrix_"
      innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
      out << '_' << rows << "_" << columns << "_t "
      prettyprintInner(out)
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

object IR_ExtractMatrices extends DefaultStrategy("Extract and split matrix expressions where necessary") {
  val annotationFctCallCounter = "IR_ResolveMatrices.fctCallCounter"
  var fctCallCounter = 0
  // temporary variable used to replace function calls in expressions
  val annotationMatExpCounter = "IR_ResolveMatrices.matrixExpressionCounter"
  var matExpCounter = 0
  var resolveFunctions = ListBuffer[String]()
  var globalCollection : Option[IR_GlobalCollection] = None

  this.onBefore = () => {
    resolveFunctions.clear()
    resolveFunctions ++= ListBuffer("dotProduct", "dot", "crossProduct", "cross", "det", "transpose")
    if (Knowledge.experimental_resolveInverseFunctionCall != "Runtime") resolveFunctions += "inverse"
    globalCollection = StateManager.findFirst[IR_GlobalCollection]()
  }

  this += new Transformation("assignment of operation with self", {
    case stmt @ IR_Assignment(dest : IR_VariableAccess, src, _) if (dest.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      // resolve M = M * M into tmp = M * M; M = tmp
      var selfassign = false
      StateManager.findAll[IR_Multiplication](HelperNode(src)).foreach(mult =>
        if (mult.factors.exists(p => p.isInstanceOf[IR_VariableAccess] && p.asInstanceOf[IR_VariableAccess].name == dest.name))
          selfassign = true
      )

      if (selfassign) {
        var newStmts = ListBuffer[IR_Statement]()
        val decl = IR_VariableDeclaration(dest.datatype, "_matrixExp" + matExpCounter, None)
        newStmts += decl
        stmt.dest = IR_VariableAccess(decl)
        newStmts += stmt
        newStmts += IR_Assignment(dest, IR_VariableAccess(decl))
        matExpCounter += 1
        newStmts
      } else {
        stmt
      }
    }
  })

  if (Knowledge.experimental_resolveInverseFunctionCall == "Runtime") {
    this += new Transformation("preparation", {
      case call @ IR_FunctionCall(_, args) if (call.name == "inverse") => {
        val m = args(0).datatype.asInstanceOf[IR_MatrixDatatype]
        if (m.sizeM > 3) {
          call.function = IR_PlainInternalFunctionReference("_runtimeInverseMatrix", m)
        }
        call
      }
    })
  }

  this += new Transformation("global declarations", {
    case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, Some(exp : IR_Expression), _) => {
      StateManager.findFirst[IR_GlobalCollection]().get.initGlobals.asInstanceOf[IR_Function].body += IR_Assignment(IR_VariableAccess(Duplicate(decl)), exp)
      decl.initialValue = None
      decl
    }
  }, applyAtNode = StateManager.findFirst[IR_GlobalCollection]())

  this += new Transformation("declarations", {
    // Definition of matrix variable including initialisation -> split into decl and assignment
    case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, Some(exp : IR_Expression), _) => {
      val newStmts = ListBuffer[IR_Statement]()
      // split declaration and definition so each part can be handled by subsequent transformations
      newStmts += IR_VariableDeclaration(matrix, decl.name, None)
      newStmts += IR_Assignment(IR_VariableAccess(Duplicate(decl)), exp)
      newStmts
    }
  })

  this += new Transformation("extract function calls 1/2", {
    case stmt @ IR_Assignment(dest, src, op) if (src.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      // Extract all function calls into separate variables since any function could have unwanted side effects if called more than once
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_FunctionCall](src).filter(f => !resolveFunctions.contains(f.function.name)).foreach(exp => {
        val decl = IR_VariableDeclaration(exp.datatype, "_fct" + fctCallCounter + "_" + exp.name.replace('<', '_').replace('>', '_'), None)
        newStmts += decl
        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
        exp.annotate(annotationFctCallCounter, fctCallCounter)
        fctCallCounter += 1
      })
      // FIXME: only do the following if necessary
      //      StateManager.findAll[IR_MatrixExpression](src).foreach(exp => {
      //        val decl = IR_VariableDeclaration(exp.datatype, "_matrixExp" + matExpCounter, None)
      //        newStmts += decl
      //        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
      //        exp.annotate(annotationMatExpCounter, matExpCounter)
      //        matExpCounter += 1
      //      })
      newStmts += stmt
      newStmts
    }
    case stmt @ IR_ExpressionStatement(src) if (src.datatype.isInstanceOf[IR_MatrixDatatype])  => {
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_FunctionCall](src).foreach(exp => { // resolveFunction check not needed: all internally resolved function return a value
        val decl = IR_VariableDeclaration(exp.datatype, "_fct" + fctCallCounter + "_" + exp.name.replace('<', '_').replace('>', '_'), None)
        newStmts += decl
        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
        exp.annotate(annotationFctCallCounter, fctCallCounter)
        fctCallCounter += 1
      })
      //      StateManager.findAll[IR_MatrixExpression](src).foreach(exp => {
      //        val decl = IR_VariableDeclaration(exp.datatype, "_matrixExp" + matExpCounter, None)
      //        newStmts += decl
      //        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
      //        exp.annotate(annotationMatExpCounter, matExpCounter)
      //        matExpCounter += 1
      //      })
      newStmts += stmt
      newStmts
    }
  })

  this += new Transformation("extract function calls 2/2", {
    case exp : IR_FunctionCall if (exp.hasAnnotation(annotationFctCallCounter))    => {
      IR_VariableAccess("_fct" + exp.popAnnotation(annotationFctCallCounter).get + "_" + exp.function.name.replace('<', '_').replace('>', '_'), exp.function.returnType)
    }

    case exp : IR_MatrixExpression if (exp.hasAnnotation(annotationMatExpCounter)) => {
      IR_VariableAccess("_matrixExp" + exp.popAnnotation(annotationMatExpCounter).get, exp.datatype)
    }
  })

  this += new Transformation("parameters and return types", {
    case arg : IR_FunctionArgument if (arg.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      arg.datatype = IR_ReferenceDatatype(arg.datatype)
      arg
    }
    case func : IR_Function if (func.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      val matrix = func.datatype.asInstanceOf[IR_MatrixDatatype]
      func.parameters += IR_FunctionArgument("_matrix_return", IR_ReferenceDatatype(matrix))
      func.datatype = IR_UnitDatatype

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
    case stmt @ IR_Assignment(dest, src : IR_FunctionCall, "=") if (dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype])  => {
      // FIXME resolve IR_Assignments with operator += before this
      src.arguments += dest
      IR_ExpressionStatement(src)
    }
    case stmt @ IR_Assignment(dest, src : IR_FunctionCall, "+=") if (dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      Logger.error("+= matrix operator resolution not yet implemented")
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
    case stmt @ IR_Assignment(_, exp : IR_FunctionCall, _) if !resolveFunctions.contains(exp.function.name) => {
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
}

object IR_ResolveMatrixFunctions extends DefaultStrategy("Resolve special matrix and vector functions") {
  val annotationMatrixRow = "IR_ResolveMatrices.matrixRow"
  val annotationMatrixCol = "IR_ResolveMatrices.matrixCol"

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
        val tmpDet = m.get(i, 0) * calculateDeterminant(tmp) * IR_DoubleConstant(math.pow(-1, i))
        det += IR_GeneralSimplifyWrapper.process[IR_Expression](tmpDet)
      }
      IR_GeneralSimplifyWrapper.process(det)
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

  def findPivot(matrix : IR_MatrixExpression, startRow : Int) : Int = {
    var myMax = (0, 0.0)
    for (row <- startRow until matrix.rows) {
      val myElem = matrix.get(row, startRow)
      myElem match {
        case x : IR_IntegerConstant => if (Math.abs(x.value) > myMax._2) myMax = (row, Math.abs(x.v))
        case x : IR_RealConstant    => if (Math.abs(x.value) > myMax._2) myMax = (row, Math.abs(x.v))
        case x : IR_FloatConstant   => if (Math.abs(x.value) > myMax._2) myMax = (row, Math.abs(x.v))
        case _                      =>
      }
    }
    if (myMax._2 == 0.0) {
      // it's not constant 0 (or all the other stuff was 0 as well), so let's hope we gonna be fine...
      return startRow
    } else {
      return myMax._1
    }
  }

  def getElem(exp : IR_Expression, row : Integer, col : Integer) = {
    exp match {
      case x : IR_MatrixExpression                                           => x.get(row, col)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(row, col)))
      case _                                                                 => Logger.error(s"Dot product argument is of wrong type ${ exp.getClass.getTypeName }: $exp")
    }
  }

  this += new Transformation("resolution of built-in functions 2/2", {
    case call : IR_FunctionCall if (call.name == "dotProduct" || call.name == "dot") => {
      if (call.arguments.length != 2) {
        Logger.error(s"dotProduct() must have two arguments; has ${ call.arguments.length }")
      }

      val left = call.arguments(0)
      val right = call.arguments(1)

      val lsize = left match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Dot product argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }
      val rsize = right match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Dot product argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }

      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for dotProduct() - attempting ${ lsize._1 }x${ lsize._2 } * ${ rsize._1 }x${ rsize._2 }")
      }
      var additions = ListBuffer[IR_Expression]()
      for (row <- 0 until lsize._1) {
        for (col <- 0 until lsize._2) {
          additions += IR_Multiplication(getElem(left, row, col), getElem(right, row, col))
        }
      }
      IR_Addition(additions)
    }

    case call : IR_FunctionCall if call.name == "crossProduct" || call.name == "cross" =>
      if (call.arguments.length != 2) {
        Logger.error(s"Cross product must have two arguments; has ${ call.arguments.length }")
      }

      val left = call.arguments(0)
      val right = call.arguments(1)

      val lsize = left match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Cross product argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }
      val rsize = right match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Cross product argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }

      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for dotProduct() - attempting ${ lsize._1 }x${ lsize._2 } * ${ rsize._1 }x${ rsize._2 }")
      }
      lsize._1 match {
        case 2     => getElem(left, 0, 0) * getElem(right, 1, 0) - getElem(left, 1, 0) * getElem(right, 0, 0)
        case 3     => ???
        case other => Logger.error(s"Cross product is not defined for dimensionality $other")
      }

    // FIXME: other vec functions: length, normalize

    case call : IR_FunctionCall if (Knowledge.experimental_resolveInverseFunctionCall != "Runtime" && call.name == "inverse") => {
      if (call.arguments.length != 1) {
        Logger.error("inverse() must have one argument")
      }
      call.arguments(0) match {
        case s : IR_Expression if (s.datatype.isInstanceOf[IR_ScalarDatatype])  => 1 / s
        case s : IR_Expression if (s.datatype.isInstanceOf[IR_ComplexDatatype]) => 1 / s
        case m : IR_MatrixExpression                                            =>
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
              // TODO gather and exploit knowledge about matrix structure
              Knowledge.experimental_resolveInverseFunctionCall match {
                case "Cofactors"   => {
                  val inv_det = IR_IntegerConstant(1) / calculateDeterminant(m)
                  val tmp = IR_MatrixExpression(Some(m.innerDatatype.getOrElse(IR_RealDatatype)), m.rows, m.columns)
                  for (row <- 0 until m.rows) {
                    for (col <- 0 until m.columns) {
                      tmp.set(col, row, calculateMatrixOfMinorsElement(m, row, col) * IR_DoubleConstant(math.pow(-1, row + col)) * inv_det)
                    }
                  }
                  tmp
                }
                case "GaussJordan" => {
                  val matrix = Duplicate(m)
                  val other = IR_MatrixExpression(matrix.datatype, matrix.rows, matrix.columns)
                  for (i <- 0 until other.rows) {
                    for (j <- 0 until other.columns) {
                      if (i == j) other.set(i, j, 1.0); else other.set(i, j, 0.0)
                    }
                  }

                  for (i <- matrix.rows - 1 to 0) {
                    var swap = false
                    val topValue = matrix.get(i - 1, i)
                    val currentValue = matrix.get(i, i)
                    (topValue, currentValue) match {
                      case (top : IR_Number, current : IR_Number) => swap = Math.abs(top.value.asInstanceOf[Number].doubleValue) > Math.abs(current.value.asInstanceOf[Number].doubleValue)
                      case _                                      =>
                    }

                    if (swap) {
                      for (j <- 0 until matrix.columns) {
                        var d = matrix.get(i, j)
                        matrix.set(i, j, matrix.get(i - 1, j))
                        matrix.set(i - 1, j, d)
                        d = other.get(i, j)
                        other.set(i, j, other.get(i - 1, j))
                        other.set(i - 1, j, d)
                      }
                    }
                  }

                  for (i <- 0 until matrix.rows) {
                    for (j <- 0 until matrix.rows) {
                      if (j != i) {
                        val d = matrix.get(j, i) / matrix.get(i, i)
                        for (k <- 0 until matrix.rows) {
                          var newExp = matrix.get(j, k) - Duplicate(matrix.get(i, k)) * Duplicate(d)
                          matrix.set(j, k, newExp)

                          newExp = other.get(j, k) - Duplicate(other.get(i, k)) * Duplicate(d)
                          other.set(j, k, newExp)
                        }
                      }
                    }
                  }

                  IR_GeneralSimplify.doUntilDoneStandalone(matrix)

                  for (i <- 0 until matrix.rows) {
                    val d = matrix.get(i, i)
                    for (j <- 0 until matrix.rows) {
                      val newExp = other.get(i, j) / Duplicate(d)
                      other.set(i, j, newExp)
                    }
                  }
                  other
                }
                case "Runtime"     => Logger.warn("'Runtime' matrix inversion chosen but in code path for compile time"); call
                case _             => Logger.error(s"""Unknown matrix inversion resolution strategy "${ Knowledge.experimental_resolveInverseFunctionCall }""""); call
              }
            }
          }
          ret
      }
    }
    case call : IR_FunctionCall if (call.name == "det")                                                                       => {
      if (call.arguments.length != 1) {
        Logger.error("det() must have one argument")
      }
      val m = call.arguments(0).asInstanceOf[IR_MatrixExpression]
      calculateDeterminant(m)
    }

    // FIXME shorten this code (less code duplication)
    case call @ IR_ElementwiseMultiplication(left, right) => {
      val lsize = left match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Element-wise operation argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }
      val rsize = right match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Element-wise operation argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }

      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for .* - attempting ${ lsize._1 }x${ lsize._2 } * ${ rsize._1 }x${ rsize._2 }")
      }
      val me = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
      for (row <- 0 until lsize._1) {
        for (col <- 0 until lsize._2) {
          me.set(row, col, IR_Multiplication(getElem(left, row, col), getElem(right, row, col)))
        }
      }
      me
    }

    case call @ IR_ElementwiseDivision(left, right) => {
      val lsize = left match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Element-wise operation argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }
      val rsize = right match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Element-wise operation argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }

      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for ./ - attempting ${ lsize._1 }x${ lsize._2 } * ${ rsize._1 }x${ rsize._2 }")
      }
      val me = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
      for (row <- 0 until lsize._1) {
        for (col <- 0 until lsize._2) {
          me.set(row, col, IR_Division(getElem(left, row, col), getElem(right, row, col)))
        }
      }
      me
    }

    case call @ IR_ElementwiseModulo(left, right) => {
      val lsize = left match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Element-wise operation argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }
      val rsize = right match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Element-wise operation argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }

      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for .% - attempting ${ lsize._1 }x${ lsize._2 } * ${ rsize._1 }x${ rsize._2 }")
      }
      val me = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
      for (row <- 0 until lsize._1) {
        for (col <- 0 until lsize._2) {
          me.set(row, col, IR_Modulo(getElem(left, row, col), getElem(right, row, col)))
        }
      }
      me
    }

    case call @ IR_ElementwisePower(left, right) => {
      val lsize = left match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Element-wise operation argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }
      right match {
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => Logger.error(s"Element-wise operation argument is of wrong type ${ va.getClass.getTypeName }: $va")
        case other                                               =>
      }

      val me = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
      var exps = ListBuffer[IR_Expression]()
      for (row <- 0 until lsize._1) {
        for (col <- 0 until lsize._2) {
          me.set(row, col, IR_Power(getElem(left, row, col), right))
        }
      }
      me
    }

    case call : IR_FunctionCall if call.name == "transpose" => {
      if (call.arguments.length != 1) {
        Logger.error(s"Transpose operation must have one arguments; has ${ call.arguments.length }")
      }

      val left = call.arguments(0)
      var transform = true

      val lsize = left match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case _                                                   => transform = false; (0, 0)
      }
      if (transform) {
        var me = IR_MatrixExpression(left.datatype, lsize._2, lsize._1)
        var additions = ListBuffer[IR_Expression]()
        for (row <- 0 until lsize._1) {
          for (col <- 0 until lsize._2) {
            me.set(col, row, getElem(left, row, col))
          }
        }
        me
      } else {
        call
      }
    }

  })

  if (Knowledge.experimental_resolveInverseFunctionCall == "Runtime") {
    def runtimeInverse(in : IR_VariableAccess, out : IR_VariableAccess) = {
      def printMatrix(matrix : IR_VariableAccess) = {
        val stmts = ListBuffer[IR_Statement]()
        matrix.datatype match {
          case dt : IR_MatrixDatatype => {
            for (i <- 0 until dt.sizeM) {
              for (j <- 0 until dt.sizeN) {
                stmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, ListBuffer[IR_Expression](IR_StringConstant("%e "), IR_HighDimAccess(matrix, IR_ConstIndex(i, j)))))
              }
              stmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("\\n")))
            }
          }
        }
        stmts
      }
      def mkConstant(dt : IR_Datatype, v : Double) = dt match {
        case IR_RealDatatype    => IR_RealConstant(v)
        case IR_IntegerDatatype => IR_IntegerConstant(v.toInt)
        case _                  => exastencils.logger.Logger.error("mkConstant not implemented for " + dt.toString)
      }

      val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]

      val N = inDt.sizeM
      val myType = inDt.resolveBaseDatatype
      val func = IR_Scope(Nil)
      val myL = IR_VariableAccess("_L", IR_MatrixDatatype(myType, N, N))
      val myU = IR_VariableAccess("_U", IR_MatrixDatatype(myType, N, N))
      val myQ = IR_VariableAccess("_q", IR_ArrayDatatype(IR_IntegerDatatype, N))
      val myI = IR_VariableAccess("_i", IR_IntegerDatatype)
      val myJ = IR_VariableAccess("_j", IR_IntegerDatatype)
      val myK = IR_VariableAccess("_k", IR_IntegerDatatype)
      func.body += IR_VariableDeclaration(myL)
      func.body += IR_VariableDeclaration(myU)
      func.body += IR_Assignment(myL, mkConstant(myType, 0))
      func.body += IR_Assignment(myU, in)
      func.body += IR_VariableDeclaration(myQ)
      func.body += IR_Assignment(out, mkConstant(myType, 0))
      for (i <- 0 until N) {
        func.body += IR_Assignment(IR_HighDimAccess(myL, IR_ConstIndex(i, i)), mkConstant(myType, 1))
      }
      for (i <- 0 until N) {
        func.body += IR_Assignment(IR_ArrayAccess(myQ, IR_IntegerConstant(i)), IR_IntegerConstant(i))
      }
      val myColmax = IR_VariableAccess("_colmax", myType)
      val myMaxCol = IR_VariableAccess("_maxCol", IR_IntegerDatatype)
      func.body += IR_ForLoop(IR_VariableDeclaration(myK, IR_IntegerConstant(0)), IR_Lower(myK, IR_IntegerConstant(N - 1)), IR_ExpressionStatement(IR_PreIncrement(myK)), ListBuffer[IR_Statement](
        IR_VariableDeclaration(myColmax, IR_RealConstant(0)),
        IR_VariableDeclaration(myMaxCol, myK),
        IR_ForLoop(IR_VariableDeclaration(myJ, myK), IR_Lower(myJ, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myJ)), ListBuffer[IR_Statement](
          IR_IfCondition(IR_Greater(IR_FunctionCall(IR_MathFunctionReference.fabs, IR_HighDimAccess(myU, IR_ExpressionIndex(myK, myJ))), myColmax), ListBuffer[IR_Statement](
            IR_Assignment(myColmax, IR_FunctionCall(IR_MathFunctionReference.fabs, IR_HighDimAccess(myU, IR_ExpressionIndex(myK, myJ)))),
            IR_Assignment(myMaxCol, myJ)
          ))
        )),
        IR_IfCondition(IR_Lower(myColmax, mkConstant(myType, 1e-15)),
          IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[WARNING] Inverting potentially singular matrix\\n"))) +:
            printMatrix(myU)
        ),
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::swap"), IR_ArrayAccess(myQ, myK), IR_ArrayAccess(myQ, myMaxCol)))
      ))
      for (i <- 0 until N) {
        func.body.last.asInstanceOf[IR_ForLoop].body += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::swap"), IR_HighDimAccess(myL, IR_ExpressionIndex(IR_IntegerConstant(i), myK)), IR_HighDimAccess(myL, IR_ExpressionIndex(IR_IntegerConstant(i), myMaxCol))))
      }
      for (i <- 0 until N) {
        func.body.last.asInstanceOf[IR_ForLoop].body += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::swap"), IR_HighDimAccess(myU, IR_ExpressionIndex(IR_IntegerConstant(i), myK)), IR_HighDimAccess(myU, IR_ExpressionIndex(IR_IntegerConstant(i), myMaxCol))))
      }
      func.body.last.asInstanceOf[IR_ForLoop].body += IR_ForLoop(IR_VariableDeclaration(myI, myK + IR_IntegerConstant(1)), IR_Lower(myI, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myI)), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myK)), IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myK)) / IR_HighDimAccess(myU, IR_ExpressionIndex(myK, myK))),
        IR_ForLoop(IR_VariableDeclaration(myJ, myK), IR_Lower(myJ, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myJ)), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myJ)), IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myJ)) - IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myK)) * IR_HighDimAccess(myU, IR_ExpressionIndex(myK, myJ)))
        ))
      ))
      val myY = IR_VariableAccess("_y", IR_MatrixDatatype(myType, N, N))
      val mySum = IR_VariableAccess("_sum", myType)
      func.body += IR_VariableDeclaration(myY)
      func.body += IR_ForLoop(IR_VariableDeclaration(myJ, IR_IntegerConstant(0)), IR_Lower(myJ, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myJ)), ListBuffer[IR_Statement](
        IR_ForLoop(IR_VariableDeclaration(myI, IR_IntegerConstant(0)), IR_Lower(myI, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myI)), ListBuffer[IR_Statement](
          IR_VariableDeclaration(mySum, IR_RealConstant(0)),
          IR_ForLoop(IR_VariableDeclaration(myK, IR_IntegerConstant(0)), IR_Lower(myK, myI), IR_ExpressionStatement(IR_PreIncrement(myK)), ListBuffer[IR_Statement](
            IR_Assignment(mySum, mySum + IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myK)) * IR_HighDimAccess(myY, IR_ExpressionIndex(myK, myJ)))
          )),
          IR_IfCondition(IR_EqEq(myI, myJ),
            IR_Assignment(IR_HighDimAccess(myY, IR_ExpressionIndex(myI, myJ)), (mkConstant(myType, 1) - mySum) / IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myI))),
            IR_Assignment(IR_HighDimAccess(myY, IR_ExpressionIndex(myI, myJ)), (mkConstant(myType, 0) - mySum) / IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myI)))
          )
        )),
        IR_ForLoop(IR_VariableDeclaration(myI, IR_IntegerConstant(N) - IR_IntegerConstant(1)), IR_GreaterEqual(myI, IR_IntegerConstant(0)), IR_ExpressionStatement(IR_PreDecrement(myI)), ListBuffer[IR_Statement](
          IR_VariableDeclaration(mySum, IR_RealConstant(0)),
          IR_ForLoop(IR_VariableDeclaration(myK, IR_IntegerConstant(N) - IR_IntegerConstant(1)), IR_Greater(myK, myI), IR_ExpressionStatement(IR_PreDecrement(myK)), ListBuffer[IR_Statement](
            IR_Assignment(mySum, mySum + IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myK)) * IR_HighDimAccess(out, IR_ExpressionIndex(myK, myJ)))
          )),
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(myI, myJ)), (IR_HighDimAccess(myY, IR_ExpressionIndex(myI, myJ)) - mySum) / IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myI)))
        ))
      ))

      func
    }
    this += new Transformation("resolve runtime inversion", {
      case IR_ExpressionStatement(call @ IR_FunctionCall(_, ListBuffer(in : IR_VariableAccess, out : IR_VariableAccess))) if (call.name == "_runtimeInverseMatrix") => {
        runtimeInverse(in, out)
      }
    })
  }
}

object IR_ResolveMatrixAssignments extends DefaultStrategy("Resolve assignments to matrices") {
  val annotationMatrixRow = "IR_ResolveMatrices.matrixRow"
  val annotationMatrixCol = "IR_ResolveMatrices.matrixCol"

  this += new Transformation("scalarize 1/2", {
    case stmt @ IR_Assignment(dest, src, op) if (dest.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      val matrix = dest.datatype.asInstanceOf[IR_MatrixDatatype]
      var newStmts = ListBuffer[IR_Statement]()
      for (row <- 0 until matrix.sizeM) {
        for (col <- 0 until matrix.sizeN) {
          var cloned = Duplicate(stmt)
          StateManager.findAll[IR_Expression](cloned).foreach(exp => exp match {
            case x : IR_FunctionArgument                                                                                                            => // do not mark function arguments to be resolved into individual accesses
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
    case m : IR_MatrixExpression                      => m // no need to process further
    case hda : IR_HighDimAccess                       => hda // no need to process further
    case x : IR_FunctionCall if (x.name != "inverse") => x

    case access @ IR_VariableAccess(_, matrixDT : IR_MatrixDatatype) => IR_MatrixExpression(Some(matrixDT.datatype), matrixDT.sizeM, matrixDT.sizeN, duplicateExpressions(access, matrixDT))

    case access : IR_MultiDimFieldAccess if access.datatype.isInstanceOf[IR_MatrixDatatype] =>
      val matrixDT = access.datatype.asInstanceOf[IR_MatrixDatatype]
      IR_MatrixExpression(Some(matrixDT.datatype), matrixDT.sizeM, matrixDT.sizeN, duplicateExpressions(access, matrixDT))

    // FIXME: add support for stencil fields
  }, false)
}

object IR_LinearizeMatrices extends DefaultStrategy("Linearize matrices") {
  this += Transformation("Linearize", {
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

    case access @ IR_HighDimAccess(base, idx : IR_ConstIndex) if idx.indices.length == 2 =>
      val matrix = base.datatype.asInstanceOf[IR_MatrixDatatype]
      if (matrix.sizeM == matrix.sizeN == 1 && idx(0) == idx(1) == 0) base
      else
      IR_ArrayAccess(base, matrix.sizeN * idx.indices(0) + idx.indices(1))

    case access @ IR_HighDimAccess(base, idx : IR_ExpressionIndex) if idx.indices.length == 2 =>
      val matrix = base.datatype.asInstanceOf[IR_MatrixDatatype]
      if (matrix.sizeM == matrix.sizeN == 1 && idx(0) == idx(1) == 0) base
      else
      IR_ArrayAccess(base, matrix.sizeN * idx.indices(0) + idx.indices(1))
  }, false)
}
