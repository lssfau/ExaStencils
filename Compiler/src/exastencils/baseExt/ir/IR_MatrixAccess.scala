package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.core._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_GeneralSimplify
import exastencils.prettyprinting._
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
    } else if (innerDatatype.get.isInstanceOf[IR_MatrixDatatype]) {
      innerDatatype = Some(innerDatatype.get.asInstanceOf[IR_MatrixDatatype].resolveBaseDatatype)
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

case object IR_RuntimeInverseMatrix extends IR_AbstractFunction(false) with PrettyPrintable {
  def name = "_runtimeInverseMatrix"

  //override def prettyprint_decl() : String = "template<size_t N, typename T> void _runtimeInverseMatrix(T*, T*);\n"

  override def prettyprint(out : PpStream) = {}

  override def prettyprint_decl() : String = {
    """
template<size_t N, typename T> void _printMatrix(std::ostream& stream, T* A) {
    for(size_t i = 0; i < N; i++) {
        for(size_t j = 0; j < N; ++j) {
            stream << A[i * N + j] << "\t";
        }
        stream << "\n";
    }
    stream << std::endl;
}
template<size_t N, typename T>
void _runtimeInverseMatrix(T* in, T* out) {
    T L[N * N];
    T U[N * N];
    size_t p[N], q[N];

    std::copy(in, in + N * N, U);
    std::fill(std::begin(L), std::end(L), 0);
    for (size_t i = 0; i < N * N; i += N + 1)
    {
        L[i] = 1;
    }

    for (size_t i = 0; i < N; ++i)
    {
        p[i] = i;
        q[i] = i;
    }

    for (size_t k = 0; k < N - 1; ++k)
    {
        T colmax(0);
        size_t /*maxrow = 0,*/ maxcol = 0; // will always be >= k
        //for (size_t i = k; i < N; ++i) {
        for (size_t j = k; j < N; ++j)
        {
            if (std::abs(U[k * N + j]) > colmax)
            {
                colmax = std::abs(U[k * N + j]);
                //maxrow = i; // no row swapping for now
                maxcol = j;
            }
        }
        if (colmax < 1e-15)
        {
            // matrix singular -> print warning
            std::cerr << "WARNING: matrix potentially singular:" << std::endl;
            _printMatrix<N>(std::cerr, U);
        }
        //}
        //std::swap ( p[k], p[maxrow] );
        //std::swap_ranges(L + k * N, L + (k+1) * N, L + maxrow * N);
        //std::swap_ranges(U + k * N, U + (k+1) * N, U + maxrow * N);

        std::swap(q[k], q[maxcol]);
        for (size_t i = 0; i < N; ++i)
        {
            std::swap(L[i * N + k], L[i * N + maxcol]);
            std::swap(U[i * N + k], U[i * N + maxcol]);
        }

        for (size_t i = k + 1; i < N; ++i)
        {
            L[i * N + k] = U[i * N + k] / U[k * N + k];
            for (size_t j = k; j < N; ++j)
            {
                U[i * N + j] -= L[i * N + k] * U[k * N + j];
            }
        }
    }

    T y[N * N];

    for (size_t j = 0; j < N; ++j)
    {
        for (size_t i = 0; i < N; ++i)
        {
            T sum(0);
            for (size_t k = 0; k < i; ++k)
            {
                sum += L[i * N + k] * y[k * N + j];
            }

            if (p[i] == j)
            {
                y[i * N + j] = (1 - sum) / L[i * N + i];
            }
            else
            {
                y[i * N + j] = (0 - sum) / L[i * N + i];
            }
        }

        for (size_t i = N - 1; i <= SIZE_MAX - 1; --i)
        {
            T sum = 0;
            for (size_t k = N - 1; k > i; --k)
            {
                sum += U[i * N + k] * out[k * N + j];
            }
            out[i * N + j] = (y[i * N + j] - sum) / U[i * N + i];
        }
    }
}
    """
  }
}

object IR_ExtractMatrices extends DefaultStrategy("Extract and split matrix expressions where necessary") {
  val annotationFctCallCounter = "IR_ResolveMatrices.fctCallCounter"
  var fctCallCounter = 0
  // temporary variable used to replace function calls in expressions
  val annotationMatExpCounter = "IR_ResolveMatrices.matrixExpressionCounter"
  var matExpCounter = 0
  var resolveFunctions = ListBuffer[String]()
  var runtimeFunctions = ListBuffer[String]()

  override def apply(applyAtNode : Option[Node]) : Unit = {
    resolveFunctions.clear()
    resolveFunctions ++= ListBuffer("dotProduct", "dot", "crossProduct", "cross", "det")
    runtimeFunctions.clear()
    if (Knowledge.experimental_resolveInverseFunctionCall == "Runtime") {
      runtimeFunctions += "inverse"
      IR_UserFunctions.get.functions += IR_RuntimeInverseMatrix
    } else {
      resolveFunctions += "inverse"
    }

    super.apply(applyAtNode)
  }

  this += new Transformation("declarations", {
    // Definition of matrix variable including initialisation -> split into decl and assignment
    case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, Some(exp : IR_Expression)) => {
      val newStmts = ListBuffer[IR_Statement]()
      // split declaration and definition so each part can be handled by subsequent transformations
      newStmts += IR_VariableDeclaration(matrix, decl.name, None)
      newStmts += IR_Assignment(IR_VariableAccess(Duplicate(decl)), exp)
      newStmts
    }
  })

  this += new Transformation("preparation", {
    case call : IR_FunctionCall if (Knowledge.experimental_resolveInverseFunctionCall == "Runtime" && call.name == "inverse") => {
      val m = call.arguments(0).datatype.asInstanceOf[IR_MatrixDatatype]
      if (m.sizeM > 3) {
        call.function = new IR_UserFunctionAccess("_runtimeInverseMatrix<" + m.sizeM + ">", m)
      }
      call
    }
  })

  this += new Transformation("extract function calls 1/2", {
    case stmt @ IR_Assignment(dest, src, op) if (src.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      // Extract all function calls into separate variables since any function could have unwanted side effects if called more than once
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_FunctionCall](src).foreach(exp => {
        val decl = IR_VariableDeclaration(exp.datatype, "_fct" + fctCallCounter + "_" + exp.name.replace('<', '_').replace('>', '_'), None)
        newStmts += decl
        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
        exp.annotate(annotationFctCallCounter, fctCallCounter)
        fctCallCounter += 1
      })
      // FIXME: only do the following if necessary
      StateManager.findAll[IR_MatrixExpression](src).foreach(exp => {
        val decl = IR_VariableDeclaration(exp.datatype, "_matrixExp" + matExpCounter, None)
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
        val decl = IR_VariableDeclaration(exp.datatype, "_fct" + fctCallCounter + "_" + exp.name.replace('<', '_').replace('>', '_'), None)
        newStmts += decl
        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
        exp.annotate(annotationFctCallCounter, fctCallCounter)
        fctCallCounter += 1
      })
      StateManager.findAll[IR_MatrixExpression](src).foreach(exp => {
        val decl = IR_VariableDeclaration(exp.datatype, "_matrixExp" + matExpCounter, None)
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
      IR_VariableAccess("_fct" + exp.popAnnotation(annotationFctCallCounter).get + "_" + exp.function.name.replace('<', '_').replace('>', '_'), exp.function.datatype)
    }
    case exp : IR_MatrixExpression if (exp.hasAnnotation(annotationMatExpCounter)) => {
      IR_VariableAccess("_matrixExp" + exp.popAnnotation(annotationMatExpCounter).get, exp.datatype)
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
        Logger.pushLevel(Logger.WARNING)
        IR_GeneralSimplify.applyStandalone(tmpDet)
        Logger.popLevel()
        det += tmpDet
      }
      Logger.pushLevel(Logger.WARNING)
      IR_GeneralSimplify.applyStandalone(det)
      Logger.popLevel()
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

    case call : IR_FunctionCall if call.name == "crossProduct" || call.name == "cross" =>
      if (call.arguments.length != 2) {
        Logger.error(s"Cross product must have two arguments; has ${ call.arguments.length }")
      }
      val left = call.arguments(0) match {
        case me : IR_MatrixExpression => me
        case other                    => Logger.error(s"Cross product argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }
      val right = call.arguments(1) match {
        case me : IR_MatrixExpression => me
        case other                    => Logger.error(s"Cross product argument is of wrong type ${ other.getClass.getTypeName }: $other")
      }
      if (left.rows != right.rows || left.columns != 1 || right.columns != 1) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for cross product - attempting ${ left.rows }x${ left.columns } ; ${ right.rows }x${ right.columns }")
      }
      left.rows match {
        case 2     => left.get(0, 0) * right.get(1, 0) - left.get(1, 0) * right.get(0, 0)
        case 3     => ???
        case other => Logger.error(s"Cross product is not defined for dimensionality $other")
      }

      // FIXME: other vec functions: length, normalize

    case call : IR_FunctionCall if (call.name == "inverse") => {
      if (Knowledge.experimental_resolveInverseFunctionCall != "Runtime") {
        if (call.arguments.length != 1) {
          Logger.error("inverse() must have one argument")
        }
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
          // TODO exploit knowledge about matrix structure
          Knowledge.experimental_resolveInverseFunctionCall match {
            case "Runtime"     => {
              call.function.name = "_runtimeInverseMatrix<" + m.rows + ">"
              call
            }
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

              Logger.pushLevel(Logger.WARNING)
              IR_GeneralSimplify.applyStandalone(matrix)
              Logger.popLevel()

              for (i <- 0 until matrix.rows) {
                val d = matrix.get(i, i)
                for (j <- 0 until matrix.rows) {
                  val newExp = other.get(i, j) / Duplicate(d)
                  other.set(i, j, newExp)
                }
              }
              other
            }
          }
        }
      }
      ret
    }
    case call : IR_FunctionCall if (call.name == "det")     => {
      if (call.arguments.length != 1) {
        Logger.error("det() must have one argument")
      }
      val m = call.arguments(0).asInstanceOf[IR_MatrixExpression]
      calculateDeterminant(m)
    }
  })
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
    case m : IR_MatrixExpression                                                => m // no need to process further
    case hda : IR_HighDimAccess                                                 => hda // no need to process further
    case x : IR_FunctionCall if (x.function.name.startsWith("_runtimeInverse")) => x // FIXME remove after inlining runtimeInverse()

    case access @ IR_VariableAccess(_, matrixDT : IR_MatrixDatatype) =>
      IR_MatrixExpression(Some(matrixDT.datatype), matrixDT.sizeM, matrixDT.sizeN, duplicateExpressions(access, matrixDT))

    case access : IR_MultiDimFieldAccess if access.datatype.isInstanceOf[IR_MatrixDatatype] =>
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
