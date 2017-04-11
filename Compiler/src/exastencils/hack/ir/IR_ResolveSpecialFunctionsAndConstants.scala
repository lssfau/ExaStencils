package exastencils.hack.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.{ IR_FunctionCall, _ }
import exastencils.baseExt.ir._
import exastencils.boundary.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.core.collectors.StackCollector
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger
import exastencils.parallelization.api.cuda._
import exastencils.parallelization.api.mpi._

/// HACK_IR_ResolveSpecialFunctionsAndConstants

// TODO: split according to functionality and move to appropriate packages
object HACK_IR_ResolveSpecialFunctionsAndConstants extends DefaultStrategy("ResolveSpecialFunctionsAndConstants") {
  var collector = new StackCollector
  this.register(collector)

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
      var det : IR_Expression = 0
      var tmp = IR_MatrixExpression(Some(m.innerDatatype.getOrElse(IR_RealDatatype)), m.rows - 1, m.columns - 1)
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
        det += m.get(i, 0) * calculateDeterminant(tmp) * math.pow(-1, i)
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

  def getIndex(fieldAccess : IR_FieldAccess) = {
    val index = fieldAccess.index
    if (fieldAccess.offset.isDefined)
      for (i <- 0 until Math.min(fieldAccess.index.length, fieldAccess.offset.get.length))
        index(i) += fieldAccess.offset.get(i)
    index
  }

  this += new Transformation("SearchAndReplace", {
    // functions
    // FIXME: datatypes for function accesses

    case IR_FunctionCall(IR_UserFunctionAccess("concat", _), args) => Logger.error("Concat expression is deprecated => will be deleted soon")

    // Vector functions
    case f : IR_FunctionCall if f.name == "cross" || f.name == "crossproduct" =>
      f.arguments.foreach(a => if ((f.arguments(0).isInstanceOf[IR_VectorExpression] || f.arguments(0).isInstanceOf[IR_VectorExpression])
        && a.getClass != f.arguments(0).getClass) Logger.error("Must have matching types!"))
      f.arguments.foreach(a => if (a.asInstanceOf[IR_VectorExpression].length != f.arguments(0).asInstanceOf[IR_VectorExpression].length) Logger.error("Vectors must have matching lengths"))
      if (f.arguments.length + 1 != f.arguments(0).asInstanceOf[IR_VectorExpression].length) Logger.error("Must have matching number of vector arguments!")
      // For now: Restrict to 3 dimensions
      if (f.arguments.length != 2) Logger.error("Cross product only defined for 2D vectors!")

      val x = f.arguments(0).asInstanceOf[IR_VectorExpression]
      val y = f.arguments(1).asInstanceOf[IR_VectorExpression]
      if (!x.isConstant || !y.isConstant) {
        f // do nothing for vectors containing variable expressions
      } else {
        val r = ListBuffer[IR_Expression](x(1) * y(2) - x(2) * y(1), x(2) * y(0) - x(0) * y(2), x(0) * y(1) - x(1) * y(0))
        IR_VectorExpression(x.innerDatatype, r, x.rowVector)
      }
    // Matrix functions
    case x : IR_FunctionCall if x.name == "det" && x.arguments.size == 1 && exastencils.config.Knowledge.experimental_internalHighDimTypes =>
      x.arguments(0) match {
        case m : IR_MatrixExpression => calculateDeterminant(m)
        case _                       => x
      }
    case x : IR_FunctionCall if x.name == "inverse" && exastencils.config.Knowledge.experimental_internalHighDimTypes                      =>
      if (x.arguments.size == 1) {
        x.arguments(0) match {
          case m : IR_MatrixExpression                                             => {
            val m = x.arguments(0).asInstanceOf[IR_MatrixExpression]
            m.innerDatatype match {
              case Some(IR_IntegerDatatype)                     => {
                Logger.warn("Converting matrix expression to real data type for inversion")
                m.innerDatatype = Some(IR_RealDatatype)
              }
              case Some(IR_ComplexDatatype(IR_IntegerDatatype)) => {
                Logger.warn("Converting matrix expression to real data type for inversion")
                m.innerDatatype = Some(IR_ComplexDatatype(IR_RealDatatype))
              }
              case _                                            =>
            }
            if (m.rows == 1 && m.columns == 1) {
              IR_MatrixExpression(m.innerDatatype, 1, 1, Array(1.0 / m.get(0, 0)))
            } else if (m.rows == 2 && m.columns == 2) {
              val a = m.get(0, 0)
              val b = m.get(0, 1)
              val c = m.get(1, 0)
              val d = m.get(1, 1)
              val det : IR_Expression = 1.0 / (a * d - b * c)
              IR_MatrixExpression(m.innerDatatype, 2, 2, Array(Duplicate(det) * Duplicate(d), Duplicate(det) * Duplicate(b) * (-1), Duplicate(det) * Duplicate(c) * (-1), Duplicate(det) * Duplicate(a)))
            } else if (m.rows == 3 && m.columns == 3) {
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
              val B = -1 * (Duplicate(d) * Duplicate(i) - Duplicate(f) * Duplicate(g))
              val C = Duplicate(d) * Duplicate(h) - Duplicate(e) * Duplicate(g)
              val D = -1 * (Duplicate(b) * Duplicate(i) - Duplicate(c) * Duplicate(h))
              val E = Duplicate(a) * Duplicate(i) - Duplicate(c) * Duplicate(g)
              val F = -1 * (Duplicate(a) * Duplicate(h) - Duplicate(b) * Duplicate(g))
              val G = Duplicate(b) * Duplicate(f) - Duplicate(c) * Duplicate(e)
              val H = -1 * (Duplicate(a) * Duplicate(f) - Duplicate(c) * Duplicate(d))
              val I = Duplicate(a) * Duplicate(e) - Duplicate(b) * Duplicate(d)
              val det = Duplicate(a) * A + Duplicate(b) * B + Duplicate(c) * C
              IR_MatrixExpression(m.innerDatatype, 3, 3, Array(Duplicate(A) / Duplicate(det), Duplicate(D) / Duplicate(det), Duplicate(G) / Duplicate(det), Duplicate(B) / Duplicate(det), Duplicate(E) / Duplicate(det), Duplicate(H) / Duplicate(det), Duplicate(C) / Duplicate(det), Duplicate(F) / Duplicate(det), Duplicate(I) / Duplicate(det)))
            } else if (m.rows == m.columns) {
              val inv_det = 1.0 / calculateDeterminant(m)
              val tmp = IR_MatrixExpression(Some(m.innerDatatype.getOrElse(IR_RealDatatype)), m.rows, m.columns)
              for (row <- 0 until m.rows) {
                for (col <- 0 until m.columns) {
                  tmp.set(col, row, calculateMatrixOfMinorsElement(m, row, col) * math.pow(-1, row + col) * inv_det)
                }
              }
              tmp
            } else {
              x
            }
          }
          case m : IR_Expression if (m.datatype.isInstanceOf[IR_MatrixExpression]) => m
          case _                                                                   => x
        }
      } else {
        x
      }

    // FIXME: HACK to realize application functionality
    case func : IR_Function if "Application" == func.name =>
      func.returntype = IR_IntegerDatatype
      func.name = "main"
      if (!func.parameters.isEmpty)
        Logger.warning("function Application is not allowed to have parameters, omitting them")
      func.parameters = ListBuffer(IR_FunctionArgument("argc", IR_IntegerDatatype), IR_FunctionArgument("argv", IR_SpecialDatatype("char**")))
      func.allowFortranInterface = false
      func.allowInlining = false
      //if (true) {
      //func.body.append(new ConditionStatement(new MPI_IsRootProc,
      //  """#ifdef TRACK_CALLS
      //CallTracker::PrintCallStack();
      //#endif"""))
      //func.body.append("""#ifdef TRACK_CALLS
      //CallTracker::ClearCallStack();
      //#endif""")
      //}
      if (Knowledge.cuda_enabled) {
        func.body.prepend(CUDA_Init)
        func.body.append(CUDA_Finalize)
      }
      if (Knowledge.mpi_enabled) {
        func.body.prepend(MPI_Init)
        func.body.append(MPI_Finalize)
      }
      func.body.append(IR_Return(0))
      func

    // FIXME: IR_UserFunctionAccess's

    case IR_FunctionCall(IR_UserFunctionAccess("isOnBoundaryOf", _), args) =>
      val fieldAccess = args(0).asInstanceOf[IR_FieldAccess]
      IR_IsOnBoundary(fieldAccess.fieldSelection, getIndex(fieldAccess))

    case IR_FunctionCall(IR_UserFunctionAccess("isOnEastBoundaryOf", _), args) =>
      val fieldAccess = args(0).asInstanceOf[IR_FieldAccess]
      IR_IsOnSpecBoundary(fieldAccess.fieldSelection, DefaultNeighbors.getNeigh(Array(1, 0, 0)), getIndex(fieldAccess))

    case IR_FunctionCall(IR_UserFunctionAccess("isOnWestBoundaryOf", _), args) =>
      val fieldAccess = args(0).asInstanceOf[IR_FieldAccess]
      IR_IsOnSpecBoundary(fieldAccess.fieldSelection, DefaultNeighbors.getNeigh(Array(-1, 0, 0)), getIndex(fieldAccess))

    case IR_FunctionCall(IR_UserFunctionAccess("isOnNorthBoundaryOf", _), args) =>
      val fieldAccess = args(0).asInstanceOf[IR_FieldAccess]
      IR_IsOnSpecBoundary(fieldAccess.fieldSelection, DefaultNeighbors.getNeigh(Array(0, 1, 0)), getIndex(fieldAccess))

    case IR_FunctionCall(IR_UserFunctionAccess("isOnSouthBoundaryOf", _), args) =>
      val fieldAccess = args(0).asInstanceOf[IR_FieldAccess]
      IR_IsOnSpecBoundary(fieldAccess.fieldSelection, DefaultNeighbors.getNeigh(Array(0, -1, 0)), getIndex(fieldAccess))

    case IR_FunctionCall(IR_UserFunctionAccess("isOnTopBoundaryOf", _), args) =>
      val fieldAccess = args(0).asInstanceOf[IR_FieldAccess]
      IR_IsOnSpecBoundary(fieldAccess.fieldSelection, DefaultNeighbors.getNeigh(Array(0, 0, 1)), getIndex(fieldAccess))

    case IR_FunctionCall(IR_UserFunctionAccess("isOnBottomBoundaryOf", _), args) =>
      val fieldAccess = args(0).asInstanceOf[IR_FieldAccess]
      IR_IsOnSpecBoundary(fieldAccess.fieldSelection, DefaultNeighbors.getNeigh(Array(0, 0, -1)), getIndex(fieldAccess))

    case IR_ElementwiseAddition(left, right)       => IR_FunctionCall("elementwiseAdd", ListBuffer(left, right))
    case IR_ElementwiseSubtraction(left, right)    => IR_FunctionCall("elementwiseSub", ListBuffer(left, right))
    case IR_ElementwiseMultiplication(left, right) => IR_FunctionCall("elementwiseMul", ListBuffer(left, right))
    case IR_ElementwiseDivision(left, right)       => IR_FunctionCall("elementwiseDiv", ListBuffer(left, right))
    case IR_ElementwiseModulo(left, right)         => IR_FunctionCall("elementwiseMod", ListBuffer(left, right))
    // FIXME: IR_UserFunctionAccess
    case IR_FunctionCall(IR_UserFunctionAccess("dot", _), args) => IR_FunctionCall("dotProduct", args)

    case IR_ExpressionStatement(IR_FunctionCall(IR_UserFunctionAccess("readImage", _), args)) =>
      if (args.size != 2 || !args(0).isInstanceOf[IR_FieldAccess]) {
        Logger.warn("Malformed call to readImage; usage: readImage ( field, \"filename\" )")
        IR_NullStatement
      } else {
        val field = args(0).asInstanceOf[IR_FieldAccess]
        val filename = args(1).asInstanceOf[IR_StringConstant].value

        val stmts = ListBuffer[IR_Statement]()

        //stmts += IR_FunctionCall(IR_UserFunctionAccess("cimg_library::CImg< double > imageIn")
        stmts += HACK_IR_Native("cimg_library::CImg< double > imageIn ( \"" + filename + "\" )")
        stmts += IR_LoopOverPoints(field.fieldSelection.field,
          IR_Assignment(field, HACK_IR_Native("*imageIn.data(x,y)")))

        IR_Scope(stmts)
      }

    case IR_ExpressionStatement(IR_FunctionCall(IR_UserFunctionAccess("writeImage", _), args)) =>
      if (args.size != 2 || !args(0).isInstanceOf[IR_FieldAccess]) {
        Logger.warn("Malformed call to writeImage; usage: writeImage ( field, \"filename\" )")
        IR_NullStatement
      } else {
        val field = args(0).asInstanceOf[IR_FieldAccess]
        val fieldLayout = field.fieldSelection.field.fieldLayout
        val numPoints = (0 until fieldLayout.numDimsGrid).map(dim =>
          fieldLayout.layoutsPerDim(dim).numDupLayersLeft + fieldLayout.layoutsPerDim(dim).numInnerLayers + fieldLayout.layoutsPerDim(dim).numDupLayersRight)
        val filename = args(1)//.asInstanceOf[IR_StringConstant].value

        val stmts = ListBuffer[IR_Statement]()

        stmts += HACK_IR_Native("cimg_library::CImg< double > imageOut ( " + numPoints.mkString(", ") + " )")
        stmts += IR_LoopOverPoints(field.fieldSelection.field,
          IR_Assignment(HACK_IR_Native("*imageOut.data(x,y)"), field))
        filename match {
          case va : IR_VariableAccess => stmts += IR_MemberFunctionCall("imageOut", "save", IR_MemberFunctionCall(va, "c_str"))
          case other                  => stmts += IR_MemberFunctionCall("imageOut", "save", other)
        }
        //stmts += HACK_IR_Native("imageOut.save( \"" + filename.value + "\" )")

        IR_Scope(stmts)
      }

    case IR_ExpressionStatement(IR_FunctionCall(IR_UserFunctionAccess("writeMappedImage", _), args)) =>
      if (args.size != 2 || !args(0).isInstanceOf[IR_FieldAccess]) {
        Logger.warn("Malformed call to writeMappedImage; usage: writeMappedImage ( field, \"filename\" )")
        IR_NullStatement
      } else {
        val field = args(0).asInstanceOf[IR_FieldAccess]
        val fieldLayout = field.fieldSelection.field.fieldLayout
        var numPoints = (0 until fieldLayout.numDimsGrid).map(dim =>
          fieldLayout.layoutsPerDim(dim).numDupLayersLeft + fieldLayout.layoutsPerDim(dim).numInnerLayers + fieldLayout.layoutsPerDim(dim).numDupLayersRight).toList
        val filename = args(1)//.asInstanceOf[IR_StringConstant].value

        val stmts = ListBuffer[IR_Statement]()

        while (numPoints.length < 3) numPoints :+= 1
        // add color channels
        numPoints :+= 3

        stmts += HACK_IR_Native("cimg_library::CImg< double > imageOut ( " + numPoints.mkString(", ") + ", 1. )")
        stmts += IR_LoopOverPoints(field.fieldSelection.field,
          IR_Assignment(HACK_IR_Native("*imageOut.data(x,y,0,0)"), 360.0 * field))

        stmts += IR_MemberFunctionCall("imageOut", "HSVtoRGB")
        filename match {
          case va : IR_VariableAccess => stmts += IR_MemberFunctionCall("imageOut", "save", IR_MemberFunctionCall(va, "c_str"))
          case other                  => stmts += IR_MemberFunctionCall("imageOut", "save", other)
        }
        //stmts += HACK_IR_Native("imageOut.save( \"" + filename.value + "\" )")

        IR_Scope(stmts)
      }

    case IR_ExpressionStatement(IR_FunctionCall(IR_UserFunctionAccess("showImage", _), args)) =>
      if (0 == args.size || !args.map(_.isInstanceOf[IR_FieldAccess]).reduce(_ && _)) {
        Logger.warn("Malformed call to showImage; usage: showImage ( field.* )")
        IR_NullStatement
      } else {
        val fields = args.map(_.asInstanceOf[IR_FieldAccess])
        val fieldLayouts = fields.map(_.fieldSelection.field.fieldLayout)
        val numPoints = fieldLayouts.map(fieldLayout => (0 until fieldLayout.numDimsGrid).map(dim =>
          fieldLayout.layoutsPerDim(dim).numDupLayersLeft + fieldLayout.layoutsPerDim(dim).numInnerLayers + fieldLayout.layoutsPerDim(dim).numDupLayersRight))

        val tmpImgs = fields.indices.map(i => s"imageShow$i")
        val displays = fields.indices.map(i => s"cImgDisp$i")

        val stmts = ListBuffer[IR_Statement]()

        for (i <- fields.indices) {
          stmts += HACK_IR_Native("cimg_library::CImg< double > " + tmpImgs(i) + " ( " + numPoints(i).mkString(", ") + " )")
          stmts += IR_LoopOverPoints(fields(i).fieldSelection.field,
            IR_Assignment(HACK_IR_Native("*" + tmpImgs(i) + ".data(x,y)"), fields(i)))
          val dispName = fields(i).fieldSelection.field.name + "@" + fields(i).fieldSelection.field.level
          stmts += HACK_IR_Native("cimg_library::CImgDisplay " + displays(i) + "(" + tmpImgs(i) + ", \"" + dispName + "\")")
        }
        stmts += IR_WhileLoop(fields.indices.map(i => IR_Negation(IR_MemberFunctionCall(displays(i), "is_closed")) : IR_Expression).reduceLeft(IR_OrOr),
          fields.indices.map(i => IR_MemberFunctionCall(displays(i), "wait") : IR_Statement).to[ListBuffer])

        IR_Scope(stmts)
      }
  })
}
