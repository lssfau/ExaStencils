package exastencils.visualization.ir.visit

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.domain.ir._
import exastencils.parallelization.api.mpi._
import exastencils.visualization.ir.visit.IR_VisItGlobals._


case class IR_VisItInitialization() extends IR_VisItFuturePlainFunction {

  import exastencils.visualization.ir.visit.IR_VisItUtil._

  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()
    val cwd = IR_VariableAccess("cwd", IR_PointerDatatype(IR_CharDatatype))

    val getCWD = if (Platform.targetCompiler == "MSVC") {
      IR_ExternalFunctionReference("_getcwd")
    } else {
      IR_ExternalFunctionReference("getcwd")
    }

    // allocate coordinate arrays
    for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
      // regular coordinates for rect mesh, not used in 1d case
      if (useRectMesh()) {
        for (coords <- coordsArrays.distinct) {
          for (dim <- 0 until Knowledge.dimensionality) {
            val arrayIndices = ArrayBuffer[Int]()
            if (Knowledge.dimensionality > 1) arrayIndices += dim
            if (Knowledge.numLevels > 1) arrayIndices += level - Knowledge.minLevel

            val coordsIdx = coordsArrays.indexOf(coords)
            val coordsAccess = if (arrayIndices.isEmpty)
              coords
            else if (arrayIndices.length == 1)
              IR_ArrayAccess(coords, arrayIndices.head)
            else
              IR_MultiDimArrayAccess(coords, IR_ExpressionIndex(arrayIndices.toArray))

            fctBody += IR_ArrayAllocation(coordsAccess, IR_RealDatatype,
              (Knowledge.domain_fragmentLengthAsVec(dim) * Knowledge.domain_rect_numFragsPerBlockAsVec(dim) * (1 << level)) + isNodalInDim(coordsIdx)(dim))
          }
        }
      }

      // curve coordinates, only used in 1d/2d
      if (useCurveMesh()) {
        for (curveCoords <- curveCoordsArrays.distinct) {
          for (dim <- 0 until Knowledge.dimensionality) {
            val arrayIndices = ArrayBuffer[Int]()
            if (Knowledge.dimensionality > 1) arrayIndices += dim
            if (Knowledge.numLevels > 1) arrayIndices += level - Knowledge.minLevel

            val curCoordsCurve = curveCoordsArrays.indexOf(curveCoords)
            val curveCoordsAccess = if (arrayIndices.isEmpty)
              curveCoords
            else if (arrayIndices.length == 1)
              IR_ArrayAccess(curveCoords, arrayIndices.head)
            else
              IR_MultiDimArrayAccess(curveCoords, IR_ExpressionIndex(arrayIndices.toArray))

            // coordinates for every point within the n-dim domain
            fctBody += IR_ArrayAllocation(
              curveCoordsAccess, IR_RealDatatype,
              (0 until Knowledge.dimensionality).map(d => {
                (Knowledge.domain_fragmentLengthAsVec(d) * Knowledge.domain_rect_numFragsPerBlockAsVec(d) * (1 << level)) + isNodalInDimCurve(curCoordsCurve)(d)
              }).product
            )
          }
        }
      }
    }

    // put Knowledge discr_h* (* = x/y/z) into one array
    val discrAsVec = Array(Knowledge.discr_hx, Knowledge.discr_hy, Knowledge.discr_hz)
    val discrPerDim : Array[Array[Double]] = (0 until Knowledge.dimensionality).map(dim => {
      (Knowledge.minLevel to Knowledge.maxLevel).map(level => discrAsVec(dim)(level - Knowledge.minLevel)).toArray
    }).toArray

    for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
      for (coords <- coordsArrays.distinct) {
        val coordsIdx = coordsArrays.indexOf(coords)

        // get number of points per dimension
        val numPointsDim = (0 until Knowledge.dimensionality).map(d => Knowledge.domain_fragmentLengthAsVec(d) * (1 << level) + isNodalInDim(coordsIdx)(d))

        // get current position of point "i" in the current block/fragment
        val stepSizeDim = Array.ofDim[IR_Expression](Knowledge.dimensionality)
        for (dim <- 0 until Knowledge.dimensionality) {
          // compute offset towards cell-center
          val iterator = if (isNodalInDim(coordsIdx)(dim) != 1) IR_FieldIteratorAccess(0) + 0.5 else IR_FieldIteratorAccess(0)

          // compute offset towards point "i" on current block/fragment
          if (Knowledge.mpi_enabled || Knowledge.domain_numFragmentsPerBlock > 1) {
            stepSizeDim(dim) = IR_IV_FragmentPositionBegin(dim) + iterator * discrPerDim(dim)(level - Knowledge.minLevel)
          } else {
            stepSizeDim(dim) = iterator * discrPerDim(dim)(level - Knowledge.minLevel)
          }
        }

        // assign coordinate values, 1d cases only need curve coords
        if (useRectMesh()) {
          val forBody = ListBuffer[IR_Statement]()

          for (dim <- 0 until Knowledge.dimensionality) {
            val iter = IR_FieldIteratorAccess(0)

            val arrayIndices = ArrayBuffer[IR_Expression]()
            if (Knowledge.domain_numFragmentsPerBlock > 1) {
              arrayIndices += iter +
                (numPointsDim(dim) - isNodalInDim(coordsIdx)(dim)) * (IR_IV_FragmentIndex(dim) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(dim))
            } else {
              arrayIndices += iter
            }
            if (Knowledge.dimensionality > 1) arrayIndices += dim
            if (Knowledge.numLevels > 1) arrayIndices += level - Knowledge.minLevel

            val coordsAccess = if (arrayIndices.length == 1) {
              IR_ArrayAccess(coords, arrayIndices.head)
            } else {
              IR_MultiDimArrayAccess(coords, IR_ExpressionIndex(arrayIndices.toArray))
            }

            forBody += IR_ForLoop(
              IR_VariableDeclaration(iter, IR_IntegerConstant(0)), iter < numPointsDim(dim), IR_PreIncrement(iter),
              IR_Assignment(coordsAccess, stepSizeDim(dim)))
          }

          fctBody += IR_LoopOverFragments(forBody)
        }
      }


      // curve coords for 1d and 2d problems
      if (useCurveMesh()) {
        for (curveCoords <- curveCoordsArrays.distinct) {
          val coordsCurveIdx = curveCoordsArrays.indexOf(curveCoords)

          // get number of points per dimension
          val numPointsDimCurve = (0 until Knowledge.dimensionality).map(d => Knowledge.domain_fragmentLengthAsVec(d) * (1 << level) + isNodalInDimCurve(coordsCurveIdx)(d))

          // get current position of point "i" in the current block/fragment
          val stepSizeDimCurve = Array.ofDim[IR_Expression](Knowledge.dimensionality) // adapt step size for initialization of curve coordinates
          for (dim <- 0 until Knowledge.dimensionality) {
            // compute offset towards cell-center
            val iteratorCurve = if (isNodalInDimCurve(coordsCurveIdx)(dim) != 1) IR_FieldIteratorAccess(dim) + 0.5 else IR_FieldIteratorAccess(dim)

            // compute offset towards point "i" on current block/fragment
            if (Knowledge.mpi_enabled || Knowledge.domain_numFragmentsPerBlock > 1) {
              stepSizeDimCurve(dim) = IR_IV_FragmentPositionBegin(dim) + iteratorCurve * discrPerDim(dim)(level - Knowledge.minLevel)
            } else {
              stepSizeDimCurve(dim) = iteratorCurve * discrPerDim(dim)(level - Knowledge.minLevel)
            }
          }

          // offset to the current fragment
          val fragOffset = (0 until Knowledge.dimensionality).map(d => {
            if (Knowledge.domain_rect_numFragsPerBlockAsVec(d) <= 1)
              IR_IntegerConstant(0)
            else
              (numPointsDimCurve(d) - isNodalInDimCurve(coordsCurveIdx)(d)) * (IR_IV_FragmentIndex(d) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(d))
          }).toArray

          for (dim <- 0 until Knowledge.dimensionality) {
            val arrayIndicesCurve = ArrayBuffer[IR_Expression]()
            arrayIndicesCurve += (0 until Knowledge.dimensionality).map(d => {
              (IR_FieldIteratorAccess(d) + fragOffset(d)) *
                (0 until d).map(dd => {
                  Knowledge.domain_rect_numFragsPerBlockAsVec(dd) *
                    (numPointsDimCurve(dd) - isNodalInDimCurve(coordsCurveIdx)(dd)) + isNodalInDimCurve(coordsCurveIdx)(dd)
                }).product : IR_Expression
            }).reduce(_ + _)
            if (Knowledge.dimensionality > 1) arrayIndicesCurve += dim
            if (Knowledge.numLevels > 1) arrayIndicesCurve += level - Knowledge.minLevel

            val curveCoordsAccess = if (arrayIndicesCurve.length == 1) {
              IR_ArrayAccess(curveCoords, arrayIndicesCurve.head)
            } else {
              IR_MultiDimArrayAccess(curveCoords, IR_ExpressionIndex(arrayIndicesCurve.toArray))
            }

            // assign curve coordinate values
            var forBody = ListBuffer[IR_Statement]()
            forBody += IR_Assignment(curveCoordsAccess, stepSizeDimCurve(dim))
            for (d <- 0 until Knowledge.dimensionality) {
              forBody = ListBuffer(
                IR_ForLoop(IR_VariableDeclaration(IR_FieldIteratorAccess(d), 0), IR_FieldIteratorAccess(d) < numPointsDimCurve(d), IR_PreIncrement(IR_FieldIteratorAccess(d)),
                  forBody))
            }

            fctBody += IR_LoopOverFragments(forBody)
          }
        }
      }
    }

    // set path(top level directory where visit is installed) to select a certain visit version
    val strDecl = IR_VariableDeclaration(IR_StringDatatype, "str", IR_FunctionCall(IR_ExternalFunctionReference("std::getenv"), IR_StringConstant("VISIT_HOME")))
    val str = IR_VariableAccess(strDecl)
    val pathDecl = IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "path")
    val path = IR_VariableAccess(pathDecl)
    fctBody += strDecl
    fctBody += IR_IfCondition(
      IR_MemberFunctionCall(str, "empty") Neq IR_BooleanConstant(true),
      ListBuffer[IR_Statement](
        pathDecl,
        IR_ArrayAllocation(path, IR_CharDatatype, IR_MemberFunctionCall(str, "size") + IR_IntegerConstant(1)),
        IR_FunctionCall(
          IR_ExternalFunctionReference("std::copy"),
          IR_MemberFunctionCall(str, "begin"),
          IR_MemberFunctionCall(str, "end"),
          path
        ),
        IR_Assignment(IR_ArrayAccess(path, IR_MemberFunctionCall(str, "size")), IR_Native("\'\\0\'")),
        IR_FunctionCall(IR_ExternalFunctionReference("VisItSetDirectory"), path),
        IR_ArrayFree(path)
      )
    )

    // name of the sim2 file
    val simName = ListBuffer[String](Knowledge.dimensionality + "d_" + Knowledge.discr_type)
    if (Knowledge.grid_isStaggered) simName += "staggered"
    simName += Knowledge.domain_numBlocks + "Blocks"
    simName += Knowledge.domain_numFragmentsPerBlock + "Frags"

    // mandatory functions for VisIt (setup environment variables, parallel initializations)
    if (Knowledge.mpi_enabled) {
      val fn = IR_VariableAccess("fn", IR_ArrayDatatype(IR_CharDatatype, 1000))
      fctBody += IR_VariableDeclaration(fn)
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("sprintf"), fn, IR_StringConstant("trace_%d.txt"), MPI_IV_MpiRank)
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItOpenTraceFile"), fn)

      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetBroadcastIntFunction"), IR_VisItBroadcastIntCallback().name)
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetBroadcastStringFunction"), IR_VisItBroadcastStringCallback().name)

      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetParallel"), IR_BooleanConstant(true))
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetParallelRank"), MPI_IV_MpiRank)

      val env = IR_VariableAccess("env", IR_PointerDatatype(IR_CharDatatype))
      fctBody += IR_VariableDeclaration(env, nullptr)
      fctBody += IR_IfCondition(
        MPI_IsRootProc(),
        IR_Assignment(env, IR_FunctionCall(IR_ExternalFunctionReference("VisItGetEnvironment")))
      )
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetupEnvironment2"), env)
      fctBody += IR_IfCondition(
        env Neq nullptr,
        IR_FunctionCall(IR_ExternalFunctionReference("free"), env)
      )

      fctBody += IR_IfCondition(
        MPI_IsRootProc.apply(),
        ListBuffer[IR_Statement](
          IR_VariableDeclaration(cwd, nullptr),
          IR_ArrayAllocation(cwd, IR_CharDatatype, IR_IntegerConstant(1000)),
          IR_Assignment(cwd, IR_FunctionCall(getCWD, cwd, IR_IntegerConstant(1000))),
          IR_FunctionCall(IR_ExternalFunctionReference("VisItInitializeSocketAndDumpSimFile"), IR_StringConstant(simName.head),
            IR_StringConstant(simName.tail.mkString("_")), cwd, nullptr, nullptr, nullptr),
          IR_ArrayFree(cwd)
        )
      )
    } else {
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItOpenTraceFile"), IR_StringConstant("trace.txt"))
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetupEnvironment"))
      fctBody += IR_VariableDeclaration(cwd, nullptr)
      fctBody += IR_ArrayAllocation(cwd, IR_CharDatatype, IR_IntegerConstant(1000))
      fctBody += IR_Assignment(cwd, IR_FunctionCall(getCWD, cwd, IR_IntegerConstant(1000)))
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItInitializeSocketAndDumpSimFile"),
        IR_StringConstant(simName.head), IR_StringConstant(simName.tail.mkString("_")), cwd, nullptr, nullptr, nullptr)
      fctBody += IR_ArrayFree(cwd)
    }

    IR_PlainFunction(
      name,
      IR_UnitDatatype,
      fctBody
    )
  }

  override def name : String = "visit_init"
}
