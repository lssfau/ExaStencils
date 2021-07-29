package exastencils.visualization.ir.visit

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.domain.ir._
import exastencils.parallelization.api.mpi._

case class IR_VisItInitialization() extends IR_FuturePlainVisItFunction {

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
      for (coordsDecl <- coordsArrays.distinct) {
        for (dim <- 0 until Knowledge.dimensionality) {
          val arrayIndices = ArrayBuffer[Int]()
          if (Knowledge.dimensionality > 1) arrayIndices += dim
          if (Knowledge.numLevels > 1) arrayIndices += level - Knowledge.minLevel

          val curCoords = coordsArrays.indexOf(coordsDecl)
          val coordsAccess = if (arrayIndices.isEmpty)
            IR_VariableAccess(coordsDecl)
          else if (arrayIndices.length == 1)
            IR_ArrayAccess(IR_VariableAccess(coordsDecl), arrayIndices.head)
          else
            IR_MultiDimArrayAccess(IR_VariableAccess(coordsDecl), IR_ExpressionIndex(arrayIndices.toArray))

          // regular coordinates for rect mesh, not used in 1d case
          if (Knowledge.dimensionality > 1) {
            fctBody += IR_ArrayAllocation(coordsAccess, IR_RealDatatype,
              (Knowledge.domain_fragmentLengthAsVec(dim) * Knowledge.domain_rect_numFragsPerBlockAsVec(dim) * (1 << level)) + isNodalInDim(curCoords)(dim))
          }

          // curve coordinates
          if (Knowledge.dimensionality < 3) {
            val curveCoordsDecl = IR_VariableDeclaration(coordsDecl.datatype, "curve_" + coordsDecl.name)
            val curCoordsCurve = curveCoordsArrays.indexOf(curveCoordsDecl)
            val curveCoordsAccess = if (arrayIndices.isEmpty)
              IR_VariableAccess(curveCoordsDecl)
            else if (arrayIndices.length == 1)
              IR_ArrayAccess(IR_VariableAccess(curveCoordsDecl), arrayIndices.head)
            else
              IR_MultiDimArrayAccess(IR_VariableAccess(curveCoordsDecl), IR_ExpressionIndex(arrayIndices.toArray))

            if (Knowledge.dimensionality == 1) {
              fctBody += IR_ArrayAllocation(
                curveCoordsAccess, IR_RealDatatype,
                (Knowledge.domain_fragmentLength_x * Knowledge.domain_rect_numFragsPerBlock_x * (1 << level)) + isNodalInDimCurve(curCoordsCurve)(dim)
              )
            }
            // x and y coordinates for every point within the 2d domain
            if (Knowledge.dimensionality == 2) {
              fctBody += IR_ArrayAllocation(
                curveCoordsAccess, IR_RealDatatype,
                ((Knowledge.domain_fragmentLength_x * Knowledge.domain_rect_numFragsPerBlock_x * (1 << level)) + isNodalInDimCurve(curCoordsCurve)(0)) *
                  ((Knowledge.domain_fragmentLength_y * Knowledge.domain_rect_numFragsPerBlock_y * (1 << level)) + isNodalInDimCurve(curCoordsCurve)(1))
              )
            }
          }
        }
      }
    }

    // put Knowledge discr_h* (* = x/y/z) into one array
    val discrPerDim = Array.ofDim[Double](Knowledge.dimensionality, Knowledge.numLevels)
    for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
      discrPerDim(0)(level - Knowledge.minLevel) = Knowledge.discr_hx(level - Knowledge.minLevel)
      if (Knowledge.dimensionality > 1) discrPerDim(1)(level - Knowledge.minLevel) = Knowledge.discr_hy(level - Knowledge.minLevel)
      if (Knowledge.dimensionality > 2) discrPerDim(2)(level - Knowledge.minLevel) = Knowledge.discr_hz(level - Knowledge.minLevel)
    }

    for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
      for (coordsDecl <- coordsArrays.distinct) {
        val curveCoordsDecl = IR_VariableDeclaration(coordsDecl.datatype, "curve_" + coordsDecl.name)
        val curCoords = coordsArrays.indexOf(coordsDecl)
        val curCoordsCurve = curveCoordsArrays.indexOf(curveCoordsDecl)

        // get number of points per dimension
        val numPointsDim = (0 until Knowledge.dimensionality).map(d => Knowledge.domain_fragmentLengthAsVec(d) * (1 << level) + isNodalInDim(curCoords)(d))
        val numPointsDimCurve = (0 until Knowledge.dimensionality).map(d => Knowledge.domain_fragmentLengthAsVec(d) * (1 << level) + isNodalInDimCurve(curCoordsCurve)(d))

        // get current position of point "i" in the current block/fragment
        val stepSizeDim = Array.ofDim[IR_Expression](Knowledge.dimensionality)
        val stepSizeDimCurve = Array.ofDim[IR_Expression](Knowledge.dimensionality) // adapt step size for initialization of curve coordinates

        for (dim <- 0 until Knowledge.dimensionality) {
          // compute offset towards cell-center
          val iterator = if (isNodalInDim(curCoords)(dim) != 1) IR_FieldIteratorAccess(0) + 0.5 else IR_FieldIteratorAccess(0)
          val iteratorCurve = if (isNodalInDimCurve(curCoordsCurve)(dim) != 1) IR_FieldIteratorAccess(dim) + 0.5 else IR_FieldIteratorAccess(dim)

          // compute offset towards point "i" on current block/fragment
          if (Knowledge.mpi_enabled || Knowledge.domain_numFragmentsPerBlock > 1) {
            stepSizeDim(dim) = IR_IV_FragmentPositionBegin(dim) + iterator * discrPerDim(dim)(level - Knowledge.minLevel)
            stepSizeDimCurve(dim) = IR_IV_FragmentPositionBegin(dim) + iteratorCurve * discrPerDim(dim)(level - Knowledge.minLevel)
          } else {
            stepSizeDim(dim) = iterator * discrPerDim(dim)(level - Knowledge.minLevel)
            stepSizeDimCurve(dim) = iteratorCurve * discrPerDim(dim)(level - Knowledge.minLevel)
          }
        }

        val forBody = ListBuffer[IR_Statement]()
        for (dim <- 0 until Knowledge.dimensionality) {
          // assign coordinate values, 1d cases only need curve coords
          if (Knowledge.dimensionality > 1) {
            val arrayIndices = ArrayBuffer[IR_Expression]()
            if (Knowledge.domain_numFragmentsPerBlock > 1) {
              arrayIndices += IR_FieldIteratorAccess(0) +
                (numPointsDim(dim) - isNodalInDim(curCoords)(dim)) * (IR_IV_FragmentIndex(dim) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(dim))
            } else {
              arrayIndices += IR_FieldIteratorAccess(0)
            }
            if (Knowledge.dimensionality > 1) arrayIndices += dim
            if (Knowledge.numLevels > 1) arrayIndices += level - Knowledge.minLevel

            val coordsAccess = if (arrayIndices.length == 1) {
              IR_ArrayAccess(IR_VariableAccess(coordsDecl), arrayIndices.head)
            } else {
              IR_MultiDimArrayAccess(IR_VariableAccess(coordsDecl), IR_ExpressionIndex(arrayIndices.toArray))
            }

            forBody += IR_ForLoop(
              IR_VariableDeclaration(IR_FieldIteratorAccess(0), IR_IntegerConstant(0)), IR_FieldIteratorAccess(0) < numPointsDim(dim), IR_PreIncrement(IR_FieldIteratorAccess(0)),
              IR_Assignment(coordsAccess, stepSizeDim(dim))
            )
          }

          // curve coords for 1d and 2d problems
          if (Knowledge.dimensionality < 3) {
            // offset to the current fragment
            val fragOffset = (0 until Knowledge.dimensionality).map(d => if (Knowledge.domain_rect_numFragsPerBlockAsVec(d) <= 1) IR_IntegerConstant(0)
            else (numPointsDimCurve(d) - isNodalInDimCurve(curCoordsCurve)(d)) * (IR_IV_FragmentIndex(d) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(d))).toArray

            val arrayIndicesCurve = ArrayBuffer[IR_Expression]()
            if (Knowledge.domain_numFragmentsPerBlock > 1) {
              if (Knowledge.dimensionality == 1) {
                arrayIndicesCurve += IR_FieldIteratorAccess(0) + fragOffset(0)
              } else {
                arrayIndicesCurve +=
                  (Knowledge.domain_rect_numFragsPerBlock_x * (numPointsDimCurve(0) - isNodalInDimCurve(curCoordsCurve)(dim)) + isNodalInDimCurve(curCoordsCurve)(dim)) * (IR_FieldIteratorAccess(1) + fragOffset(1)) +
                    (IR_FieldIteratorAccess(0) + fragOffset(0))
              }
            } else {
              if (Knowledge.dimensionality == 1) {
                arrayIndicesCurve += IR_FieldIteratorAccess(0)
              } else {
                arrayIndicesCurve += numPointsDimCurve(0) * IR_FieldIteratorAccess(1) + IR_FieldIteratorAccess(0)
              }
            }
            if (Knowledge.dimensionality > 1) arrayIndicesCurve += dim
            if (Knowledge.numLevels > 1) arrayIndicesCurve += level - Knowledge.minLevel

            val curveCoordsAccess = if (arrayIndicesCurve.length == 1) {
              IR_ArrayAccess(IR_VariableAccess(curveCoordsDecl), arrayIndicesCurve.head)
            } else {
              IR_MultiDimArrayAccess(IR_VariableAccess(curveCoordsDecl), IR_ExpressionIndex(arrayIndicesCurve.toArray))
            }

            // assign curve coordinate values
            if (Knowledge.dimensionality == 1) {
              forBody += IR_ForLoop(
                IR_VariableDeclaration(IR_FieldIteratorAccess(0), IR_IntegerConstant(0)), IR_FieldIteratorAccess(0) < numPointsDimCurve(0), IR_PreIncrement(IR_FieldIteratorAccess(0)),
                IR_Assignment(curveCoordsAccess, numPointsDimCurve(dim))
              )
            } else {
              forBody += IR_ForLoop(
                IR_VariableDeclaration(IR_FieldIteratorAccess(1), IR_IntegerConstant(0)), IR_FieldIteratorAccess(1) < numPointsDimCurve(1), IR_PreIncrement(IR_FieldIteratorAccess(1)),
                IR_ForLoop(
                  IR_VariableDeclaration(IR_FieldIteratorAccess(0), IR_IntegerConstant(0)), IR_FieldIteratorAccess(0) < numPointsDimCurve(0), IR_PreIncrement(IR_FieldIteratorAccess(0)),
                  IR_Assignment(curveCoordsAccess, numPointsDimCurve(dim))
                )
              )
            }
          }
        }

        fctBody += IR_LoopOverFragments(
          forBody
        )
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
