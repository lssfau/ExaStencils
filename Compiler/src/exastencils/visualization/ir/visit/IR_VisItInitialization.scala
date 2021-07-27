package exastencils.visualization.ir.visit

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.domain.ir._
import exastencils.parallelization.api.mpi._


case class IR_VisItInitialization() extends IR_FuturePlainFunction {

  import exastencils.visualization.ir.visit.IR_VisItUtil._

  override def generateFct() : IR_PlainFunction =  {
    val fctBody = ListBuffer[IR_Statement]()
    val cwd_decl = IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "cwd", nullptr)

    val getCWD = if (Platform.targetCompiler == "MSVC") {
      IR_ExternalFunctionReference("_getcwd")
    } else {
      IR_ExternalFunctionReference("getcwd")
    }

    // allocate coordinate arrays
    for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
      for (coords_decl <- coords_arrays.distinct) {
        for (dim <- 0 until Knowledge.dimensionality) {
          val array_indices = ArrayBuffer[Int]()
          if (Knowledge.dimensionality > 1) array_indices += dim
          if (Knowledge.numLevels > 1) array_indices += level - Knowledge.minLevel

          val curCoords = coords_arrays.indexOf(coords_decl)
          val coords_access = if (array_indices.isEmpty) {
            IR_VariableAccess(coords_decl)
          }
          else if (array_indices.length == 1) {
            IR_ArrayAccess(IR_VariableAccess(coords_decl), array_indices.head)
          }
          else IR_MultiDimArrayAccess(IR_VariableAccess(coords_decl), IR_ExpressionIndex(array_indices.toArray))

          // regular coordinates for rect mesh, not used in 1d case
          if (Knowledge.dimensionality > 1) {
            fctBody += IR_ArrayAllocation(coords_access, IR_RealDatatype,
              (Knowledge.domain_fragmentLengthAsVec(dim) * Knowledge.domain_rect_numFragsPerBlockAsVec(dim) * (1 << level)) + isNodalInDim(curCoords)(dim))
          }

          // curve coordinates
          if (Knowledge.dimensionality < 3) {
            val curveCoords_decl = IR_VariableDeclaration(coords_decl.datatype, "curve_" + coords_decl.name)
            val curCoords_curve = curveCoords_arrays.indexOf(curveCoords_decl)
            val curveCoords_access = if (array_indices.isEmpty) {
              IR_VariableAccess(curveCoords_decl)
            }
            else if (array_indices.length == 1) {
              IR_ArrayAccess(IR_VariableAccess(curveCoords_decl), array_indices.head)
            }
            else IR_MultiDimArrayAccess(IR_VariableAccess(curveCoords_decl), IR_ExpressionIndex(array_indices.toArray))

            if (Knowledge.dimensionality == 1) {
              fctBody += IR_ArrayAllocation(
                curveCoords_access, IR_RealDatatype,
                (Knowledge.domain_fragmentLength_x * Knowledge.domain_rect_numFragsPerBlock_x * (1 << level)) + isNodalInDim_curve(curCoords_curve)(dim)
              )
            }
            // x and y coordinates for every point within the 2d domain
            if (Knowledge.dimensionality == 2) {
              fctBody += IR_ArrayAllocation(
                curveCoords_access, IR_RealDatatype,
                ((Knowledge.domain_fragmentLength_x * Knowledge.domain_rect_numFragsPerBlock_x * (1 << level)) + isNodalInDim_curve(curCoords_curve)(0)) *
                  ((Knowledge.domain_fragmentLength_y * Knowledge.domain_rect_numFragsPerBlock_y * (1 << level)) + isNodalInDim_curve(curCoords_curve)(1))
              )
            }
          }
        }
      }
    }

    // put Knowledge discr_h* (* = x/y/z) into one array
    val discr_perDim = Array.ofDim[Double](Knowledge.dimensionality, Knowledge.numLevels)
    for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
      discr_perDim(0)(level - Knowledge.minLevel) = Knowledge.discr_hx(level - Knowledge.minLevel)
      if (Knowledge.dimensionality > 1) discr_perDim(1)(level - Knowledge.minLevel) = Knowledge.discr_hy(level - Knowledge.minLevel)
      if (Knowledge.dimensionality > 2) discr_perDim(2)(level - Knowledge.minLevel) = Knowledge.discr_hz(level - Knowledge.minLevel)
    }

    for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
      for (coords_decl <- coords_arrays.distinct) {
        val curveCoords_decl = IR_VariableDeclaration(coords_decl.datatype, "curve_" + coords_decl.name)
        val curCoords = coords_arrays.indexOf(coords_decl)
        val curCoords_curve = curveCoords_arrays.indexOf(curveCoords_decl)

        // get number of points per dimension
        val numPointsDim = (0 until Knowledge.dimensionality).map(d => Knowledge.domain_fragmentLengthAsVec(d) * (1 << level) + isNodalInDim(curCoords)(d))
        val numPointsDim_curve = (0 until Knowledge.dimensionality).map(d => Knowledge.domain_fragmentLengthAsVec(d) * (1 << level) + isNodalInDim_curve(curCoords_curve)(d))

        // get current position of point "i" in the current block/fragment
        val stepSizeDim = Array.ofDim[IR_Expression](Knowledge.dimensionality)
        val stepSizeDim_curve = Array.ofDim[IR_Expression](Knowledge.dimensionality) // adapt step size for initialization of curve coordinates

        for (dim <- 0 until Knowledge.dimensionality) {
          // compute offset towards cell-center
          val iterator = if (isNodalInDim(curCoords)(dim) != 1) IR_FieldIteratorAccess(0) + 0.5 else IR_FieldIteratorAccess(0)
          val iterator_curve = if (isNodalInDim_curve(curCoords_curve)(dim) != 1) IR_FieldIteratorAccess(dim) + 0.5 else IR_FieldIteratorAccess(dim)

          // compute offset towards point "i" on current block/fragment
          if (Knowledge.mpi_enabled || Knowledge.domain_numFragmentsPerBlock > 1) {
            stepSizeDim(dim) = IR_IV_FragmentPositionBegin(dim) + iterator * discr_perDim(dim)(level - Knowledge.minLevel)
            stepSizeDim_curve(dim) = IR_IV_FragmentPositionBegin(dim) + iterator_curve * discr_perDim(dim)(level - Knowledge.minLevel)
          } else {
            stepSizeDim(dim) = iterator * discr_perDim(dim)(level - Knowledge.minLevel)
            stepSizeDim_curve(dim) = iterator_curve * discr_perDim(dim)(level - Knowledge.minLevel)
          }
        }

        val forBody = ListBuffer[IR_Statement]()
        for (dim <- 0 until Knowledge.dimensionality) {
          // assign coordinate values, 1d cases only need curve coords
          if (Knowledge.dimensionality > 1) {
            val array_indices = ArrayBuffer[IR_Expression]()
            if (Knowledge.domain_numFragmentsPerBlock > 1) {
              array_indices += IR_FieldIteratorAccess(0) +
                (numPointsDim(dim) - isNodalInDim(curCoords)(dim)) * (IR_IV_FragmentIndex(dim) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(dim))
            } else {
              array_indices += IR_FieldIteratorAccess(0)
            }
            if (Knowledge.dimensionality > 1) array_indices += dim
            if (Knowledge.numLevels > 1) array_indices += level - Knowledge.minLevel

            val coords_access = if (array_indices.length == 1) {
              IR_ArrayAccess(IR_VariableAccess(coords_decl), array_indices.head)
            } else {
              IR_MultiDimArrayAccess(IR_VariableAccess(coords_decl), IR_ExpressionIndex(array_indices.toArray))
            }

            forBody += IR_ForLoop(
              IR_VariableDeclaration(IR_FieldIteratorAccess(0), IR_IntegerConstant(0)), IR_FieldIteratorAccess(0) < numPointsDim(dim), IR_PreIncrement(IR_FieldIteratorAccess(0)),
              IR_Assignment(coords_access, stepSizeDim(dim))
            )
          }

          // curve coords for 1d and 2d problems
          if (Knowledge.dimensionality < 3) {
            // offset to the current fragment
            val fragOffset = (0 until Knowledge.dimensionality).map(d => if (Knowledge.domain_rect_numFragsPerBlockAsVec(d) <= 1) IR_IntegerConstant(0)
            else (numPointsDim_curve(d) - isNodalInDim_curve(curCoords_curve)(d)) * (IR_IV_FragmentIndex(d) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(d))).toArray

            val arrayIndices_curve = ArrayBuffer[IR_Expression]()
            if (Knowledge.domain_numFragmentsPerBlock > 1) {
              if (Knowledge.dimensionality == 1) {
                arrayIndices_curve += IR_FieldIteratorAccess(0) + fragOffset(0)
              } else {
                arrayIndices_curve +=
                  (Knowledge.domain_rect_numFragsPerBlock_x * (numPointsDim_curve(0) - isNodalInDim_curve(curCoords_curve)(dim)) + isNodalInDim_curve(curCoords_curve)(dim)) * (IR_FieldIteratorAccess(1) + fragOffset(1)) +
                    (IR_FieldIteratorAccess(0) + fragOffset(0))
              }
            } else {
              if (Knowledge.dimensionality == 1) {
                arrayIndices_curve += IR_FieldIteratorAccess(0)
              } else {
                arrayIndices_curve += numPointsDim_curve(0) * IR_FieldIteratorAccess(1) + IR_FieldIteratorAccess(0)
              }
            }
            if (Knowledge.dimensionality > 1) arrayIndices_curve += dim
            if (Knowledge.numLevels > 1) arrayIndices_curve += level - Knowledge.minLevel

            val curveCoords_access = if (arrayIndices_curve.length == 1) {
              IR_ArrayAccess(IR_VariableAccess(curveCoords_decl), arrayIndices_curve.head)
            } else {
              IR_MultiDimArrayAccess(IR_VariableAccess(curveCoords_decl), IR_ExpressionIndex(arrayIndices_curve.toArray))
            }

            // assign curve coordinate values
            if (Knowledge.dimensionality == 1) {
              forBody += IR_ForLoop(
                IR_VariableDeclaration(IR_FieldIteratorAccess(0), IR_IntegerConstant(0)), IR_FieldIteratorAccess(0) < numPointsDim_curve(0), IR_PreIncrement(IR_FieldIteratorAccess(0)),
                IR_Assignment(curveCoords_access, stepSizeDim_curve(dim))
              )
            } else {
              forBody += IR_ForLoop(
                IR_VariableDeclaration(IR_FieldIteratorAccess(1), IR_IntegerConstant(0)), IR_FieldIteratorAccess(1) < numPointsDim_curve(1), IR_PreIncrement(IR_FieldIteratorAccess(1)),
                IR_ForLoop(
                  IR_VariableDeclaration(IR_FieldIteratorAccess(0), IR_IntegerConstant(0)), IR_FieldIteratorAccess(0) < numPointsDim_curve(0), IR_PreIncrement(IR_FieldIteratorAccess(0)),
                  IR_Assignment(curveCoords_access, stepSizeDim_curve(dim))
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
    val str_decl = IR_VariableDeclaration(IR_StringDatatype, "str", IR_FunctionCall(IR_ExternalFunctionReference("std::getenv"), IR_StringConstant("VISIT_HOME")))
    val path_decl = IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "path")
    fctBody += str_decl
    fctBody += IR_IfCondition(
      IR_MemberFunctionCall(IR_VariableAccess(str_decl), "empty") Neq IR_BooleanConstant(true),
      ListBuffer[IR_Statement](
        path_decl,
        IR_ArrayAllocation(IR_VariableAccess(path_decl), IR_CharDatatype, IR_MemberFunctionCall(IR_VariableAccess(str_decl), "size") + IR_IntegerConstant(1)),
        IR_FunctionCall(
          IR_ExternalFunctionReference("std::copy"),
          IR_MemberFunctionCall(IR_VariableAccess(str_decl), "begin"),
          IR_MemberFunctionCall(IR_VariableAccess(str_decl), "end"),
          IR_VariableAccess(path_decl)
        ),
        IR_Assignment(IR_ArrayAccess(IR_VariableAccess(path_decl), IR_MemberFunctionCall(IR_VariableAccess(str_decl), "size")), IR_Native("\'\\0\'")),
        IR_FunctionCall(IR_ExternalFunctionReference("VisItSetDirectory"), IR_VariableAccess(path_decl)),
        IR_ArrayFree(IR_VariableAccess(path_decl))
      )
    )

    // name of the sim2 file
    val sim_name = ListBuffer[String](Knowledge.dimensionality + "d_" + Knowledge.discr_type)
    if (Knowledge.grid_isStaggered) sim_name += "staggered"
    sim_name += Knowledge.domain_numBlocks + "Blocks"
    sim_name += Knowledge.domain_numFragmentsPerBlock + "Frags"

    // mandatory functions for VisIt (setup environment variables, parallel initializations)
    if (Knowledge.mpi_enabled) {
      fctBody += IR_VariableDeclaration(IR_ArrayDatatype(IR_CharDatatype, 1000), "fn")
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("sprintf"), IR_VariableAccess("fn", IR_ArrayDatatype(IR_CharDatatype, 1000)), IR_StringConstant("trace_%d.txt"), MPI_IV_MpiRank)
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItOpenTraceFile"), IR_VariableAccess("fn", IR_ArrayDatatype(IR_CharDatatype, 1000)))

      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetBroadcastIntFunction"), IR_Native("visit_broadcast_int_callback"))
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetBroadcastStringFunction"), IR_Native("visit_broadcast_string_callback"))

      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetParallel"), IR_BooleanConstant(true))
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetParallelRank"), MPI_IV_MpiRank)

      fctBody += IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "env", nullptr)
      fctBody += IR_IfCondition(
        MPI_IsRootProc(),
        IR_Assignment(IR_VariableAccess(IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "env")), IR_FunctionCall(IR_ExternalFunctionReference("VisItGetEnvironment")))
      )
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetupEnvironment2"), IR_VariableAccess(IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "env")))
      fctBody += IR_IfCondition(
        IR_VariableAccess(IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "env")) Neq nullptr,
        IR_FunctionCall(IR_ExternalFunctionReference("free"), IR_VariableAccess(IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "env")))
      )

      fctBody += IR_IfCondition(
        MPI_IsRootProc.apply(),
        ListBuffer[IR_Statement](
          cwd_decl,
          IR_ArrayAllocation(IR_VariableAccess(cwd_decl), IR_CharDatatype, IR_IntegerConstant(1000)),
          IR_Assignment(IR_VariableAccess(cwd_decl), IR_FunctionCall(getCWD, IR_VariableAccess(cwd_decl), IR_IntegerConstant(1000))),
          IR_FunctionCall(IR_ExternalFunctionReference("VisItInitializeSocketAndDumpSimFile"), IR_StringConstant(sim_name.head),
            IR_StringConstant(sim_name.tail.mkString("_")), IR_VariableAccess(cwd_decl), nullptr, nullptr, nullptr),
          IR_ArrayFree(IR_VariableAccess(cwd_decl))
        )
      )
    } else {
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItOpenTraceFile"), IR_StringConstant("trace.txt"))
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetupEnvironment"))
      fctBody += cwd_decl
      fctBody += IR_ArrayAllocation(IR_VariableAccess(cwd_decl), IR_CharDatatype, IR_IntegerConstant(1000))
      fctBody += IR_Assignment(IR_VariableAccess(cwd_decl), IR_FunctionCall(getCWD, IR_VariableAccess(cwd_decl), IR_IntegerConstant(1000)))
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItInitializeSocketAndDumpSimFile"),
        IR_StringConstant(sim_name.head), IR_StringConstant(sim_name.tail.mkString("_")), IR_VariableAccess(cwd_decl), nullptr, nullptr, nullptr)
      fctBody += IR_ArrayFree(IR_VariableAccess(cwd_decl))
    }

    IR_PlainFunction(
      name,
      IR_UnitDatatype,
      fctBody
    )
  }

  override def name : String = "visit_init"
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint()
}
