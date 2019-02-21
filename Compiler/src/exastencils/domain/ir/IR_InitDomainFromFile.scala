package exastencils.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.communication.NeighborInfo
import exastencils.communication.ir.IR_CommTrafoCollection
import exastencils.communication.ir.IR_Communicate
import exastencils.communication.ir.IR_CommunicateTarget
import exastencils.communication.ir.IR_IV_CommunicationId
import exastencils.communication.ir.IR_IV_CommTrafoId
import exastencils.communication.ir.IR_IV_NeighFragId
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.core.Duplicate
import exastencils.deprecated.domain.ir.IR_ReadValueFrom
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.field.ir.IR_DirectFieldAccess
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.globals.ir.IR_AllocateDataFunction
import exastencils.grid.ir.IR_VF_NodePositionAsVec
import exastencils.optimization.ir.IR_GeneralSimplify
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_BuildString

//TODO move this to another place
case class IR_ReadStream(var stream : IR_Access, var toPrint : ListBuffer[IR_Access]) extends IR_Statement {
  override def prettyprint(out : PpStream) = out << stream << " >> " <<< (toPrint, " >> ") << ';'
}

case class IR_InitDomainFromFile() extends IR_FuturePlainFunction {
  override var name = "initDomain"
  override def prettyprint_decl() = prettyprint

  def setIterationOffset() = {
    IR_Comment("Iteration offset not implemented")
  }

  def connectLocalElement(localFragmentIdx : IR_Expression, neighFragmentIdx : IR_Expression, neighIdx : Int, domain : Int) = {
    ListBuffer[IR_Statement](
      IR_Assignment(IR_IV_NeighborIsValid(domain, neighIdx, Duplicate(localFragmentIdx)), true),
      IR_Assignment(IR_IV_NeighborIsRemote(domain, neighIdx, Duplicate(localFragmentIdx)), false),
      IR_Assignment(IR_IV_NeighborFragmentIdx(domain, neighIdx, Duplicate(localFragmentIdx)), neighFragmentIdx),
      setIterationOffset())
  }

  def connectRemoteElement(localFragmentIdx : IR_Expression, localNeighIdx : IR_Expression, remoteRank : IR_Expression, neighIdx : Int, domain : Int) = {
    ListBuffer[IR_Statement](
      IR_Assignment(IR_IV_NeighborIsValid(domain, neighIdx, Duplicate(localFragmentIdx)), true),
      IR_Assignment(IR_IV_NeighborIsRemote(domain, neighIdx, Duplicate(localFragmentIdx)), true),
      IR_Assignment(IR_IV_NeighborFragmentIdx(domain, neighIdx, Duplicate(localFragmentIdx)), localNeighIdx),
      IR_Assignment(IR_IV_NeighborRemoteRank(domain, neighIdx, Duplicate(localFragmentIdx)), remoteRank),
      setIterationOffset())
  }

  def connectFragmentFromFile(neighborBlockID : IR_VariableAccess, neighborCommID : IR_VariableAccess) = {
    var body = new ListBuffer[IR_Statement]

    val neighbors = DefaultNeighbors.neighbors
    val domains = IR_DomainCollection.objects

    for (d <- domains.indices) {
      body += IR_Assignment(IR_IV_IsValidForDomain(d), IR_ReadValueFrom(IR_BooleanDatatype, "data"))
    }

    if (Knowledge.domain_canHaveLocalNeighs || Knowledge.domain_canHaveRemoteNeighs || Knowledge.domain_rect_hasPeriodicity) {
      for (neigh <- neighbors) {
        var statements = ListBuffer[IR_Statement]()

        // compile connect calls
        val neigh_commID = IR_ArrayAccess(neighborCommID, neigh.index)
        val neigh_blockID = IR_ArrayAccess(neighborBlockID, neigh.index)

        def localConnect(domainIdx : Int) = connectLocalElement(IR_LoopOverFragments.defIt,
          neigh_commID, neigh.index, domainIdx)

        def remoteConnect(domainIdx : Int) = connectRemoteElement(IR_LoopOverFragments.defIt,
          neigh_commID, neigh_blockID, neigh.index, domainIdx)

        for (d <- domains.indices) {
          statements += IR_IfCondition(IR_EqEq(neigh_blockID, -1),
            IR_Assignment(IR_IV_NeighborIsValid(d, neigh.index, Duplicate(IR_LoopOverFragments.defIt)), false),
            IR_IfCondition(IR_EqEq(neigh_blockID, MPI_IV_MpiRank),
              localConnect(d),
              IR_IfCondition(IR_Neq(neigh_blockID, MPI_IV_MpiRank),
                remoteConnect(d))
            )
          )

        }

        // wrap in scope due to local variable declarations
        body += IR_Scope(statements)
      }
    }

    body
  }

  def setupCommTransformation(neighbor_edge : IR_VariableAccess, domain : Int) = {
    var commStmts = new ListBuffer[IR_Statement]

    def i = IR_VariableAccess("i", IR_IntegerDatatype)

    def ne = IR_ArrayAccess(neighbor_edge, i)

    def neighBoundaryId = IR_VariableAccess("neighBoundaryId", IR_IntegerDatatype)

    commStmts += IR_VariableDeclaration(neighBoundaryId)
    commStmts += IR_Assignment(neighBoundaryId, IR_IntegerConstant(-1))

    commStmts += IR_IfCondition(IR_EqEq(ne, IR_StringConstant("W")), IR_Assignment(neighBoundaryId, 0))
    commStmts += IR_IfCondition(IR_EqEq(ne, IR_StringConstant("E")), IR_Assignment(neighBoundaryId, 1))
    commStmts += IR_IfCondition(IR_EqEq(ne, IR_StringConstant("S")), IR_Assignment(neighBoundaryId, 2))
    commStmts += IR_IfCondition(IR_EqEq(ne, IR_StringConstant("N")), IR_Assignment(neighBoundaryId, 3))
    commStmts += IR_IfCondition(IR_EqEq(ne, IR_StringConstant("X")), IR_Assignment(IR_IV_CommTrafoId(domain, i), -1))

    def commCase(caseList : List[(Int, Int)], id : Int) = {

      def andAndWrap(thisEdge : Int, neighEdge : Int) : IR_Expression = {
        IR_AndAnd(IR_EqEq(i, thisEdge), IR_EqEq(neighBoundaryId, neighEdge))
      }

      var and = new ListBuffer[IR_Expression]
      for (c <- caseList) {
        and = and :+ andAndWrap(c._1, c._2)
      }

      IR_IfCondition(
        and.reduce(IR_OrOr),
        IR_Assignment(IR_IV_CommTrafoId(domain, i), id)
      )
    }

    IR_CommTrafoCollection.trafoArray.zipWithIndex.foreach {
      case (x, i) => commStmts += commCase(x, i)
    }
    // get comm_ids of neighbors
    ListBuffer[IR_Statement](IR_ForLoop(
      IR_VariableDeclaration(i, 0),
      IR_Lower(i, 4),
      IR_PreIncrement(i),
      commStmts
    ))

  }

  def adaptGhostLayers(field : IR_Field) = {
    var stmts = ListBuffer[IR_Statement]()

    val neighbors = DefaultNeighbors.neighbors
    val domainIdx : IR_Expression = field.domain.index
    val numDims = field.fieldLayout.numDimsGrid

    def resolveIndex(indexId : String, dim : Int) = field.fieldLayout.idxById(indexId, dim)

    def changePositionForward(neigh : NeighborInfo) = {
      val dirId = if (neigh.dir(0) == 0) 1 else 0
      val nonDirId = if (dirId == 0) 1 else 0

      val begin = IR_ExpressionIndex(
        (0 until numDims).toArray.map {
          case i if neigh.dir(i) == 0 => resolveIndex("GLB", i)
          case i if neigh.dir(i) < 0  => resolveIndex("GLB", i)
          case i if neigh.dir(i) > 0  => resolveIndex("GRB", i)
        })

      val end = IR_ExpressionIndex(
        (0 until numDims).toArray.map {
          case i if neigh.dir(i) == 0 => resolveIndex("GRE", i)
          case i if neigh.dir(i) < 0  => resolveIndex("GLE", i)
          case i if neigh.dir(i) > 0  => resolveIndex("GRE", i)
        })

      val iDir = IR_VariableAccess("iDir", IR_IntegerDatatype)

      val accessIndex = IR_ExpressionIndex(
        (0 until numDims).toArray.map {
          case i if neigh.dir(i) == 0 => iDir
          case i if neigh.dir(i) < 0  => resolveIndex("GLE", i) - 1
          case i if neigh.dir(i) > 0  => resolveIndex("GRB", i)
        })

      val accessIndexIncr = IR_ExpressionIndex(
        (0 until numDims).toArray.map {
          case i if neigh.dir(i) == 0 => iDir + 1
          case i if neigh.dir(i) < 0  => resolveIndex("GLE", i) - 1
          case i if neigh.dir(i) > 0  => resolveIndex("GRB", i)
        })

      val decl = IR_VariableDeclaration(iDir, begin(nonDirId))
      val cond = IR_Lower(iDir, end(nonDirId))
      val incr = IR_PreIncrement(iDir)

      val fieldAccess = IR_DirectFieldAccess(IR_FieldSelection(field, field.level, 0), accessIndex)
      val fieldAccessIncr = IR_DirectFieldAccess(IR_FieldSelection(field, field.level, 0), accessIndexIncr)

      val body = IR_Assignment(fieldAccess, fieldAccessIncr)

      IR_ForLoop(decl, cond, incr, body)
    }

    def changePositionBackward(neigh : NeighborInfo) = {
      val dirId = if (neigh.dir(0) == 0) 1 else 0
      val nonDirId = if (dirId == 0) 1 else 0

      val begin = IR_ExpressionIndex(
        (0 until numDims).toArray.map {
          case i if neigh.dir(i) == 0 => resolveIndex("GLB", i)
          case i if neigh.dir(i) < 0  => resolveIndex("GLB", i)
          case i if neigh.dir(i) > 0  => resolveIndex("GRB", i)
        })

      val end = IR_ExpressionIndex(
        (0 until numDims).toArray.map {
          case i if neigh.dir(i) == 0 => resolveIndex("GRE", i) - 1
          case i if neigh.dir(i) < 0  => resolveIndex("GLE", i)
          case i if neigh.dir(i) > 0  => resolveIndex("GRE", i)
        })

      val iDir = IR_VariableAccess("iDir", IR_IntegerDatatype)

      val accessIndex = IR_ExpressionIndex(
        (0 until numDims).toArray.map {
          case i if neigh.dir(i) == 0 => iDir
          case i if neigh.dir(i) < 0  => resolveIndex("GLE", i) - 1
          case i if neigh.dir(i) > 0  => resolveIndex("GRB", i)
        })

      val accessIndexDecr = IR_ExpressionIndex(
        (0 until numDims).toArray.map {
          case i if neigh.dir(i) == 0 => iDir - 1
          case i if neigh.dir(i) < 0  => resolveIndex("GLE", i) - 1
          case i if neigh.dir(i) > 0  => resolveIndex("GRB", i)
        })

      val decl = IR_VariableDeclaration(iDir, end(nonDirId))
      val cond = IR_Greater(iDir, begin(nonDirId))
      val decr = IR_PreDecrement(iDir)

      val fieldAccess = IR_DirectFieldAccess(IR_FieldSelection(field, field.level, 0), accessIndex)
      val fieldAccessDecr = IR_DirectFieldAccess(IR_FieldSelection(field, field.level, 0), accessIndexDecr)

      val body = IR_Assignment(fieldAccess, fieldAccessDecr)

      IR_ForLoop(decl, cond, decr, body)
    }

    // adapt boundary points for case 1 | 3
    def adapt(neigh : NeighborInfo) = {
      IR_IfCondition(IR_OrOr(
        IR_EqEq(IR_IV_CommTrafoId(domainIdx, neigh.index), 1),
        IR_EqEq(IR_IV_CommTrafoId(domainIdx, neigh.index), 3)
      ),
        if (neigh.dir.sum > 0)
          changePositionForward(neigh)
        else
          changePositionBackward(neigh)
      )
    }

    neighbors map adapt
  }

  // TODO optimize code by including edges in standard boundary computation
  def fillBoundaryGhostLayers(field : IR_Field) = {
    var body = new ListBuffer[IR_Statement]

    def numDims = field.fieldLayout.numDimsGrid

    val baseIndex = IR_LoopOverDimensions.defIt(numDims)

    def resolveIndex(indexId : String, dim : Int) = field.fieldLayout.idxById(indexId, dim)

    def genIndexRangeBoundary(neigh : NeighborInfo) : IR_ExpressionIndexRange = {
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(
          (0 until numDims).toArray.map {
            case i if neigh.dir(i) == 0 => resolveIndex("DLB", i)
            case i if neigh.dir(i) < 0  => resolveIndex("GLB", i)
            case i if neigh.dir(i) > 0  => resolveIndex("GRB", i)
          }),
        IR_ExpressionIndex(
          (0 until numDims).toArray.map {
            case i if neigh.dir(i) == 0 => resolveIndex("DRE", i)
            case i if neigh.dir(i) < 0  => resolveIndex("GLE", i)
            case i if neigh.dir(i) > 0  => resolveIndex("GRE", i)
          }))
    }

    def genIndexRangeFullBoundary(neigh : NeighborInfo) : IR_ExpressionIndexRange = {
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(
          (0 until numDims).toArray.map {
            case i if neigh.dir(i) == 0 => resolveIndex("GLB", i)
            case i if neigh.dir(i) < 0  => resolveIndex("GLB", i)
            case i if neigh.dir(i) > 0  => resolveIndex("GRB", i)
          }),
        IR_ExpressionIndex(
          (0 until numDims).toArray.map {
            case i if neigh.dir(i) == 0 => resolveIndex("GRE", i)
            case i if neigh.dir(i) < 0  => resolveIndex("GLE", i)
            case i if neigh.dir(i) > 0  => resolveIndex("GRE", i)
          }))
    }

    def genIndexBoundary(neigh : NeighborInfo) : IR_ExpressionIndex = {
      IR_ExpressionIndex(
        (0 until numDims).toArray.map {
          case i if neigh.dir(i) == 0 => IR_LoopOverDimensions.defIt(numDims)(i)
          case i if neigh.dir(i) < 0  => resolveIndex("DLB", i)
          case i if neigh.dir(i) > 0  => resolveIndex("DRE", i) - 1
        })
    }

    def genIndexInterior(neigh : NeighborInfo) : IR_ExpressionIndex = {
      IR_ExpressionIndex(
        (0 until numDims).toArray.map {
          case i if neigh.dir(i) == 0 => IR_LoopOverDimensions.defIt(numDims)(i)
          case i if neigh.dir(i) < 0  => 2 * resolveIndex("DLB", i) - baseIndex(i)
          case i if neigh.dir(i) > 0  => 2 * (resolveIndex("DRE", i) - 1) - baseIndex(i)
        })

    }

    def fieldSelection = IR_FieldSelection(field, field.level, 0)

    for (neigh <- DefaultNeighbors.neighbors) {
      val ghost = IR_DirectFieldAccess(fieldSelection, baseIndex)
      val boundary = IR_DirectFieldAccess(fieldSelection, genIndexBoundary(neigh))
      val interior = IR_DirectFieldAccess(fieldSelection, genIndexInterior(neigh))

      body += IR_IfCondition(IR_Negation(IR_IV_NeighborIsValid(0, neigh.index)), ListBuffer[IR_Statement](
        IR_LoopOverDimensions(numDims,
          // TODO generalize this if-condition (only works for 2D so far)
          if (neigh.index < 2)
            genIndexRangeBoundary(neigh)
          else
            genIndexRangeFullBoundary(neigh),
          ListBuffer[IR_Statement](
            IR_Assignment(ghost, 2 * boundary - interior)
          ), null, IR_ParallelizationInfo())
      ))
    }

    body
  }

  def setupConnectivityFromFile(read_line : IR_FunctionCall) = {
    var connStmts = new ListBuffer[IR_Statement]

    def strBuf = IR_VariableAccess("strBuf", IR_SpecialDatatype("std::string"))

    connStmts += IR_VariableDeclaration(strBuf)

    def iss = IR_VariableAccess("iss", IR_SpecialDatatype("std::istringstream"))

    connStmts += IR_VariableDeclaration(iss)

    def fragmentId = IR_VariableAccess("fragment_id", IR_IntegerDatatype)

    def neighborBlockID = IR_VariableAccess("neighborBlockID", IR_ArrayDatatype(IR_IntegerDatatype, 4))

    def neighborCommID = IR_VariableAccess("neighborCommID", IR_ArrayDatatype(IR_IntegerDatatype, 4))

    def neighborEdge = IR_VariableAccess("neighborEdge", IR_ArrayDatatype(IR_StringDatatype, 4))

    def neighborFragID = IR_VariableAccess("neighborFragID", IR_ArrayDatatype(IR_IntegerDatatype, 4))

    connStmts += IR_VariableDeclaration(fragmentId)
    connStmts += IR_VariableDeclaration(neighborBlockID)
    connStmts += IR_VariableDeclaration(neighborCommID)
    connStmts += IR_VariableDeclaration(neighborEdge)
    connStmts += IR_VariableDeclaration(neighborFragID)

    connStmts += read_line
    connStmts += IR_ReadStream(iss, ListBuffer(strBuf, fragmentId))

    // Assign values to the "real" variables
    connStmts += IR_Assignment(IR_IV_FragmentId(IR_LoopOverFragments.defIt), fragmentId)
    connStmts += IR_Assignment(IR_IV_CommunicationId(IR_LoopOverFragments.defIt), IR_LoopOverFragments.defIt)

    def i = IR_VariableAccess("i", IR_IntegerDatatype)
    // get comm_ids of neighbors
    connStmts += IR_ForLoop(
      IR_VariableDeclaration(i, 0),
      IR_Lower(i, DefaultNeighbors.neighbors.length),
      IR_PreIncrement(i),
      ListBuffer[IR_Statement](
        read_line,
        IR_ReadStream(iss, ListBuffer(
          strBuf,
          IR_ArrayAccess(neighborBlockID, i),
          IR_ArrayAccess(neighborCommID, i),
          IR_ArrayAccess(neighborEdge, i),
          IR_ArrayAccess(neighborFragID, i)))
      )
    )

    connStmts ++= connectFragmentFromFile(neighborBlockID, neighborCommID)

    val domains = IR_DomainCollection.objects
    for (d <- domains.indices) {
      connStmts ++= setupCommTransformation(neighborEdge, d)
      for (neigh <- DefaultNeighbors.neighbors) {
        connStmts += IR_Assignment(IR_IV_NeighFragId(d, neigh.index), IR_ArrayAccess(neighborFragID, neigh.index))
      }
    }

    IR_LoopOverFragments(connStmts, IR_ParallelizationInfo())
  }

  // ignoreConnectivity (for lower levels)
  def ignoreConnectivity(read_line : IR_FunctionCall) = {
    var connStmts = new ListBuffer[IR_Statement]

    def strBuf = IR_VariableAccess("strBuf", IR_SpecialDatatype("std::string"))

    connStmts += IR_VariableDeclaration(strBuf)

    def fragment_id = IR_VariableAccess("fragment_id", IR_IntegerDatatype)

    def neighbor_blockID = IR_VariableAccess("neighbor_blockID", IR_ArrayDatatype(IR_IntegerDatatype, 4))

    def neighbor_commID = IR_VariableAccess("neighbor_commID", IR_ArrayDatatype(IR_IntegerDatatype, 4))

    connStmts += IR_VariableDeclaration(fragment_id)
    connStmts += IR_VariableDeclaration(neighbor_blockID)
    connStmts += IR_VariableDeclaration(neighbor_commID)

    connStmts += read_line

    // Assign values to the "real" variables
    connStmts += IR_Assignment(IR_IV_FragmentId(IR_LoopOverFragments.defIt), fragment_id)
    connStmts += IR_Assignment(IR_IV_CommunicationId(IR_LoopOverFragments.defIt), IR_LoopOverFragments.defIt)

    def i = IR_VariableAccess("i", IR_IntegerDatatype)
    // get comm_ids of neighbors
    connStmts += IR_ForLoop(
      IR_VariableDeclaration(i, 0),
      IR_Lower(i, 4),
      IR_PreIncrement(i),
      ListBuffer[IR_Statement](read_line)
    )

    IR_LoopOverFragments(connStmts, IR_ParallelizationInfo())
  }

  def readNodes(field : IR_Field, read_line : IR_FunctionCall) = {
    var body = new ListBuffer[IR_Statement]

    def numDims = field.fieldLayout.numDimsGrid

    val start = IR_ExpressionIndex(Array.fill(numDims)(0))
    val stop = IR_ExpressionIndex(Array.fill(numDims)(0))
    val startOffset = IR_ExpressionIndex(Array.fill(numDims)(0))
    val endOffset = IR_ExpressionIndex(Array.fill(numDims)(0))
    val n_grid_nodes = IR_ConstIndex(Array.fill(numDims)(0))
    for (dim <- 0 until numDims) {
      start(dim) = field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim)
      stop(dim) = field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim)
      n_grid_nodes(dim) = field.fieldLayout.defIdxById("DRE", dim) - field.fieldLayout.defIdxById("DLB", dim)
    }

    var node_statements = ListBuffer[IR_Statement]()

    // TODO make x/y-nodes a multi-array
    def x_nodes = IR_VariableAccess("x_nodes", IR_ArrayDatatype(IR_FloatDatatype, n_grid_nodes(0) * n_grid_nodes(0)))

    def y_nodes = IR_VariableAccess("y_nodes", IR_ArrayDatatype(IR_FloatDatatype, n_grid_nodes(1) * n_grid_nodes(1)))

    node_statements += IR_VariableDeclaration(x_nodes)
    node_statements += IR_VariableDeclaration(y_nodes)

    def iss = IR_VariableAccess("iss", IR_SpecialDatatype("std::istringstream"))

    node_statements += IR_VariableDeclaration(iss)

    def i = IR_VariableAccess("i", IR_IntegerDatatype)

    node_statements += IR_ForLoop(
      IR_VariableDeclaration(i, 0),
      IR_Lower(i, n_grid_nodes(0) * n_grid_nodes(1)),
      IR_PreIncrement(i),
      ListBuffer[IR_Statement](
        read_line,
        IR_ReadStream(iss, ListBuffer(IR_ArrayAccess(x_nodes, i), IR_ArrayAccess(y_nodes, i)))
      )
    )

    for (dim <- 0 until numDims) {
      val indexRange = IR_ExpressionIndexRange(start, stop)
      IR_GeneralSimplify.doUntilDoneStandalone(indexRange)

      val baseIndex = IR_LoopOverDimensions.defIt(numDims)
      baseIndex.indices ++= Array[IR_Expression](dim, 0)
      val baseAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), baseIndex)

      node_statements += IR_LoopOverDimensions(numDims, indexRange,
        ListBuffer[IR_Statement](
          IR_Assignment(Duplicate(baseAccess),
            IR_ArrayAccess(if (dim == 0) x_nodes else y_nodes, baseIndex(0) + baseIndex(1) * n_grid_nodes(dim)))
        ), null, IR_ParallelizationInfo())
    }

    body += IR_LoopOverFragments(node_statements, IR_ParallelizationInfo())

  }

  def readGrid(level : Int, setupConnectivityAndFields : Boolean) = {
    var body = ListBuffer[IR_Statement]()

    body += IR_Comment("Read file for level " + level)
    if (setupConnectivityAndFields)
      body += IR_Comment("Setup connectivity and fields")

    // open file
    def file = IR_VariableAccess("file", IR_SpecialDatatype("std::ifstream"))

    body += IR_VariableDeclaration(file)

    def fileName = IR_VariableAccess("fileName", IR_StringDatatype)

    body += IR_VariableDeclaration(fileName)
    body += IR_BuildString(fileName, ListBuffer[IR_Expression](IR_StringConstant(Settings.experimental_domain_file + "/b"),
      MPI_IV_MpiRank,
      IR_StringConstant("_"),
      (1 << level) + 1,
      IR_StringConstant(".block")))
    body += IR_MemberFunctionCall(file, "open", fileName)


    // FIXME assert should cover all mpi-ranks
    body += IR_Assert(IR_MemberFunctionCall(file, "is_open"), ListBuffer("\"Unable to open file \"", fileName), IR_FunctionCall("exit", 1))

    def iss = IR_VariableAccess("iss", IR_SpecialDatatype("std::istringstream"))

    def strBuf = IR_VariableAccess("strBuf", IR_SpecialDatatype("std::string"))

    def n_fragments = IR_VariableAccess("n_fragments", IR_IntegerDatatype)

    body += IR_VariableDeclaration(iss)
    body += IR_VariableDeclaration(strBuf)
    body += IR_VariableDeclaration(n_fragments)

    val read_line = IR_FunctionCall(IR_ReadLineFromFile.name, file, iss)

    body += read_line // jump over block_id
    body += read_line
    body += IR_ReadStream(iss, ListBuffer(strBuf, n_fragments))
    body += read_line // jump over n_grid_nodes

    // read connectivity
    if (setupConnectivityAndFields) {
      body += setupConnectivityFromFile(read_line)
      body += IR_FunctionCall(IR_AllocateDataFunction.fctName)
      body += IR_FunctionCall("initFieldsWithZero")
    }
    else {
      body += ignoreConnectivity(read_line)
    }

    val field = IR_VF_NodePositionAsVec.find(level).associatedField

    // read grid nodes
    body ++= readNodes(field, read_line)

    body += IR_MemberFunctionCall(file, "close")

    // communicate (updated interior ghost layers)
    body += IR_Communicate(IR_FieldSelection(field, field.level, 0), "both", ListBuffer(IR_CommunicateTarget("ghost", None, None)), None)
    // deal with ghost layers on boundary
    body += IR_LoopOverFragments(fillBoundaryGhostLayers(field))
    // adapt ghost layers to match upper and lower triangles of neighbors
    body += IR_Comment("Adapt ghost layers by repositioning knots")
    body ++= adaptGhostLayers(field)

    IR_Scope(body)
  }

  override def generateFct() = {
    var body = ListBuffer[IR_Statement]()

    if (Knowledge.mpi_enabled)
      body += IR_Assert(IR_EqEq(s"mpiSize", Knowledge.domain_numBlocks),
        ListBuffer("\"Invalid number of MPI processes (\"", "mpiSize", "\") should be \"", Knowledge.mpi_numThreads),
        IR_FunctionCall("exit", 1))

    body += readGrid(Knowledge.maxLevel, true)

    if (Knowledge.experimental_domain_readAllLevelsFromFile) {
      var level = 0
      for (level <- Knowledge.maxLevel - 1 to Knowledge.minLevel by -1) {
        body += readGrid(level, false)
      }
    }
    else {
      // TODO Restriction from Random Points
      // TODO ........
      ////////////////////////////////
      // Remove this stuff.... ///////
      body += IR_Comment("Restriction is not implemented yet!!!")
      body += IR_Assert(IR_EqEq(Knowledge.maxLevel, Knowledge.minLevel),
        ListBuffer("\"Cannot handle different levels internally yet.\""),
        IR_FunctionCall("exit", 1))
      ///////////////////////////////

    }

    IR_PlainFunction(name, IR_UnitDatatype, body)
  }
}
