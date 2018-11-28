package exastencils.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.communication.ir.IR_Communicate
import exastencils.communication.ir.IR_CommunicateTarget
import exastencils.communication.ir.IR_IV_CommunicationId
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.core.Duplicate
import exastencils.deprecated.domain.ir.IR_ReadValueFrom
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.globals.ir.IR_AllocateDataFunction
import exastencils.grid.ir.IR_VF_NodePositionAsVec
import exastencils.optimization.ir.IR_GeneralSimplify
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_BuildString

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
    // localFragmentIdx = my index
    // neighIdx = neigh.index
    // neighFragmentIdx <-- from file
    ListBuffer[IR_Statement](
      IR_Assignment(IR_IV_NeighborIsValid(domain, neighIdx, Duplicate(localFragmentIdx)), true),
      IR_Assignment(IR_IV_NeighborIsRemote(domain, neighIdx, Duplicate(localFragmentIdx)), false),
      IR_Assignment(IR_IV_NeighborFragmentIdx(domain, neighIdx, Duplicate(localFragmentIdx)), neighFragmentIdx),
      setIterationOffset())
  }

  // remoteRank = blockID
  def connectRemoteElement(localFragmentIdx : IR_Expression, localNeighIdx : IR_Expression, remoteRank : IR_Expression, neighIdx : Int, domain : Int) = {
    ListBuffer[IR_Statement](
      IR_Assignment(IR_IV_NeighborIsValid(domain, neighIdx, Duplicate(localFragmentIdx)), true),
      IR_Assignment(IR_IV_NeighborIsRemote(domain, neighIdx, Duplicate(localFragmentIdx)), true),
      IR_Assignment(IR_IV_NeighborFragmentIdx(domain, neighIdx, Duplicate(localFragmentIdx)), localNeighIdx),
      IR_Assignment(IR_IV_NeighborRemoteRank(domain, neighIdx, Duplicate(localFragmentIdx)), remoteRank),
      setIterationOffset())
  }

  def connectFragmentFromFile(neighbor_blockID : IR_VariableAccess, neighbor_commID : IR_VariableAccess) = {
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
        val neigh_commID = IR_ArrayAccess(neighbor_commID, neigh.index)
        val neigh_blockID = IR_ArrayAccess(neighbor_blockID, neigh.index)

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

  def fillBoundaryGhostLayers(field : IR_Field) = {
    var body = new ListBuffer[IR_Statement]

    def numDims = field.fieldLayout.numDimsGrid

    val baseIndex = IR_LoopOverDimensions.defIt(numDims)
    baseIndex.indices ++= Array[IR_Expression](0, 0)
    val neighbors = DefaultNeighbors.neighbors

    // W
    val w_start = IR_ExpressionIndex(Array.fill(numDims)(0))
    val w_stop = IR_ExpressionIndex(Array.fill(numDims)(0))
    w_start(0) = field.fieldLayout.idxById("GLB", 0)
    w_stop(0) = field.fieldLayout.idxById("GLE", 0)
    w_start(1) = field.fieldLayout.idxById("DLB", 1)
    w_stop(1) = field.fieldLayout.idxById("DRE", 1)

    val w_indexRange = IR_ExpressionIndexRange(IR_ExpressionIndex(Array.fill(numDims)(0)), w_stop - w_start)
    IR_GeneralSimplify.doUntilDoneStandalone(w_indexRange)

    val w_boundaryIndex = IR_LoopOverDimensions.defIt(numDims)
    w_boundaryIndex(0) = field.fieldLayout.idxById("GLB", 0)
    val w_boundary = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), w_boundaryIndex)

    val w_interiorIndex = Duplicate(w_boundaryIndex)
    w_interiorIndex(0) += 1 + baseIndex(0)
    val w_interior = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), w_interiorIndex)

    val w_ghostIndex = Duplicate(w_boundaryIndex)
    w_ghostIndex(0) -= 1 + baseIndex(0)
    val w_ghost = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), w_ghostIndex)

    body += IR_IfCondition(IR_Negation(IR_IV_NeighborIsValid(0, neighbors(0).index, IR_LoopOverFragments.defIt)), ListBuffer[IR_Statement](
      IR_Comment("Loop for west ghost layers"),
      IR_LoopOverDimensions(numDims, w_indexRange,
        ListBuffer[IR_Statement](
          IR_Assignment(Duplicate(w_ghost), 2 * Duplicate(w_boundary) - Duplicate(w_interior))
        ), null, IR_ParallelizationInfo())
    ))

    // E
    val e_start = IR_ExpressionIndex(Array.fill(numDims)(0))
    val e_stop = IR_ExpressionIndex(Array.fill(numDims)(0))
    e_start(0) = field.fieldLayout.idxById("GRB", 0)
    e_stop(0) = field.fieldLayout.idxById("GRE", 0)
    e_start(1) = field.fieldLayout.idxById("DLB", 1)
    e_stop(1) = field.fieldLayout.idxById("DRE", 1)

    val e_indexRange = IR_ExpressionIndexRange(IR_ExpressionIndex(Array.fill(numDims)(0)), e_stop - e_start)
    IR_GeneralSimplify.doUntilDoneStandalone(e_indexRange)

    val e_boundaryIndex = IR_LoopOverDimensions.defIt(numDims)
    e_boundaryIndex(0) = field.fieldLayout.idxById("DRE", 0) - (w_stop(0) - w_start(0)) - 1
    val e_boundary = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), e_boundaryIndex)

    val e_interiorIndex = Duplicate(e_boundaryIndex)
    e_interiorIndex(0) -= 1 + baseIndex(0)
    val e_interior = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), e_interiorIndex)

    val e_ghostIndex = Duplicate(e_boundaryIndex)
    e_ghostIndex(0) += 1 + baseIndex(0)
    val e_ghost = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), e_ghostIndex)

    body += IR_IfCondition(IR_Negation(IR_IV_NeighborIsValid(0, neighbors(1).index, IR_LoopOverFragments.defIt)), ListBuffer[IR_Statement](
      IR_Comment("Loop for east ghost layers"),
      IR_LoopOverDimensions(numDims, e_indexRange,
        ListBuffer[IR_Statement](
          IR_Assignment(Duplicate(e_ghost), 2 * Duplicate(e_boundary) - Duplicate(e_interior))
        ), null, IR_ParallelizationInfo())
    ))

    // S
    val s_start = IR_ExpressionIndex(Array.fill(numDims)(0))
    val s_stop = IR_ExpressionIndex(Array.fill(numDims)(0))
    s_start(0) = field.fieldLayout.idxById("DLB", 0)
    s_stop(0) = field.fieldLayout.idxById("DRE", 0)
    s_start(1) = field.fieldLayout.idxById("GLB", 1)
    s_stop(1) = field.fieldLayout.idxById("GLE", 1)

    val s_indexRange = IR_ExpressionIndexRange(IR_ExpressionIndex(Array.fill(numDims)(0)), s_stop - s_start)
    IR_GeneralSimplify.doUntilDoneStandalone(s_indexRange)

    val s_boundaryIndex = IR_LoopOverDimensions.defIt(numDims)
    s_boundaryIndex(1) = field.fieldLayout.idxById("GLB", 1)
    val s_boundary = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), s_boundaryIndex)

    val s_interiorIndex = Duplicate(s_boundaryIndex)
    s_interiorIndex(1) += 1 + baseIndex(1)
    val s_interior = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), s_interiorIndex)

    val s_ghostIndex = Duplicate(s_boundaryIndex)
    s_ghostIndex(1) -= 1 + baseIndex(1)
    val s_ghost = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), s_ghostIndex)

    body += IR_IfCondition(IR_Negation(IR_IV_NeighborIsValid(0, neighbors(2).index, IR_LoopOverFragments.defIt)), ListBuffer[IR_Statement](
      IR_Comment("Loop for south ghost layers"),
      IR_LoopOverDimensions(numDims, s_indexRange,
        ListBuffer[IR_Statement](
          IR_Assignment(Duplicate(s_ghost), 2 * Duplicate(s_boundary) - Duplicate(s_interior))
        ), null, IR_ParallelizationInfo())
    ))

    // N
    val n_start = IR_ExpressionIndex(Array.fill(numDims)(0))
    val n_stop = IR_ExpressionIndex(Array.fill(numDims)(0))
    n_start(0) = field.fieldLayout.idxById("DLB", 0)
    n_stop(0) = field.fieldLayout.idxById("DRE", 0)
    n_start(1) = field.fieldLayout.idxById("GRB", 1)
    n_stop(1) = field.fieldLayout.idxById("GRE", 1)

    val n_indexRange = IR_ExpressionIndexRange(IR_ExpressionIndex(Array.fill(numDims)(0)), n_stop - n_start)
    IR_GeneralSimplify.doUntilDoneStandalone(n_indexRange)

    val n_boundaryIndex = IR_LoopOverDimensions.defIt(numDims)
    n_boundaryIndex(1) = field.fieldLayout.idxById("DRE", 1) - (s_stop(1) - s_start(1)) - 1
    val n_boundary = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), n_boundaryIndex)

    val n_interiorIndex = Duplicate(n_boundaryIndex)
    n_interiorIndex(1) -= 1 + baseIndex(1)
    val n_interior = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), n_interiorIndex)

    val n_ghostIndex = Duplicate(n_boundaryIndex)
    n_ghostIndex(1) += 1 + baseIndex(1)
    val n_ghost = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), n_ghostIndex)

    body += IR_IfCondition(IR_Negation(IR_IV_NeighborIsValid(0, neighbors(3).index, IR_LoopOverFragments.defIt)), ListBuffer[IR_Statement](
      IR_Comment("Loop for north ghost layers"),
      IR_LoopOverDimensions(numDims, n_indexRange,
        ListBuffer[IR_Statement](
          IR_Assignment(Duplicate(n_ghost), 2 * Duplicate(n_boundary) - Duplicate(n_interior))
        ), null, IR_ParallelizationInfo())
    ))

  }



  // setupConnectivityFromFile
  def setupConnectivityFromFile(read_line : IR_FunctionCall) = {
    var connStmts = new ListBuffer[IR_Statement]

    def strBuf = IR_VariableAccess("strBuf", IR_SpecialDatatype("std::string"))
    connStmts += IR_VariableDeclaration(strBuf)
    def iss = IR_VariableAccess("iss", IR_SpecialDatatype("std::istringstream"))
    connStmts += IR_VariableDeclaration(iss)

    def fragment_id = IR_VariableAccess("fragment_id", IR_IntegerDatatype)
    def neighbor_blockID = IR_VariableAccess("neighbor_blockID", IR_ArrayDatatype(IR_IntegerDatatype, 4))
    def neighbor_commID = IR_VariableAccess("neighbor_commID", IR_ArrayDatatype(IR_IntegerDatatype, 4))
    connStmts += IR_VariableDeclaration(fragment_id)
    connStmts += IR_VariableDeclaration(neighbor_blockID)
    connStmts += IR_VariableDeclaration(neighbor_commID)

    connStmts += read_line
    connStmts += IR_ReadStream(iss, ListBuffer(strBuf, fragment_id))

    // Assign values to the "real" variables
    connStmts += IR_Assignment(IR_IV_FragmentId(IR_LoopOverFragments.defIt), fragment_id)
    connStmts += IR_Assignment(IR_IV_CommunicationId(IR_LoopOverFragments.defIt), IR_LoopOverFragments.defIt)

    def i = IR_VariableAccess("i", IR_IntegerDatatype)
    // get comm_ids of neighbors
    connStmts += IR_ForLoop(
      IR_VariableDeclaration(i, 0),
      IR_Lower(i, 4),
      IR_PreIncrement(i),
      ListBuffer[IR_Statement](
        read_line,
        IR_ReadStream(iss, ListBuffer(strBuf, IR_ArrayAccess(neighbor_blockID, i), IR_ArrayAccess(neighbor_commID, i)))
      )
    )

    connStmts ++= connectFragmentFromFile(neighbor_blockID, neighbor_commID)

    IR_LoopOverFragments(connStmts, IR_ParallelizationInfo())
  }

  // jumpOverConnectivity
  //TODO ............
  def ignoreConnectivity(read_line : IR_FunctionCall) = {
    //TODO ...........................  code was just copy&pasted from setupConnectivityFromFile
    var connStmts = new ListBuffer[IR_Statement]

    def strBuf = IR_VariableAccess("strBuf", IR_SpecialDatatype("std::string"))
    connStmts += IR_VariableDeclaration(strBuf)
    //def iss = IR_VariableAccess("iss", IR_SpecialDatatype("std::istringstream"))
    //connStmts += IR_VariableDeclaration(iss)

    def fragment_id = IR_VariableAccess("fragment_id", IR_IntegerDatatype)
    def neighbor_blockID = IR_VariableAccess("neighbor_blockID", IR_ArrayDatatype(IR_IntegerDatatype, 4))
    def neighbor_commID = IR_VariableAccess("neighbor_commID", IR_ArrayDatatype(IR_IntegerDatatype, 4))
    connStmts += IR_VariableDeclaration(fragment_id)
    connStmts += IR_VariableDeclaration(neighbor_blockID)
    connStmts += IR_VariableDeclaration(neighbor_commID)

    connStmts += read_line
    //connStmts += IR_ReadStream(iss, ListBuffer(strBuf, fragment_id))

    // Assign values to the "real" variables
    connStmts += IR_Assignment(IR_IV_FragmentId(IR_LoopOverFragments.defIt), fragment_id)
    connStmts += IR_Assignment(IR_IV_CommunicationId(IR_LoopOverFragments.defIt), IR_LoopOverFragments.defIt)

    def i = IR_VariableAccess("i", IR_IntegerDatatype)
    // get comm_ids of neighbors
    connStmts += IR_ForLoop(
      IR_VariableDeclaration(i, 0),
      IR_Lower(i, 4),
      IR_PreIncrement(i),
      ListBuffer[IR_Statement](
        read_line
        //IR_ReadStream(iss, ListBuffer(strBuf, IR_ArrayAccess(neighbor_blockID, i), IR_ArrayAccess(neighbor_commID, i)))
      )
    )

    //connStmts ++= connectFragmentFromFile(neighbor_blockID, neighbor_commID)

    IR_LoopOverFragments(connStmts, IR_ParallelizationInfo())
  }

  // readNodes
  def readNodes(field : IR_Field,read_line : IR_FunctionCall) = {
    var body = new ListBuffer[IR_Statement]

    def numDims = field.fieldLayout.numDimsGrid
    val start = IR_ExpressionIndex(Array.fill(numDims)(0))
    val stop = IR_ExpressionIndex(Array.fill(numDims)(0))
    val startOffset = IR_ExpressionIndex(Array.fill(numDims)(0))
    val endOffset = IR_ExpressionIndex(Array.fill(numDims)(0))
    val n_grid_nodes = IR_ConstIndex(Array.fill(numDims)(0))
    for (dim <- 0 until numDims){
      start(dim) = field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim)
      stop(dim) = field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim)
      n_grid_nodes(dim) = field.fieldLayout.defIdxById("DRE", dim) -  field.fieldLayout.defIdxById("DLB", dim)

      //body += IR_Assert(IR_EqEq(n_grid_nodes(dim), n_grid_nodes_from_file),
      //  ListBuffer("\"Number of grid nodes (\"", "n_grid_nodes", "\") in dimension \"", dim, "\" does not fit maxLevel (\"", Knowledge.maxLevel, "\").\""),
      //  IR_FunctionCall("exit", 1))
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


    // get fragmentPosBegin_x, fragmentPosBegin_y, fragmentPosEnd_x, fragmentPosEnd_y
    // FIXME HACK for uniform grid test. It is not wrong but actually not required. Might be removed later on.
    node_statements += IR_Assignment(IR_IV_FragmentPositionBegin(0), IR_ArrayAccess(x_nodes, 0))
    node_statements += IR_Assignment(IR_IV_FragmentPositionBegin(1), IR_ArrayAccess(y_nodes, 0))
    node_statements += IR_Assignment(IR_IV_FragmentPositionEnd(0), IR_ArrayAccess(x_nodes, n_grid_nodes(0) * n_grid_nodes(1) - 1))
    node_statements += IR_Assignment(IR_IV_FragmentPositionEnd(1), IR_ArrayAccess(y_nodes, n_grid_nodes(0) * n_grid_nodes(1) - 1))


    for (dim <- 0 until numDims) {
      val indexRange = IR_ExpressionIndexRange(start, stop)
      IR_GeneralSimplify.doUntilDoneStandalone(indexRange)

      val baseIndex = IR_LoopOverDimensions.defIt(numDims)
      baseIndex.indices ++= Array[IR_Expression](dim, 0)
      val baseAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), baseIndex)

      node_statements += IR_LoopOverDimensions(numDims, indexRange,
        ListBuffer[IR_Statement](
          IR_Assignment(Duplicate(baseAccess),
            IR_ArrayAccess(if(dim == 0) x_nodes else y_nodes, baseIndex(0) + baseIndex(1) * n_grid_nodes(dim)))
        ), null, IR_ParallelizationInfo())
    }

    body += IR_LoopOverFragments(node_statements, IR_ParallelizationInfo())

  }

  // readGrid(level)
  def readGrid(level : Int, setupConnectivityAndFields : Boolean) = {
    var body = ListBuffer[IR_Statement]()

    body += IR_Comment("Read file for level " + level)
    if(setupConnectivityAndFields)
      body += IR_Comment("Setup connectivity and fields")

    // open file
    def file = IR_VariableAccess("file", IR_SpecialDatatype("std::ifstream"))   // I think I could also use val here
    body += IR_VariableDeclaration(file)
    def fileName = IR_VariableAccess("fileName", IR_StringDatatype)
    body += IR_VariableDeclaration(fileName)
    body += IR_BuildString(fileName , ListBuffer[IR_Expression](IR_StringConstant(Settings.experimental_domain_file + "/b"),
      MPI_IV_MpiRank,
      IR_StringConstant("_"),
      (1 << level) + 1,
      IR_StringConstant(".block")))
    body += IR_MemberFunctionCall(file, "open", fileName)


    // FIXME bad assert, should cover all mpi-ranks and also give information about file (like its name)
    body += IR_Assert(IR_MemberFunctionCall(file, "is_open"), ListBuffer("\"Unable to open file\""), IR_FunctionCall("exit", 1))


    def iss = IR_VariableAccess("iss", IR_SpecialDatatype("std::istringstream"))
    body += IR_VariableDeclaration(iss)
    def strBuf = IR_VariableAccess("strBuf", IR_SpecialDatatype("std::string"))
    body += IR_VariableDeclaration(strBuf)

    //get_line(file)
    val read_line = IR_FunctionCall(IR_ReadLineFromFile.name, file, iss)

    def n_fragments = IR_VariableAccess("n_fragments", IR_IntegerDatatype)
    body += IR_VariableDeclaration(n_fragments)

    body += read_line   // jump over block_id
    body += read_line
    body += IR_ReadStream(iss, ListBuffer(strBuf, n_fragments))
    body += read_line   // jump over n_grid_nodes

    ///////////////////////
    // read connectivity //
    if (setupConnectivityAndFields) {
      body += setupConnectivityFromFile(read_line)
      body += IR_FunctionCall(IR_AllocateDataFunction.fctName)
      body += IR_FunctionCall("initFieldsWithZero")
    }
    else {
      body += ignoreConnectivity(read_line)
    }

    val field = IR_VF_NodePositionAsVec.find(level).associatedField

    body ++= readNodes(field, read_line)


    body += IR_MemberFunctionCall(file, "close")

    // communicate (updated interior ghost layers)
    body += IR_Communicate(IR_FieldSelection(field, field.level, 0), "both", ListBuffer(IR_CommunicateTarget("ghost", None, None)), None)
    // deal with ghost layers
    body ++= fillBoundaryGhostLayers(field)

    IR_Scope(body)
  }





  override def generateFct() = {
    var body = ListBuffer[IR_Statement]()

    body += IR_Assert(IR_EqEq(s"mpiSize", Knowledge.domain_numBlocks),
      ListBuffer("\"Invalid number of MPI processes (\"", "mpiSize", "\") should be \"", Knowledge.mpi_numThreads),
      IR_FunctionCall("exit", 1))

    body += readGrid(Knowledge.maxLevel, true)

    if(Knowledge.experimental_domain_readAllLevelsFromFile){
      var level = 0
      for(level <- Knowledge.maxLevel - 1 to Knowledge.minLevel by -1){
        body += readGrid(level, false)
      }
    }
    else{
      // TODO Restriction from Random Points
      // TODO ........
      body += IR_Comment("Restriction is not implemented yet!!!")
      body += IR_Assert(IR_EqEq(Knowledge.maxLevel, Knowledge.minLevel),
        ListBuffer("\"Cannot handle different levels internally yet.\""),
        IR_FunctionCall("exit", 1))
    }

    IR_PlainFunction(name, IR_UnitDatatype, body)
  }
}
