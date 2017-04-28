package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.baseExt.l4.L4_VectorDatatype
import exastencils.boundary.l4.L4_NoBC
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.deprecated.domain.RectangularDomain
import exastencils.deprecated.ir._
import exastencils.domain.AABB
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.field.l4._
import exastencils.hack.ir.HACK_IR_Native
import exastencils.logger.Logger

/// GridGeometry_nonAA

object GridGeometry_nonAA extends GridGeometry {
  def genDirectFieldAccess(fieldName : String, level : Int, index : IR_ExpressionIndex) = {
    val field = IR_FieldCollection.getByIdentifierLevExp(fieldName, level).get
    IR_FieldAccess(IR_FieldSelection(field, field.level, 0), Duplicate(index))
  }

  def genComponentAccess(fieldName : String, level : Int, index : IR_ExpressionIndex, dim : Int) = {
    val field = IR_FieldCollection.getByIdentifierLevExp(fieldName, level).get
    val newIndex = Duplicate(index)
    // extra index for matrix expression
    newIndex.indices ++= Array[IR_Expression](dim, 0)
    IR_FieldAccess(IR_FieldSelection(field, field.level, 0), newIndex)
  }

  // direct accesses
  override def nodePosAsVec(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int]) =
    genDirectFieldAccess("node_pos", level, index)

  // direct accesses
  override def nodePosition(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) =
    genComponentAccess("node_pos", level, index, dim)

  // direct accesses
  override def cellCenAsVec(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int]) : IR_Expression =
    genDirectFieldAccess("cell_center", level, index)

  // direct accesses
  override def cellCenter(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression =
    genComponentAccess("cell_center", level, index, dim)

  override def cellWidth(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression = ???

  object VF_NodePosition extends VirtualField {
    def datatype = L4_VectorDatatype(L4_RealDatatype, Knowledge.dimensionality)
    def layout = s"NodePosVec${ Knowledge.dimensionality }Data"

    def getLayoutDecl = {
      L4_FieldLayoutDecl(
        L4_LeveledIdentifier(layout, L4_AllLevels),
        datatype, "node",
        ListBuffer(
          L4_FieldLayoutOption("ghostLayers", L4_ConstIndex(Array.fill(Knowledge.dimensionality)(2)), true),
          L4_FieldLayoutOption("duplicateLayers", L4_ConstIndex(Array.fill(Knowledge.dimensionality)(1)), true)))
    }

    def getFieldDecl = {
      L4_FieldDecl(L4_LeveledIdentifier("node_pos", L4_AllLevels), "global", layout, L4_NoBC, 1, 0)
    }
  }

  object VF_CellCenter extends VirtualField {
    def datatype = L4_VectorDatatype(L4_RealDatatype, Knowledge.dimensionality)
    def layout = s"CellCenVec${ Knowledge.dimensionality }Data"

    def getLayoutDecl = {
      L4_FieldLayoutDecl(
        L4_LeveledIdentifier(layout, L4_AllLevels),
        datatype, "cell",
        ListBuffer(
          L4_FieldLayoutOption("ghostLayers", L4_ConstIndex(Array.fill(Knowledge.dimensionality)(2)), true),
          L4_FieldLayoutOption("duplicateLayers", L4_ConstIndex(Array.fill(Knowledge.dimensionality)(0)), true)))
    }

    def getFieldDecl = {
      L4_FieldDecl(L4_LeveledIdentifier("cell_center", L4_AllLevels), "global", layout, L4_NoBC, 1, 0)
    }
  }

  // injection of  missing l4 information for virtual fields and generation of setup code
  override def initL4() : Unit = {
    ExaRootNode.l4_root.nodes += VF_NodePosition.getLayoutDecl
    ExaRootNode.l4_root.nodes += VF_NodePosition.getFieldDecl

    ExaRootNode.l4_root.nodes += VF_CellCenter.getLayoutDecl
    ExaRootNode.l4_root.nodes += VF_CellCenter.getFieldDecl
  }

  override def generateInitCode() : ListBuffer[IR_Statement] = {
    Knowledge.grid_spacingModel match {
      case "uniform" =>
        (Knowledge.maxLevel to Knowledge.minLevel by -1).map(level =>
          setupNodePos_Uniform(level)).reduceLeft(_ ++ _)

      case "random" =>
        prepareRandomEngine ++
          (Knowledge.maxLevel to Knowledge.minLevel by -1).map(level =>
            setupNodePos_Random(level)).reduceLeft(_ ++ _)
    }
  }

  def HACK_numDims = Knowledge.dimensionality // TODO: fix dim

  def setupNodePos_Uniform(level : Int) : ListBuffer[IR_Statement] = {
    val stmts = ListBuffer[IR_Statement]()

    for (dim <- Knowledge.dimensions) {
      val numCellsPerFrag = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim)
      val numCellsTotal = numCellsPerFrag * Knowledge.domain_rect_numFragsTotalAsVec(dim)

      // fix grid width to match domain size
      if (IR_DomainCollection.objects.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to the first domain")
      val domainBounds = IR_DomainCollection.objects(0).asInstanceOf[RectangularDomain].shape.shapeData.asInstanceOf[AABB]
      val cellWidth = (domainBounds.upper(dim) - domainBounds.lower(dim)) / numCellsTotal

      // look up field and compile access to base element
      val field = IR_FieldCollection.getByIdentifier(s"node_pos", level).get
      val baseIndex = IR_LoopOverDimensions.defIt(HACK_numDims)
      baseIndex.indices ++= Array[IR_Expression](dim, 0)
      val baseAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), baseIndex)

      // fix the inner iterator -> used for zone checks
      def innerIt =
        if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
          IR_LoopOverDimensions.defItForDim(dim)
        else
          IR_VariableAccess(s"global_${ IR_DimToString(dim) }", IR_IntegerDatatype)
      val innerItDecl =
        if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
          IR_NullStatement
        else
          IR_VariableDeclaration(innerIt, IR_LoopOverDimensions.defItForDim(dim) + IR_IV_FragmentIndex(dim) * numCellsPerFrag)

      // compile final loop
      val innerLoop = IR_LoopOverPoints(field, None,
        IR_ExpressionIndex(Array.fill(HACK_numDims)(-2)),
        IR_ExpressionIndex(Array.fill(HACK_numDims)(-2)),
        IR_ExpressionIndex(1, 1, 1),
        ListBuffer[IR_Statement](
          innerItDecl,
          IR_Assignment(Duplicate(baseAccess),
            domainBounds.lower(dim) + innerIt * cellWidth))
      )
      innerLoop.parallelization.potentiallyParallel = false

      stmts += innerLoop
    }

    stmts
  }

  def prepareRandomEngine : ListBuffer[IR_Statement] = {
    val stmts = ListBuffer[IR_Statement]()

    stmts += HACK_IR_Native(s"static std::default_random_engine generator(${ Knowledge.maxLevel })")
    stmts += HACK_IR_Native(s"static std::uniform_real_distribution <double> distribution(-0.1 * pow(2, -${ Knowledge.maxLevel }), 0.1 * pow(2, -${ Knowledge.maxLevel }))")
    stmts += HACK_IR_Native(s"static auto randn = std::bind (distribution, generator)")

    stmts
  }

  def setupNodePos_Random(level : Int) : ListBuffer[IR_Statement] = {
    val stmts = ListBuffer[IR_Statement]()

    // init with uniform first
    stmts ++= setupNodePos_Uniform(level)

    // apply modification of positions
    for (dim <- Knowledge.dimensions) {
      // look up field and compile access to base element
      val field = IR_FieldCollection.getByIdentifier(s"node_pos", level).get
      val baseIndex = IR_LoopOverDimensions.defIt(HACK_numDims)
      baseIndex.indices ++= Array[IR_Expression](dim, 0)
      val baseAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), baseIndex)

      if (level == Knowledge.maxLevel) {
        // on finest level: add random offset
        val innerLoop = IR_LoopOverPoints(field, IR_CompoundAssignment(Duplicate(baseAccess), IR_FunctionCall("randn"), IR_BinaryOperators.Addition))
        innerLoop.parallelization.potentiallyParallel = false
        stmts += innerLoop
      } else {
        val finerField = IR_FieldCollection.getByIdentifier(s"node_pos", level + 1).get
        val finerIndex = IR_LoopOverDimensions.defIt(HACK_numDims)
        finerIndex.indices ++= Array[IR_Expression](dim, 0)
        val finerAccess = IR_FieldAccess(IR_FieldSelection(finerField, finerField.level, 0), finerIndex)

        // on all levels but the finest: inject positions from finer level
        stmts += IR_LoopOverPoints(field, IR_Assignment(Duplicate(baseAccess), finerAccess))
      }
    }

    stmts
  }
}
