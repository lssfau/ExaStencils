package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.StatementList
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.grid.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_GeneralSimplify

case class IR_QuadraticInterpPackingC2FRemote(
    var send : Boolean,
    var field : IR_Field,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_RemotePackInfo,
    var concurrencyId : Int,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  def numDims = field.layout.numDimsData

  def defIt = IR_LoopOverDimensions.defIt(numDims)

  def commDir : Array[Int] = packInfo.neighDir

  def invCommDir : Array[Int] = packInfo.inverseNeighDir

  def isUpwindDir(dir : Array[Int]) = dir(getDimFromDir(dir)) > 0

  def orthogonalNeighborDirs() = {
    DefaultNeighbors.neighbors.map(_.dir).filter(dir => !(dir sameElements commDir) && !(dir sameElements invCommDir))
  }

  def isAtBlockCornersForDim(dim : Int) : IR_Expression = {
    val ival = packInfo.getPackInterval()

    // onion-peel of one layer in pack info interval
    ival.begin(dim) != defIt(dim) || ival.end(dim) != defIt(dim)
  }

  def isAtBlockCornersForDir(dir : Array[Int]) : IR_Expression = {
    val ival = packInfo.getPackInterval()

    val dim = getDimFromDir(dir)

    if (isUpwindDir(dir))
      ival.end(dim) != defIt(dim)
    else
      ival.begin(dim) != defIt(dim)
  }

  def isAtBlockCorners() : IR_Expression = {
    (0 until Knowledge.dimensionality - 1).map(d => isAtBlockCornersForDim(d)).reduce(_ OrOr _)
  }

  sealed case class BaseShift(var remappable : Boolean, var i0 : Int, var i1 : Int, var i2 : Int) {
    private val toArray = Array(i0, i1, i2)

    def scaleDir(dir : Array[Int], scale : Int) : Array[IR_Expression] = dir.map(_ * scale : IR_Expression)

    def toOffsetArrays(dir : Array[Int]) : Array[Array[IR_Expression]] = {
      val offsetDir = if (remappable) dir.map(_ * -1) else dir
      Array(scaleDir(offsetDir, toArray(0)), scaleDir(offsetDir, toArray(1)), scaleDir(offsetDir, toArray(2)))
    }
  }

  sealed case class BasePositions(var x0 : IR_Expression, var x1 : IR_Expression, var x2 : IR_Expression) {
    private val asArray : Array[IR_Expression] = Array(x0, x1, x2)

    def computeWeights(pos : IR_Expression) : Array[IR_Expression] = {
      def factors(j : Int) = {
        for (k <- asArray.indices if k != j) yield (pos - asArray(k)) / (asArray(j) - asArray(k)) : IR_Expression
      }

      val ret = for (baseIdx <- asArray.indices) yield factors(baseIdx).reduce(_ * _ : IR_Expression)
      for (e <- ret)
        IR_GeneralSimplify.doUntilDoneStandalone(e)

      ret.toArray
    }
  }

  sealed case class BaseValues(var f0 : IR_Expression, var f1 : IR_Expression, var f2 : IR_Expression) {
    private val asArray : Array[IR_Expression] = Array(f0, f1, f2)

    def toArray : Array[IR_Expression] = asArray
  }

  def interpolate(x0 : IR_Expression, basePos : BasePositions, baseVals : BaseValues) : IR_Expression = {
    basePos.computeWeights(x0).zip(baseVals.toArray).map(e => e._1 * e._2 : IR_Expression).reduce(_ + _)
  }

  def getDimFromDir(dir : Array[Int]) : Int = {
    if (dir.map(math.abs).sum != 1)
      Logger.error("Invalid direction used for quadratic interp: " + dir.mkString("Array(", ", ", ")"))

    // assume only axis directions -> first non-zero entry of array is requested dimension
    dir.indexWhere(_ != 0)
  }

  def getCellCenter(dir : Array[Int], origin : IR_ExpressionIndex, offset : IR_ExpressionIndex = IR_ExpressionIndex(0)) : IR_VirtualFieldAccess =
    IR_VF_CellCenterPerDim.access(field.level, getDimFromDir(dir), origin + offset)

  def getCellWidth(dir : Array[Int], origin : IR_ExpressionIndex, offset : IR_ExpressionIndex = IR_ExpressionIndex(0)) : IR_VirtualFieldAccess =
    IR_VF_CellWidthPerDim.access(field.level, getDimFromDir(dir), origin + offset)

  def getBasePositions(dir : Array[Int], origin : IR_ExpressionIndex, shifts : BaseShift) : BasePositions = {
    val offsets = shifts.toOffsetArrays(dir)

    field.localization match {
      case IR_AtCellCenter =>
        BasePositions(
          0, // set origin to current cell
          getCellCenter(dir, origin, IR_ExpressionIndex(offsets(1))) - getCellCenter(dir, origin, IR_ExpressionIndex(offsets(0))),
          getCellCenter(dir, origin, IR_ExpressionIndex(offsets(2))) - getCellCenter(dir, origin, IR_ExpressionIndex(offsets(0)))
        )
      case _               =>
        Logger.error("Unsupported localization for quadratic interp in comm for mesh refinement.")
    }
  }

  def getBaseValues(dir : Array[Int], origin : IR_ExpressionIndex, shifts : BaseShift) : BaseValues = {
    val offsets = shifts.toOffsetArrays(dir)

    BaseValues(
      IR_DirectFieldAccess(field, Duplicate(slot), origin + IR_ExpressionIndex(offsets(0))),
      IR_DirectFieldAccess(field, Duplicate(slot), origin + IR_ExpressionIndex(offsets(1))),
      IR_DirectFieldAccess(field, Duplicate(slot), origin + IR_ExpressionIndex(offsets(2)))
    )
  }

  // shifts for accessing cell centers for interpolation with bases at [-h, 0, h]
  private val CenteredBasesShifts = BaseShift(remappable = false, -1, 0, 1)

  // shifts for accessing cell centers for extrapolation/interpolation with bases at [0, h, 2h]
  // these shifts are chosen such that values are not reconstructed with values from the ghost layers
  private val RemappedBasesShifts = BaseShift(remappable = true, 0, 1, 2)

  def getBasePositionsForExtrapolation(dir : Array[Int], origin : IR_ExpressionIndex) : BasePositions =
    getBasePositions(dir, origin, RemappedBasesShifts)

  def getBasePositionsForInterpolation(dir : Array[Int], origin : IR_ExpressionIndex) : BasePositions =
    getBasePositions(dir, origin, CenteredBasesShifts)

  override def expand() : Output[StatementList] = {
    if (condition.isDefined)
      Logger.error("Conditions for refined communication are not supported yet.")

    val neighbor = packInfo.neighbor
    val neighborIdx = neighbor.index
    val indices = packInfo.getPackInterval()

    var ret = ListBuffer[IR_Statement]()

    def itName = if (send) "Send" else "Recv"

    def commBuffer = IR_IV_CommBuffer(field, s"${ itName }_${ concurrencyId }", indices.getTotalSize, neighborIdx)

    val fieldAccess = IR_DirectFieldAccess(field, Duplicate(slot), defIt)

    val tmpBufAccess = IR_TempBufferAccess(commBuffer,
      IR_ExpressionIndex(defIt, indices.begin, _ - _),
      IR_ExpressionIndex(indices.end, indices.begin, _ - _))

    var innerStmt : IR_Statement = IR_NullStatement
    if (send) {
      // TODO: add pack loop with interp kernel

      // Step 1 (1D interp): get value on neighboring fine ghost layer by extrapolating coarse cells in inverse comm direction
      val basePos = getBasePositionsForExtrapolation(commDir, defIt)
      val baseVals = getBaseValues(commDir, defIt, RemappedBasesShifts)
      val x0 = (if (isUpwindDir(commDir)) +1 else -1) * getCellWidth(commDir, defIt) / IR_RealConstant(4) // target location at: +/- 0.25h

      val f0_ext = IR_VariableAccess("f0_ext", IR_RealDatatype)
      innerStmt = IR_VariableDeclaration(f0_ext, interpolate(x0, basePos, baseVals))

      // Step 2: also extrapolate new bases for orthogonal face directions
      for (orthoDir <- orthogonalNeighborDirs()) {
        val baseValsRemapped = getBaseValues(commDir, defIt + IR_ExpressionIndex(orthoDir), RemappedBasesShifts)
        val baseValsNeigh = getBaseValues(commDir, defIt + IR_ExpressionIndex(orthoDir), CenteredBasesShifts)

        IR_IfCondition(isAtBlockCornersForDir(orthoDir),
          // true -> remap
          IR_NullStatement, // TODO
          // false -> regular handling
          IR_NullStatement // TODO
        )
      }

    } else {
      // TODO: add unpack loop
    }

    val loop = new IR_LoopOverDimensions(numDims, indices, ListBuffer(innerStmt), condition = condition)
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true
    ret += loop

    ret
  }
}

case class IR_QuadraticInterpPackingC2FLocal(
    var send : Boolean,
    var field : IR_Field,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_LocalPackInfo,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  def numDims = field.layout.numDimsData

  override def expand() : OutputType = {
    if (condition.isDefined)
      Logger.error("Conditions for refined communication are not supported yet.")

    val packIntervalDest = packInfo.getPackIntervalDest()
    val packIntervalSrc = packInfo.getPackIntervalSrc()

    val neighbor = packInfo.neighbor
    val domainIdx = field.domain.index
    val neighborIdx = neighbor.index

    var innerStmt : IR_Statement = IR_NullStatement
    if (send) {
      // TODO: add pack loop with interp kernel
    } else {
      // TODO: add unpack loop
    }

    val loop = new IR_LoopOverDimensions(numDims, packIntervalDest, ListBuffer[IR_Statement](innerStmt))
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true

    loop
  }
}

