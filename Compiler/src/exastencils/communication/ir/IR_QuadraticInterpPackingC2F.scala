package exastencils.communication.ir

import scala.collection.immutable.HashMap
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

  def getOrthogonalNeighborDirs() = {
    DefaultNeighbors.neighbors.map(_.dir).filter(dir => !(dir sameElements commDir) && !(dir sameElements invCommDir))
  }

  def isRegularCase() : IR_Expression = IR_BooleanConstant(true)

  def isAtBlockCornerForDir3D(commDir : Array[Int], orthoDir : Array[Int], min : Boolean) : IR_Expression = {
    val ival = packInfo.getPackInterval()

    val commDirDim = getDimFromDir(commDir)
    val orthoDirDim = getDimFromDir(orthoDir)
    val remainingDim = ((0 until 3).toSet diff Set(commDirDim, orthoDirDim)).head

    isAtBlockCornerForDir2D(orthoDir) AndAnd (defIt(remainingDim) EqEq (if (min) ival.begin(remainingDim) else (ival.end(remainingDim) - 1)))
  }

  def isAtBlockCornerForDir2D(orthoDir : Array[Int]) : IR_Expression = {
    val ival = packInfo.getPackInterval()

    val dim = getDimFromDir(orthoDir)

    if (isUpwindDir(orthoDir))
      defIt(dim) EqEq (ival.end(dim) - 1)
    else
      defIt(dim) EqEq ival.begin(dim)
  }

  sealed case class BaseShift(var remapped : Boolean) {
    def toArray = if (remapped) Array(0, 1, 2) else Array(-1, 0, 1)

    def scaleDir(dir : Array[Int], scale : Int) : Array[IR_Expression] = dir.map(_ * scale : IR_Expression)

    def toOffsetArrays(dir : Array[Int]) : Array[Array[IR_Expression]] = {
      Array(scaleDir(dir, toArray(0)), scaleDir(dir, toArray(1)), scaleDir(dir, toArray(2)))
    }
  }

  sealed case class BasePositions(var x0 : IR_Expression, var x1 : IR_Expression, var x2 : IR_Expression) {
    private val asArray : Array[IR_Expression] = Array(Duplicate(x0), Duplicate(x1), Duplicate(x2))

    def computeWeights(pos : IR_Expression) : Array[IR_Expression] = {
      def factors(j : Int) = {
        for (k <- asArray.indices if k != j) yield (pos - asArray(k)) / (asArray(j) - asArray(k)) : IR_Expression
      }

      (for (baseIdx <- asArray.indices) yield factors(baseIdx).reduce(_ * _ : IR_Expression)).toArray

      // TODO: weights wrong when simplified here
      /*val ret = asArray.indices.map(i => IR_Multiplication(factors(i) : _*))
      for (e <- ret)
        IR_GeneralSimplify.doUntilDoneStandalone(IR_ExpressionStatement(e))

      ret.toArray*/
    }
  }

  sealed case class BaseValues(var f0 : IR_Expression, var f1 : IR_Expression, var f2 : IR_Expression) {
    private val asArray : Array[IR_Expression] = Array(Duplicate(f0), Duplicate(f1), Duplicate(f2))

    def toArray : Array[IR_Expression] = asArray
  }

  def interpolate1D(x0 : IR_Expression, basePos : BasePositions, baseVals : BaseValues) : IR_Expression = {
    basePos.computeWeights(x0).zip(baseVals.toArray).map(e => e._1 * e._2 : IR_Expression).reduce(_ + _)
  }

  def getDimFromDir(dir : Array[Int]) : Int = {
    if (dir.map(math.abs).sum != 1)
      Logger.error("Invalid direction used for quadratic interp: " + dir.mkString("Array(", ", ", ")"))

    // assume only axis directions -> first non-zero entry of array is requested dimension
    dir.indexWhere(_ != 0)
  }

  def getCellCenter(dim : Int, origin : IR_ExpressionIndex, offset : IR_ExpressionIndex = IR_ExpressionIndex(0)) : IR_VirtualFieldAccess =
    IR_VF_CellCenterPerDim.access(field.level, dim, origin + offset)

  def getCellWidth(dim : Int, origin : IR_ExpressionIndex, offset : IR_ExpressionIndex = IR_ExpressionIndex(0)) : IR_Expression =
    IR_VF_CellWidthPerDim.access(field.level, dim, origin + offset)

  def getBasePositions(dir : Array[Int], origin : IR_ExpressionIndex, shifts : BaseShift) : BasePositions = {
    val offsets = shifts.toOffsetArrays(dir)
    val dim = getDimFromDir(dir)

    val centerIdx = shifts.toArray.indexWhere(_ == 0)
    field.localization match {
      case IR_AtCellCenter =>
        if (Knowledge.grid_isUniform) {
          // set origin to center cell
          val off = shifts.toArray(centerIdx) * getCellWidth(dim, origin)

          BasePositions(
            shifts.toArray(0) * getCellWidth(dim, origin) - off,
            shifts.toArray(1) * getCellWidth(dim, origin) - off,
            shifts.toArray(2) * getCellWidth(dim, origin) - off
          )
        } else {
          // set origin to center cell
          val off = getCellCenter(dim, origin, IR_ExpressionIndex(offsets(centerIdx)))

          BasePositions(
            getCellCenter(dim, origin, IR_ExpressionIndex(offsets(0))) - off,
            getCellCenter(dim, origin, IR_ExpressionIndex(offsets(1))) - off,
            getCellCenter(dim, origin, IR_ExpressionIndex(offsets(2))) - off
          )
        }
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

  def dirToString(dir : Array[Int]) : String = dir match {
    case Array(-1, 0, 0) => "W"
    case Array(1, 0, 0)  => "E"
    case Array(0, -1, 0) => "S"
    case Array(0, 1, 0)  => "N"
    case Array(0, 0, -1) => "B"
    case Array(0, 0, 1)  => "T"
  }

  def isSameCondition(condA : IR_Expression, condB : IR_Expression) : Boolean = (condA, condB) match {
    case (c1 : IR_AndAnd, c2 : IR_AndAnd) if c1.left == c2.right && c1.right == c2.left => true
    case (c1 : IR_Expression, c2 : IR_Expression) if c1 == c2                           => true
    case _                                                                              => false
  }

  // shifts for accessing cell centers for interpolation with bases at [-h, 0, h]
  private val CenteredBasesShifts = BaseShift(remapped = false)

  // shifts for accessing cell centers for extrapolation/interpolation with bases at [0, h, 2h]
  // these shifts are chosen such that values are not reconstructed with values from the ghost layers
  private val RemappedBasesShifts = BaseShift(remapped = true)

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

    def it = IR_IV_CommBufferIterator(field, s"${ itName }_${ concurrencyId }", neighborIdx)

    def itName = if (send) "Send" else "Recv"

    def commBuffer = IR_IV_CommBuffer(field, s"${ itName }_${ concurrencyId }", indices.getTotalSize, neighborIdx)

    val tmpBufAccess = IR_TempBufferAccess(commBuffer,
      IR_ExpressionIndex(it), IR_ExpressionIndex(0) /* dummy stride */)

    var innerStmts : ListBuffer[IR_Statement] = ListBuffer()
    if (send) {
      // quadratic lagrange extra-/interpolation for send

      // init temp buf idx counter
      ret += IR_Assignment(it, 0)

      // Step 1 (1D): get value on neighboring fine ghost layer by extrapolating coarse cells in inverse comm direction

      val commDirDim = getDimFromDir(commDir)
      val invCommDir = commDir.map(_ * -1)
      val basePosInvCommDir = getBasePositionsForExtrapolation(invCommDir, defIt)
      val baseValsInvCommDir = getBaseValues(invCommDir, defIt, RemappedBasesShifts)
      val x0 = IR_RealConstant(-0.25) * getCellWidth(commDirDim, defIt) // target location at: -0.25h

      val f0 = IR_VariableAccess("f0_center_ext", IR_RealDatatype)
      innerStmts += IR_VariableDeclaration(f0, interpolate1D(x0, basePosInvCommDir, baseValsInvCommDir))

      // Step 2: also compute new (extrapolated) neighbor bases in orthogonal axis directions (2 dirs in 2D, 4 in 4D)

      // store conditions, extrapolation results and statements for each case (3 in 2D, 9 in 3D)
      var extrapResults : HashMap[Array[Int], ListBuffer[(IR_Expression, IR_Expression)]] = HashMap()

      val orthogonalNeighDirs = getOrthogonalNeighborDirs()
      var stencilAdaptedForCase : HashMap[IR_Expression, ListBuffer[(Array[Int], Boolean)]] = HashMap()
      for (orthoDir <- orthogonalNeighDirs) {
        val remappedOrthoDir = orthoDir.map(_ * -2)
        val remappedOrthoBaseVals = getBaseValues(invCommDir, defIt + IR_ExpressionIndex(remappedOrthoDir), RemappedBasesShifts)
        val remappedOrthoBasePosCommDir = getBasePositionsForExtrapolation(invCommDir, defIt + IR_ExpressionIndex(remappedOrthoDir))

        // target location at: -0.25h
        val x0_ortho = IR_RealConstant(-0.25) * getCellWidth(commDirDim, defIt + IR_ExpressionIndex(orthoDir))
        val x0_remapOrtho = IR_RealConstant(-0.25) * getCellWidth(commDirDim, defIt + IR_ExpressionIndex(remappedOrthoDir))

        def markStencilAdapted(currCase : IR_Expression, adapted : Boolean) = {
          stencilAdaptedForCase = stencilAdaptedForCase.updated(currCase,
            stencilAdaptedForCase.getOrElse(currCase, ListBuffer()) :+ (orthoDir -> adapted))
        }

        // cases where neighbor values for extrap has to be remapped
        var casesForOrthoDir : ListBuffer[(IR_Expression, IR_Expression)] = ListBuffer()
        Knowledge.dimensionality match {
          case 2 =>
            val isCorner = isAtBlockCornerForDir2D(orthoDir)

            casesForOrthoDir += (isCorner -> interpolate1D(x0_remapOrtho, remappedOrthoBasePosCommDir, remappedOrthoBaseVals))

            markStencilAdapted(isCorner, adapted = true)
          case 3 =>
            val isMinCorner = isAtBlockCornerForDir3D(commDir, orthoDir, min = true)
            val isMaxCorner = isAtBlockCornerForDir3D(commDir, orthoDir, min = false)

            casesForOrthoDir += (isMinCorner -> interpolate1D(x0_remapOrtho, remappedOrthoBasePosCommDir, remappedOrthoBaseVals))
            casesForOrthoDir += (isMaxCorner -> interpolate1D(x0_remapOrtho, remappedOrthoBasePosCommDir, remappedOrthoBaseVals))

            markStencilAdapted(isMinCorner, adapted = true)
            markStencilAdapted(isMaxCorner, adapted = true)

            val isEdge = isAtBlockCornerForDir2D(orthoDir)

            casesForOrthoDir += (isEdge -> interpolate1D(x0_remapOrtho, remappedOrthoBasePosCommDir, remappedOrthoBaseVals))

            markStencilAdapted(isEdge, adapted = true)
        }

        // regular case without remap in second direction
        val orthoBasePosInvCommDir = getBasePositionsForExtrapolation(invCommDir, defIt + IR_ExpressionIndex(orthoDir))
        val orthoBaseValsInvCommDir = getBaseValues(invCommDir, defIt + IR_ExpressionIndex(orthoDir), RemappedBasesShifts)

        casesForOrthoDir += (isRegularCase() -> interpolate1D(x0_ortho, orthoBasePosInvCommDir, orthoBaseValsInvCommDir))

        markStencilAdapted(isRegularCase(), adapted = false)

        extrapResults += (orthoDir -> casesForOrthoDir)
      }

      // aggregate cases by condition ...
      var cases : HashMap[IR_Expression, ListBuffer[(Array[Int], IR_Expression)]] = HashMap()
      extrapResults foreach {
        case (orthoDir, orthoCases) =>
          orthoCases foreach {
            case (cond, interp) =>
              val key = cases.keys.find(isSameCondition(_, cond)).getOrElse(cond)
              cases = cases.updated(key, cases.getOrElse(key, ListBuffer()) :+ (orthoDir -> interp))
          }
      }
      // ... and fold in regular interp case for non-remapped orthogonal directions
      for ((cond, interps) <- cases if cond != isRegularCase()) {
        val regularInterp = cases(isRegularCase())
        for ((orthoDirRegular, interpRegular) <- regularInterp if !interps.exists(e => e._1 sameElements orthoDirRegular)) {
          cases = cases.updated(cond, cases(cond) :+ (orthoDirRegular -> interpRegular))
        }
      }

      def pushToBuffer(value : IR_Expression) : ListBuffer[IR_Statement] = ListBuffer(
        IR_Assignment(tmpBufAccess, value),
        IR_PreIncrement(it) : IR_Statement
      )

      def stencilAdapted(cond : IR_Expression, dir : Array[Int]) =
        stencilAdaptedForCase(cond).exists(e => (e._1 sameElements dir) && e._2)

      // Step 3 (2D): extrap bases already built -> extrap-/interpolation in second dir
      val fillStmtsPerCase = cases.map { case (cond, interps) =>
        val interpStmts : ListBuffer[IR_Statement] = ListBuffer()

        def interpolate2D(
            center : IR_ExpressionIndex,
            centerValue : IR_Expression,
            upwindDir : Array[Int], downwindDir : Array[Int],
            upwindValue : IR_Expression, downwindValue : IR_Expression) = {

          // target location at +0.25h for interp, -0.25h for extrap
          val dirDim = getDimFromDir(upwindDir)
          val x1_int = getCellWidth(dirDim, center) / IR_RealConstant(4)
          val x1_ext = -1 * getCellWidth(dirDim, center) / IR_RealConstant(4)

          val basePosOrthoDirExt = getBasePositionsForExtrapolation(upwindDir, center)
          val basePosOrthoDirInt = getBasePositionsForInterpolation(upwindDir, center)

          // 3 Cases:
          // stencil adapted for upwind ortho dir -> bases at remapped ortho positions -> first value interp, second extrap
          // stencil adapted for downwind ortho dir -> bases at remapped ortho positions -> first value extrap, second interp
          // stencil NOT adapted -> bases at regular ortho positions -> both values interpolated
          if (stencilAdapted(cond, upwindDir)) {
            // upwind remap
            val remappedBaseVals = BaseValues(centerValue, upwindValue, downwindValue)

            (interpolate1D(x1_int, basePosOrthoDirExt, remappedBaseVals),
              interpolate1D(x1_ext, basePosOrthoDirExt, remappedBaseVals))
          } else if (stencilAdapted(cond, downwindDir)) {
            // downwind remap
            val remappedBaseVals = BaseValues(centerValue, upwindValue, downwindValue)

            (interpolate1D(x1_ext, basePosOrthoDirExt, remappedBaseVals),
              interpolate1D(x1_int, basePosOrthoDirExt, remappedBaseVals))
          } else {
            // no remap
            val baseValsForInterp = BaseValues(downwindValue, centerValue, upwindValue)

            (interpolate1D(x1_ext, basePosOrthoDirInt, baseValsForInterp),
              interpolate1D(x1_int, basePosOrthoDirInt, baseValsForInterp))
          }
        }

        // fetch orthogonal upwind/downwind neighbors (in second dir) and their corresponding extrap values ...
        val (orthoDirUpwind2D, f0_upwind2D) = interps(1)
        val (orthoDirDownwind2D, f0_downwind2D) = interps(0)

        val f0_upwind = IR_VariableAccess(s"f0_neighbor_${ dirToString(orthoDirUpwind2D) }_ext", IR_RealDatatype)
        val f0_downwind = IR_VariableAccess(s"f0_neighbor_${ dirToString(orthoDirDownwind2D) }_ext", IR_RealDatatype)

        interpStmts += IR_VariableDeclaration(f0_upwind, f0_upwind2D)
        interpStmts += IR_VariableDeclaration(f0_downwind, f0_downwind2D)

        // ... and perform another quadratic interp/extrap in second direction
        val (f1_val, f2_val) = interpolate2D(defIt, f0, orthoDirUpwind2D, orthoDirDownwind2D, f0_upwind2D, f0_downwind2D)

        Knowledge.dimensionality match {
          case 2 =>
            // write 2D results into buffer
            interpStmts ++= pushToBuffer(f1_val)
            interpStmts ++= pushToBuffer(f2_val)
          case 3 =>
            val f1 = IR_VariableAccess("f1_int", IR_RealDatatype)
            val f2 = IR_VariableAccess("f2_int", IR_RealDatatype)

            // declare 2D interpolated values of first upwind orthogonal neighbor
            if (orthoDirUpwind2D sameElements orthogonalNeighDirs(1)) {
              interpStmts += IR_VariableDeclaration(f1, f1_val)
              interpStmts += IR_VariableDeclaration(f2, f2_val)
            }

            // fetch orthogonal upwind/downwind neighbors (in third dir) and their corresponding extrap values
            val (orthoDirUpwind3D, f0_upwind3D) = interps(3)
            val (orthoDirDownwind3D, f0_downwind3D) = interps(2)

            // get directions to diagonal neighbor cells
            val diagOrigins : Array[Array[Int]] = {
              def adaptForRemap(d : Array[Int]) = if (stencilAdapted(cond, d)) d.map(_ * -2) else d

              def arrSum(a : Array[Int], b : Array[Int]) = (a, b).zipped.map(_ + _)

              Array(
                arrSum(adaptForRemap(orthoDirUpwind3D), adaptForRemap(orthoDirUpwind2D)),
                arrSum(adaptForRemap(orthoDirUpwind3D), adaptForRemap(orthoDirDownwind2D)),
                arrSum(adaptForRemap(orthoDirDownwind3D), adaptForRemap(orthoDirUpwind2D)),
                arrSum(adaptForRemap(orthoDirDownwind3D), adaptForRemap(orthoDirDownwind2D))
              )
            }

            // construct further extrap bases on diagonal neighbor cells
            val f0_diag : Array[IR_Expression] = diagOrigins.map { dir =>
              val origin = defIt + IR_ExpressionIndex(dir)
              val basePosInvCommDir = getBasePositionsForExtrapolation(invCommDir, origin)
              val baseValsInvCommDir = getBaseValues(invCommDir, origin, RemappedBasesShifts)

              interpolate1D(x0, basePosInvCommDir, baseValsInvCommDir)
            }

            val f3 = IR_VariableAccess("f3_int", IR_RealDatatype)
            val f4 = IR_VariableAccess("f4_int", IR_RealDatatype)
            val f5 = IR_VariableAccess("f5_int", IR_RealDatatype)
            val f6 = IR_VariableAccess("f6_int", IR_RealDatatype)

            val (f3_val, f4_val) = interpolate2D(defIt + IR_ExpressionIndex(orthoDirUpwind3D), f0_upwind3D,
              orthoDirUpwind2D, orthoDirDownwind2D, f0_diag(0), f0_diag(1))
            interpStmts += IR_VariableDeclaration(f3, f3_val)
            interpStmts += IR_VariableDeclaration(f4, f4_val)

            val (f5_val, f6_val) = interpolate2D(defIt + IR_ExpressionIndex(orthoDirDownwind3D), f0_downwind3D,
              orthoDirUpwind2D, orthoDirDownwind2D, f0_diag(2), f0_diag(3))
            interpStmts += IR_VariableDeclaration(f5, f5_val)
            interpStmts += IR_VariableDeclaration(f6, f6_val)

            // perform another quadratic interp/extrap in third dimension

            val (f7_val, f8_val) = interpolate2D(defIt, f1_val, orthoDirUpwind3D, orthoDirDownwind3D, f5_val, f3_val)
            val (f9_val, f10_val) = interpolate2D(defIt, f2_val, orthoDirUpwind3D, orthoDirDownwind3D, f6_val, f4_val)

            interpStmts ++= pushToBuffer(f7_val)
            interpStmts ++= pushToBuffer(f8_val)
            interpStmts ++= pushToBuffer(f9_val)
            interpStmts ++= pushToBuffer(f10_val)
        }

        IR_IfCondition(cond, interpStmts)
      }

      // sort cases to remove ambiguity ...
      val caseOrdering : Ordering[IR_IfCondition] = Ordering.by {
        case _ @ IR_IfCondition(_ : IR_AndAnd, _, _)             => 0 // corners in 3D
        case _ @ IR_IfCondition(_ : IR_EqEq, _, _)               => 1 // corners in 2D, edges in 3D
        case _ @ IR_IfCondition(b, _, _) if b == isRegularCase() => 3 // least precedence as it should be the "default case"
        case _                                                   => 2
      }

      // ... and fold into a single nested if-else construct
      val sortedCases = fillStmtsPerCase.toSeq.sorted(caseOrdering)
      innerStmts += sortedCases.dropRight(1).reverse.foldLeft(sortedCases.reverse.head)((c1, c2) => IR_IfCondition(c2.condition, c2.trueBody, c1))
    } else {
      // TODO: add unpack loop
    }

    val loop = new IR_LoopOverDimensions(numDims, indices, innerStmts, condition = condition)
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

