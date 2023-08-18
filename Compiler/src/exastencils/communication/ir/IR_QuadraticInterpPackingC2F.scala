package exastencils.communication.ir

import scala.collection.immutable.ListMap
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.StatementList
import exastencils.domain.ir._
import exastencils.fieldlike.ir._
import exastencils.logger.Logger

/// QuadraticInterpPackingC2FHelper
// helper for generating statements with quadratic interp for local/remote comm

object QuadraticInterpPackingC2FHelper {
  import IR_InterpPackingHelper._
  import IR_InterpPackingBaseHelper._

  /* shift helper variables */

  // shifts for accessing cell centers for interpolation with bases at [-h, 0, h]
  private val CenteredBasesShifts = QuadraticBaseShifts(remapped = false)

  // shifts for accessing cell centers for extrapolation/interpolation with bases at [0, h, 2h]
  // these shifts are chosen such that values are not reconstructed with values from the ghost layers
  private val RemappedBasesShifts = QuadraticBaseShifts(remapped = true)

  /* helper functions */

  // wrapper to check for equality of conditions
  def isSameCondition(condA : IR_Expression, condB : IR_Expression) : Boolean = (condA, condB) match {
    case (c1 : IR_AndAnd, c2 : IR_AndAnd) if c1.left == c2.right && c1.right == c2.left => true
    case (c1 : IR_Expression, c2 : IR_Expression) if c1 == c2                           => true
    case _                                                                              => false
  }

  /* quadratic lagrange extra-/interpolation for send */

  def generateInterpStmts(results : Array[IR_VariableAccess], field : IR_FieldLike, slot : IR_Expression, packInfo : IR_PackInfo) : ListBuffer[IR_Statement] = {
    def level : Int = field.level

    def localization = field.localization

    def isRegularCase() : IR_Expression = IR_BooleanConstant(true)

    def numDims : Int = field.layout.numDimsData

    def defIt : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(numDims)

    def getBasePositionsForExtrapolation(dir : Array[Int], origin : IR_ExpressionIndex) : BasePositions =
      getBasePositions(level, localization, dir, origin, RemappedBasesShifts)

    def getBasePositionsForInterpolation(dir : Array[Int], origin : IR_ExpressionIndex) : BasePositions =
      getBasePositions(level, localization, dir, origin, CenteredBasesShifts)

    def isAtBlockCornerForDir3D(commDir : Array[Int], orthoDir : Array[Int], min : Boolean) : IR_Expression = {
      val ival = packInfo match {
        case rp : IR_RemotePackInfo => rp.getPackInterval()
        case lp : IR_LocalPackInfo  => lp.getPackIntervalSrc()
      }

      val commDirDim = getDimFromDir(commDir)
      val orthoDirDim = getDimFromDir(orthoDir)
      val remainingDim = ((0 until 3).toSet diff Set(commDirDim, orthoDirDim)).head

      isAtBlockCornerForDir2D(orthoDir) AndAnd (defIt(remainingDim) EqEq (if (min) ival.begin(remainingDim) else (ival.end(remainingDim) - 1)))
    }

    def isAtBlockCornerForDir2D(orthoDir : Array[Int]) : IR_Expression = {
      val ival = packInfo match {
        case rp : IR_RemotePackInfo => rp.getPackInterval()
        case lp : IR_LocalPackInfo  => lp.getPackIntervalSrc()
      }

      val dim = getDimFromDir(orthoDir)

      if (isUpwindDir(orthoDir))
        defIt(dim) EqEq (ival.end(dim) - 1)
      else
        defIt(dim) EqEq ival.begin(dim)
    }

    def commDir : Array[Int] = packInfo.neighDir

    def invCommDir : Array[Int] = commDir.map(_ * -1)

    def commDirDim : Int = getDimFromDir(commDir)

    // generate statements
    var innerStmts : ListBuffer[IR_Statement] = ListBuffer()

    // Step 1 (1D): get value on neighboring fine ghost layer by extrapolating coarse cells in inverse comm direction
    val basePosInvCommDir = getBasePositionsForExtrapolation(invCommDir, defIt)
    val baseValsInvCommDir = getBaseValues(field, slot, invCommDir, defIt, RemappedBasesShifts)
    val x0 = IR_RealConstant(-0.25) * getCellWidth(level, commDirDim, defIt) // target location at: -0.25h

    val f0 = IR_VariableAccess("f0_center_ext", IR_RealDatatype)
    innerStmts += IR_VariableDeclaration(f0, interpolate1D(x0, basePosInvCommDir, baseValsInvCommDir))

    // Step 2: also compute new (extrapolated) neighbor bases in orthogonal axis directions (2 dirs in 2D, 4 in 4D)

    // store conditions, extrapolation results and statements for each case (3 in 2D, 9 in 3D)
    var extrapResults : HashMap[Array[Int], ListBuffer[(IR_Expression, IR_Expression)]] = HashMap()

    val orthogonalNeighDirs = getOrthogonalNeighborDirs(commDir)
    var stencilAdaptedForCase : HashMap[IR_Expression, ListBuffer[(Array[Int], Boolean)]] = HashMap()
    for (orthoDir <- orthogonalNeighDirs) {
      val remappedOrthoDir = orthoDir.map(_ * -2)
      val remappedOrthoBaseVals = getBaseValues(field, slot, invCommDir, defIt + IR_ExpressionIndex(remappedOrthoDir), RemappedBasesShifts)
      val remappedOrthoBasePosCommDir = getBasePositionsForExtrapolation(invCommDir, defIt + IR_ExpressionIndex(remappedOrthoDir))

      // target location at: -0.25h
      val x0_ortho = IR_RealConstant(-0.25) * getCellWidth(level, commDirDim, defIt + IR_ExpressionIndex(orthoDir))
      val x0_remapOrtho = IR_RealConstant(-0.25) * getCellWidth(level, commDirDim, defIt + IR_ExpressionIndex(remappedOrthoDir))

      def markStencilAdapted(currCase : IR_Expression, adapted : Boolean) : Unit = {
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
      val orthoBaseValsInvCommDir = getBaseValues(field, slot, invCommDir, defIt + IR_ExpressionIndex(orthoDir), RemappedBasesShifts)

      casesForOrthoDir += (isRegularCase() -> interpolate1D(x0_ortho, orthoBasePosInvCommDir, orthoBaseValsInvCommDir))

      markStencilAdapted(isRegularCase(), adapted = false)

      extrapResults += (orthoDir -> casesForOrthoDir)
    }

    // aggregate cases by condition ...
    var cases : HashMap[IR_Expression, HashMap[Array[Int], IR_Expression]] = HashMap()
    extrapResults foreach {
      case (orthoDir, orthoCases) =>
        orthoCases foreach {
          case (cond, interp) =>
            val key = cases.keys.find(isSameCondition(_, cond)).getOrElse(cond)
            cases = cases.updated(key, cases.getOrElse(key, HashMap()) + (orthoDir -> Duplicate(interp)))
        }
    }
    // ... and fold in regular interp case for non-remapped orthogonal directions
    for ((cond, interps) <- cases if cond != isRegularCase()) {
      val regularInterp = cases(isRegularCase())
      for ((orthoDirRegular, interpRegular) <- regularInterp if !interps.exists(e => e._1 sameElements orthoDirRegular)) {
        cases = cases.updated(cond, cases(cond) + (orthoDirRegular -> Duplicate(interpRegular)))
      }
    }

    def stencilAdapted(cond : IR_Expression, dir : Array[Int]) =
      stencilAdaptedForCase(cond).exists(e => (e._1 sameElements dir) && e._2)

    // Step 3 (2D): extrap bases already built -> extrap-/interpolation in second dir
    var commonExtrapBases : ListMap[IR_Expression, IR_VariableAccess] = ListMap()

    def fetchOrAddExtrapBase(extrapExpr : IR_Expression) : IR_VariableAccess = {
      if (!commonExtrapBases.contains(extrapExpr)) {
        val ret = Duplicate(extrapExpr) -> IR_VariableAccess(s"f0_neighbor2D_${commonExtrapBases.size}_ext", IR_RealDatatype)
        commonExtrapBases += ret
        ret._2
      } else {
        commonExtrapBases(extrapExpr)
      }
    }

    val fillStmtsPerCase = cases.map { case (cond, interps) =>
      val interpStmts : ListBuffer[IR_Statement] = ListBuffer()

      def interpolate2D(
          center : IR_ExpressionIndex,
          centerValue : IR_Expression,
          upwindDir : Array[Int], downwindDir : Array[Int],
          upwindValue : IR_Expression, downwindValue : IR_Expression) = {

        // target location at +0.25h for interp, -0.25h for extrap
        val dirDim = getDimFromDir(upwindDir)
        val x1_int = getCellWidth(level, dirDim, center) / IR_RealConstant(4)
        val x1_ext = -1 * getCellWidth(level, dirDim, center) / IR_RealConstant(4)

        val basePosOrthoDirExt = getBasePositionsForExtrapolation(upwindDir, center)
        val basePosOrthoDirInt = getBasePositionsForInterpolation(upwindDir, center)

        // 3 Cases:
        // stencil adapted for upwind ortho dir -> bases at remapped ortho positions -> first value interp, second extrap
        // stencil adapted for downwind ortho dir -> bases at remapped ortho positions -> first value extrap, second interp
        // stencil NOT adapted -> bases at regular ortho positions -> both values interpolated
        if (stencilAdapted(cond, upwindDir)) {
          // upwind remap
          val remappedBaseVals = QuadraticBaseValues(centerValue, upwindValue, downwindValue)

          (interpolate1D(x1_int, basePosOrthoDirExt, remappedBaseVals),
            interpolate1D(x1_ext, basePosOrthoDirExt, remappedBaseVals))
        } else if (stencilAdapted(cond, downwindDir)) {
          // downwind remap
          val remappedBaseVals = QuadraticBaseValues(centerValue, upwindValue, downwindValue)

          (interpolate1D(x1_ext, basePosOrthoDirExt, remappedBaseVals),
            interpolate1D(x1_int, basePosOrthoDirExt, remappedBaseVals))
        } else {
          // no remap
          val baseValsForInterp = QuadraticBaseValues(downwindValue, centerValue, upwindValue)

          (interpolate1D(x1_ext, basePosOrthoDirInt, baseValsForInterp),
            interpolate1D(x1_int, basePosOrthoDirInt, baseValsForInterp))
        }
      }

      // fetch orthogonal upwind/downwind neighbors (in second dir) and their corresponding extrap values ...
      val orthoDirDownwind2D = orthogonalNeighDirs(0)
      val orthoDirUpwind2D = orthogonalNeighDirs(1)

      val f0_upwind2D = fetchOrAddExtrapBase(interps(orthoDirUpwind2D))
      val f0_downwind2D = fetchOrAddExtrapBase(interps(orthoDirDownwind2D))

      // ... and perform another quadratic interp/extrap in second direction
      val (f1_val, f2_val) = interpolate2D(defIt, f0, orthoDirUpwind2D, orthoDirDownwind2D, f0_upwind2D, f0_downwind2D)

      Knowledge.dimensionality match {
        case 2 =>
          // write 2D results into variables
          interpStmts += IR_Assignment(results(0), f1_val)
          interpStmts += IR_Assignment(results(1), f2_val)
        case 3 =>
          // declare 2D interpolated values of first upwind orthogonal neighbor
          val f1 = fetchOrAddExtrapBase(f1_val)
          val f2 = fetchOrAddExtrapBase(f2_val)

          // fetch orthogonal upwind/downwind neighbors (in third dir) and their corresponding extrap values
          val orthoDirDownwind3D = orthogonalNeighDirs(2)
          val orthoDirUpwind3D = orthogonalNeighDirs(3)

          val f0_upwind3D = fetchOrAddExtrapBase(interps(orthoDirUpwind3D))
          val f0_downwind3D = fetchOrAddExtrapBase(interps(orthoDirDownwind3D))

          // get directions to diagonal neighbor cells
          val diagOrigins : Array[Array[Int]] = {
            def adaptForRemap(d : Array[Int]) = if (stencilAdapted(cond, d)) d.map(_ * -2) else d

            Array(
              dirSum(adaptForRemap(orthoDirUpwind3D), adaptForRemap(orthoDirUpwind2D)),
              dirSum(adaptForRemap(orthoDirUpwind3D), adaptForRemap(orthoDirDownwind2D)),
              dirSum(adaptForRemap(orthoDirDownwind3D), adaptForRemap(orthoDirUpwind2D)),
              dirSum(adaptForRemap(orthoDirDownwind3D), adaptForRemap(orthoDirDownwind2D))
            )
          }

          // construct further extrap bases on diagonal neighbor cells
          val f0_diag : Array[IR_VariableAccess] = diagOrigins.map { dir =>
            val origin = defIt + IR_ExpressionIndex(dir)
            val basePosInvCommDir = getBasePositionsForExtrapolation(invCommDir, origin)
            val baseValsInvCommDir = getBaseValues(field, slot, invCommDir, origin, RemappedBasesShifts)

            fetchOrAddExtrapBase(interpolate1D(x0, basePosInvCommDir, baseValsInvCommDir))
          }

          val (f3_val, f4_val) = interpolate2D(defIt + IR_ExpressionIndex(orthoDirUpwind3D), f0_upwind3D,
            orthoDirUpwind2D, orthoDirDownwind2D, f0_diag(0), f0_diag(1))

          val (f5_val, f6_val) = interpolate2D(defIt + IR_ExpressionIndex(orthoDirDownwind3D), f0_downwind3D,
            orthoDirUpwind2D, orthoDirDownwind2D, f0_diag(2), f0_diag(3))

          val f3 = fetchOrAddExtrapBase(f3_val)
          val f4 = fetchOrAddExtrapBase(f4_val)
          val f5 = fetchOrAddExtrapBase(f5_val)
          val f6 = fetchOrAddExtrapBase(f6_val)

          // perform another quadratic interp/extrap in third dimension
          val (f7_val, f8_val) = interpolate2D(defIt, f1, orthoDirUpwind3D, orthoDirDownwind3D, f5, f3)
          val (f9_val, f10_val) = interpolate2D(defIt, f2, orthoDirUpwind3D, orthoDirDownwind3D, f6, f4)

          // write 3D results into variables
          interpStmts += IR_Assignment(results(0), f7_val)
          interpStmts += IR_Assignment(results(1), f8_val)
          interpStmts += IR_Assignment(results(2), f9_val)
          interpStmts += IR_Assignment(results(3), f10_val)
      }

      IR_IfCondition(cond, interpStmts)
    }

    // add declarations for variables storing common subexpressions
    commonExtrapBases.foreach { case (initVal, varAcc) => innerStmts += IR_VariableDeclaration(varAcc, initVal) }

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
  }
}

case class IR_QuadraticInterpPackingC2FRemote(
    var send : Boolean,
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_RemotePackInfo,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[Int],
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  import IR_InterpPackingHelper._

  def numDims : Int = field.layout.numDimsData

  def defIt : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(numDims)

  def commDir : Array[Int] = packInfo.neighDir

  override def expand() : Output[StatementList] = {
    if (condition.isDefined)
      Logger.error("Conditions for refined communication are not supported yet.")

    val neighbor = packInfo.neighbor
    val neighborIdx = neighbor.index
    val indices = packInfo.getPackInterval()

    var ret = ListBuffer[IR_Statement]()

    def it = IR_IV_CommBufferIterator(field, send, neighborIdx, concurrencyId, indexOfRefinedNeighbor)

    def commBuffer = IR_IV_CommBuffer(field, send, indices.getTotalSize, neighborIdx, concurrencyId, indexOfRefinedNeighbor)

    val tmpBufAccess = IR_TempBufferAccess(commBuffer,
      IR_ExpressionIndex(it), IR_ExpressionIndex(0) /* dummy stride */)

    var innerStmts : ListBuffer[IR_Statement] = ListBuffer()

    // init temp buf idx counter
    ret += IR_Assignment(it, 0)

    if (send) {
      // store final interp results for fine ghost neighbor cells (2 in 2D, 4 in 3D)
      val interpResults = (0 until Knowledge.refinement_maxFineNeighborsForCommAxis).map(i =>
        IR_VariableAccess(s"finalInterpResult2D_$i", IR_RealDatatype)).toArray

      // declare variables for final interp results
      for (res <- interpResults)
        innerStmts += IR_VariableDeclaration(res)

      // perform computation and fill variables
      innerStmts ++= QuadraticInterpPackingC2FHelper.generateInterpStmts(interpResults, field, slot, packInfo)

      // write result variables to buffer
      for (res <- interpResults) {
        innerStmts += IR_Assignment(tmpBufAccess, res)
        innerStmts += IR_PreIncrement(it)
      }
    } else {
      // interp/extrap values from coarse neighbor are written in order
      // of the upwind orthogonal dirs (in regards to comm dir) and their cross sums

      // fetch upwind orthogonal directions and their cross sum
      val upwindOrthogonals = getOrthogonalNeighborDirs(commDir).filter(isUpwindDir)
      val crossSumUpwindOrthogonals = Knowledge.dimensionality match {
        case 2 =>
          ListBuffer(Array.fill(3)(0), upwindOrthogonals(0))
        case 3 =>
          ListBuffer(Array.fill(3)(0), upwindOrthogonals(0), upwindOrthogonals(1), dirSum(upwindOrthogonals(0), upwindOrthogonals(1)))
      }

      // read from buffer into field
      for (offset <- crossSumUpwindOrthogonals.distinct) {
        innerStmts += IR_Assignment(
          IR_DirectFieldLikeAccess(field, Duplicate(slot), defIt + IR_ExpressionIndex(offset)),
          tmpBufAccess)
        innerStmts += IR_PreIncrement(it)
      }
    }

    // 2 values per dim written from coarse neighbor to fine receiver
    val stride = if (send) null else IR_ExpressionIndex(Array.fill(Knowledge.dimensionality)(2).updated(getDimFromDir(commDir), 1))

    val loop = new IR_LoopOverDimensions(numDims, indices, innerStmts, stride, condition = condition)
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true
    ret += loop

    ret
  }
}

case class IR_QuadraticInterpPackingC2FLocal(
    var send : Boolean,
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_LocalPackInfo,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  import IR_InterpPackingHelper._

  def numDims : Int = field.layout.numDimsData

  override def expand() : OutputType = {
    if (condition.isDefined)
      Logger.error("Conditions for refined communication are not supported yet.")

    // TODO: pull scheme for local comm, only push implemented
    if (!Knowledge.comm_pushLocalData)
      Logger.warn("Pull comm scheme is not yet implemented for quadratic C2F interp.")

    val packIntervalDest = packInfo.getPackIntervalDest()
    val packIntervalSrc = packInfo.getPackIntervalSrc()

    val neighbor = packInfo.neighbor
    val domainIdx = field.domain.index
    val neighborIdx = neighbor.index

    var innerStmts : ListBuffer[IR_Statement] = ListBuffer()

    // store final interp results for fine ghost neighbor cells (2 in 2D, 4 in 3D)
    val interpResults = (0 until Knowledge.refinement_maxFineNeighborsForCommAxis).map(i =>
      IR_VariableAccess(s"finalInterpResult2D_$i", IR_RealDatatype)).toArray

    // declare variables for final interp results
    for (res <- interpResults)
      innerStmts += IR_VariableDeclaration(res)

    // perform computation and fill variables
    innerStmts ++= QuadraticInterpPackingC2FHelper.generateInterpStmts(interpResults, field, slot, packInfo)

    // push result to destination
    for (res <- interpResults) {
      innerStmts += IR_Assignment(
        IR_DirectFieldLikeAccess(field, Duplicate(slot), IR_IV_NeighborFragmentIdx(domainIdx, neighborIdx), IR_ExpressionIndex(
          IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), packIntervalSrc.begin, _ + _), packIntervalDest.begin, _ - _)),
        res)
    }

    // 2 values per dim written from coarse neighbor to fine receiver
    val stride = IR_ExpressionIndex(Array.fill(Knowledge.dimensionality)(2).updated(getDimFromDir(packInfo.neighDir), 1))

    val loop = new IR_LoopOverDimensions(numDims, packIntervalDest, innerStmts, stride, condition = condition)
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true

    loop
  }
}

