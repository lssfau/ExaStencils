package exastencils.communication.ir

import scala.collection.immutable.ListMap
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.ir.IR_InterpPackingHelper.dirSum
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.StatementList
import exastencils.domain.ir._
import exastencils.fieldlike.ir.IR_DirectFieldLikeAccess
import exastencils.fieldlike.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.EvaluationException
import exastencils.optimization.ir.IR_SimplifyExpression

/// LinearInterpPackingC2FHelper
// helper for generating statements with linear interp for local/remote comm

// TODO: formulate this and other-order schemes in a more flexible class

object LinearInterpPackingC2FHelper {

  import IR_InterpPackingHelper._
  import IR_InterpPackingBaseHelper._

  /* shift helper variables */

  // shifts for accessing cell centers for interpolation with bases at [-h, 0]
  private val CenteredBasesShifts = LinearBaseShifts(-1, 0)

  // shifts for accessing cell centers for extrapolation/interpolation with bases at [0, h]
  // these shifts are chosen such that values are not reconstructed with values from the ghost layers
  private val RemappedBasesShifts = LinearBaseShifts(0, 1)

  /* helper functions */

  // wrapper to check for equality of conditions
  def isSameCondition(condA : IR_Expression, condB : IR_Expression) : Boolean = (condA, condB) match {
    case (c1 : IR_AndAnd, c2 : IR_AndAnd) if c1.left == c2.right && c1.right == c2.left => true
    case (c1 : IR_Expression, c2 : IR_Expression) if c1 == c2                           => true
    case _                                                                              => false
  }

  /* linear lagrange extra-/interpolation for send */

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

    def getSplitPackInterval() = packInfo match {
      case rp : IR_RemotePackInfo => rp.getPackInterval()
      case lp : IR_LocalPackInfo  => lp.getPackIntervalDest()
      case _                      => Logger.error("Unsupported PackInfo type used in (cell-based) quadratic C2F interp")
    }

    def getUnsplitPackInterval() = packInfo match {
      case rp : IR_RefinementPackInfoGhost => rp.unsplitPackIntervalSend(commDir)
      case _                               => Logger.error("Unsupported PackInfo type used in (cell-based) quadratic C2F interp")
    }

    def isAtBlockCornerForDir3D(commDir : Array[Int], orthoDir : Array[Int], min : Boolean) : IR_Expression = {
      val unsplitIval = getUnsplitPackInterval()

      val commDirDim = getDimFromDir(commDir)
      val orthoDirDim = getDimFromDir(orthoDir)
      val remainingDim = ((0 until 3).toSet diff Set(commDirDim, orthoDirDim)).head

      isAtBlockCornerForDir2D(orthoDir) AndAnd (defIt(remainingDim) EqEq (if (min) unsplitIval.begin(remainingDim) else (unsplitIval.end(remainingDim) - 1)))
    }

    def isAtBlockCornerForDir2D(orthoDir : Array[Int]) : IR_Expression = {
      val unsplitIval = getUnsplitPackInterval()

      val dim = getDimFromDir(orthoDir)

      if (isUpwindDir(orthoDir))
        defIt(dim) EqEq (unsplitIval.end(dim) - 1)
      else
        defIt(dim) EqEq unsplitIval.begin(dim)
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

    val orthogonalNeighDirs = getOrthogonalNeighborDirs(commDir).filterNot(isUpwindDir)
    var stencilAdaptedForCase : HashMap[IR_Expression, ListBuffer[(Array[Int], Boolean)]] = HashMap()

    def markStencilAdapted(currCase : IR_Expression, orthoDir : Array[Int], adapted : Boolean) : Unit = {
      stencilAdaptedForCase = stencilAdaptedForCase.updated(currCase,
        stencilAdaptedForCase.getOrElse(currCase, ListBuffer()) :+ (orthoDir -> adapted))
    }

    for (orthoDir <- orthogonalNeighDirs) {
      val remappedOrthoDir = orthoDir.map(_ * -1)
      val remappedOrthoBaseVals = getBaseValues(field, slot, invCommDir, defIt + IR_ExpressionIndex(remappedOrthoDir), RemappedBasesShifts)
      val remappedOrthoBasePosCommDir = getBasePositionsForExtrapolation(invCommDir, defIt + IR_ExpressionIndex(remappedOrthoDir))

      // target location at: -0.25h
      val x0_ortho = IR_RealConstant(-0.25) * getCellWidth(level, commDirDim, defIt + IR_ExpressionIndex(orthoDir))
      val x0_remapOrtho = IR_RealConstant(-0.25) * getCellWidth(level, commDirDim, defIt + IR_ExpressionIndex(remappedOrthoDir))

      // cases where neighbor values for extrap has to be remapped
      var casesForOrthoDir : ListBuffer[(IR_Expression, IR_Expression)] = ListBuffer()
      Knowledge.dimensionality match {
        case 2 =>
          val isCorner = isAtBlockCornerForDir2D(orthoDir)

          casesForOrthoDir += (isCorner -> interpolate1D(x0_remapOrtho, remappedOrthoBasePosCommDir, remappedOrthoBaseVals))

          markStencilAdapted(isCorner, orthoDir, adapted = true)
        case 3 =>
          val isMinCorner = isAtBlockCornerForDir3D(commDir, orthoDir, min = true)

          casesForOrthoDir += (isMinCorner -> interpolate1D(x0_remapOrtho, remappedOrthoBasePosCommDir, remappedOrthoBaseVals))

          val remainingDirs = orthogonalNeighDirs.filter(dir => !(dir sameElements orthoDir) && !(dir sameElements orthoDir.map(_ * -1)))
          markStencilAdapted(isMinCorner, orthoDir, adapted = true)
          markStencilAdapted(isMinCorner, remainingDirs(0), adapted = true)

          val isEdge = isAtBlockCornerForDir2D(orthoDir)

          casesForOrthoDir += (isEdge -> interpolate1D(x0_remapOrtho, remappedOrthoBasePosCommDir, remappedOrthoBaseVals))

          markStencilAdapted(isEdge, orthoDir, adapted = true)
      }

      // regular case without remap in second direction
      val orthoBasePosInvCommDir = getBasePositionsForExtrapolation(invCommDir, defIt + IR_ExpressionIndex(orthoDir))
      val orthoBaseValsInvCommDir = getBaseValues(field, slot, invCommDir, defIt + IR_ExpressionIndex(orthoDir), RemappedBasesShifts)

      casesForOrthoDir += (isRegularCase() -> interpolate1D(x0_ortho, orthoBasePosInvCommDir, orthoBaseValsInvCommDir))

      markStencilAdapted(isRegularCase(), orthoDir, adapted = false)

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

    def caseCanBeOptimized(it : IR_VariableAccess, checkVal : IR_Expression) = {
      val splitInterval = getSplitPackInterval()
      val dim = (0 until numDims).indexWhere(d => IR_FieldIteratorAccess(d) == it)
      try {
        val evalCheck = IR_SimplifyExpression.evalIntegral(checkVal)
        val evalBoundBegin = IR_SimplifyExpression.evalIntegral(splitInterval.begin(dim))
        val evalBoundEnd = IR_SimplifyExpression.evalIntegral(splitInterval.end(dim))

        evalCheck < evalBoundBegin || evalCheck > evalBoundEnd
      } catch {
        case _ : EvaluationException =>
          Logger.warn("Could not evaluate bounds")
          false
      }
    }

    // optimize away cases that cannot occur since they are not contained in bounds of split loop
    cases = cases.map { case (cond, interp) =>
      cond match {
        case IR_EqEq(it : IR_VariableAccess, check) if caseCanBeOptimized(it, check)                                                                                             =>
          IR_BooleanConstant(false) -> interp
        case IR_AndAnd(IR_EqEq(it1 : IR_VariableAccess, check1), IR_EqEq(it2 : IR_VariableAccess, check2)) if caseCanBeOptimized(it1, check1) || caseCanBeOptimized(it2, check2) =>
          IR_BooleanConstant(false) -> interp
        case _                                                                                                                                                                   =>
          cond -> interp
      }
    }

    def stencilAdapted(cond : IR_Expression, dir : Array[Int]) =
      stencilAdaptedForCase.contains(cond) && stencilAdaptedForCase(cond).exists(e => (e._1 sameElements dir) && e._2)

    // Step 3 (2D): extrap bases already built -> extrap-/interpolation in second dir
    /*
    // TODO: find a better way to do CSE
    var commonExtrapBases : ListMap[IR_Expression, IR_VariableAccess] = ListMap()

    def fetchOrAddExtrapBase(extrapExpr : IR_Expression) : IR_VariableAccess = {
      if (!commonExtrapBases.contains(extrapExpr)) {
        val ret = Duplicate(extrapExpr) -> IR_VariableAccess(s"f0_neighbor2D_${ commonExtrapBases.size }_ext", IR_RealDatatype)
        commonExtrapBases += ret
        ret._2
      } else {
        commonExtrapBases(extrapExpr)
      }
    }
    */

    val fillStmtsPerCase = cases.map { case (cond, interps) =>
      val interpStmts : ListBuffer[IR_Statement] = ListBuffer()

      def interpolate2D(
          center : IR_ExpressionIndex,
          centerValue : IR_Expression,
          downwindDir : Array[Int],
          downwindValue : IR_Expression) = {

        // target location at +0.25h for interp, -0.25h for extrap
        val dirDim = getDimFromDir(downwindDir)
        val x1_int = getCellWidth(level, dirDim, center) / IR_RealConstant(4)
        val x1_ext = -1 * getCellWidth(level, dirDim, center) / IR_RealConstant(4)

        val basePosOrthoDirExt = getBasePositionsForExtrapolation(downwindDir, center)
        val basePosOrthoDirInt = getBasePositionsForInterpolation(downwindDir, center)

        // 3 Cases:
        // stencil adapted for downwind ortho dir -> bases at remapped ortho positions -> first value extrap, second interp
        // stencil NOT adapted -> bases at regular ortho positions -> first value interp, second extrap
        if (stencilAdapted(cond, downwindDir)) {
          // downwind remap
          val remappedBaseVals = LinearBaseValues(centerValue, downwindValue)

          (interpolate1D(x1_ext, basePosOrthoDirExt, remappedBaseVals),
            interpolate1D(x1_int, basePosOrthoDirExt, remappedBaseVals))
        } else {
          // no remap
          val baseValsForInterp = LinearBaseValues(downwindValue, centerValue)

          (interpolate1D(x1_ext, basePosOrthoDirInt, baseValsForInterp),
            interpolate1D(x1_int, basePosOrthoDirInt, baseValsForInterp))
        }
      }

      // fetch orthogonal downwind neighbor (in second dir) and their corresponding extrap values ...
      val orthoDirDownwind2D = orthogonalNeighDirs(0)

      val f0_down_2D = IR_VariableAccess("f0_down_2D", IR_RealDatatype)

      interpStmts += IR_VariableDeclaration(f0_down_2D, interps(orthoDirDownwind2D))

      // ... and perform another linear interp/extrap in second direction
      val (f1_down_val, f1_up_val) = interpolate2D(defIt, f0, orthoDirDownwind2D, f0_down_2D)

      Knowledge.dimensionality match {
        case 2 =>
          // write 2D results into variables
          interpStmts += IR_Assignment(results(0), f1_down_val)
          interpStmts += IR_Assignment(results(1), f1_up_val)
        case 3 =>
          // declare 2D interpolated values of first downwind orthogonal neighbor
          val f1_down = IR_VariableAccess("f1_down", IR_RealDatatype)
          val f1_up = IR_VariableAccess("f1_up", IR_RealDatatype)

          interpStmts += IR_VariableDeclaration(f1_down, f1_down_val)
          interpStmts += IR_VariableDeclaration(f1_up, f1_up_val)

          // fetch orthogonal downwind neighbors (in third dir) and their corresponding extrap values
          val orthoDirDownwind3D = orthogonalNeighDirs(1)

          val f0_down_3D = IR_VariableAccess("f0_down_3D", IR_RealDatatype)

          interpStmts += IR_VariableDeclaration(f0_down_3D, interps(orthoDirDownwind3D))

          // get directions to diagonal neighbor cells
          val diagOrigins : Array[Array[Int]] = {
            def adaptForRemap(d : Array[Int]) = if (stencilAdapted(cond, d)) d.map(_ * -1) else d

            Array(
              dirSum(adaptForRemap(orthoDirDownwind3D), adaptForRemap(orthoDirDownwind2D)),
            )
          }

          // construct further extrap bases on diagonal neighbor cells
          val f0_diag_3D : Array[IR_VariableAccess] = diagOrigins.zipWithIndex.map { case (dir, i) =>
            val origin = defIt + IR_ExpressionIndex(dir)
            val basePosInvCommDir = getBasePositionsForExtrapolation(invCommDir, origin)
            val baseValsInvCommDir = getBaseValues(field, slot, invCommDir, origin, RemappedBasesShifts)

            val diagExtrapBase = IR_VariableAccess(s"diagExtrapBase_$i", IR_RealDatatype)
            interpStmts += IR_VariableDeclaration(diagExtrapBase, interpolate1D(x0, basePosInvCommDir, baseValsInvCommDir))

            diagExtrapBase
          }


          val (f2_down_val, f2_up_val) = interpolate2D(defIt + IR_ExpressionIndex(orthoDirDownwind3D), f0_down_3D,
            orthoDirDownwind2D, f0_diag_3D(0))

          val f2_down = IR_VariableAccess("f2_down", IR_RealDatatype)
          val f2_up = IR_VariableAccess("f2_up", IR_RealDatatype)

          interpStmts += IR_VariableDeclaration(f2_down, f2_down_val)
          interpStmts += IR_VariableDeclaration(f2_up, f2_up_val)

          // perform another linear interp/extrap in third dimension
          var (f4_down_val, f4_up_val) = interpolate2D(defIt, f1_down, orthoDirDownwind3D, f2_down)
          var (f5_down_val, f5_up_val) = interpolate2D(defIt, f1_up, orthoDirDownwind3D, f2_up)

          // simplify results on 3d interp here
          f4_down_val = IR_SimplifyExpression.simplifyFloatingExpr(f4_down_val)
          f4_up_val = IR_SimplifyExpression.simplifyFloatingExpr(f4_up_val)
          f5_down_val = IR_SimplifyExpression.simplifyFloatingExpr(f5_down_val)
          f5_up_val = IR_SimplifyExpression.simplifyFloatingExpr(f5_up_val)

          // write 3D results into variables
          interpStmts += IR_Assignment(results(0), f4_down_val)
          interpStmts += IR_Assignment(results(1), f5_down_val)
          interpStmts += IR_Assignment(results(2), f4_up_val)
          interpStmts += IR_Assignment(results(3), f5_up_val)
      }

      IR_IfCondition(cond, interpStmts)
    }

    // add declarations for variables storing common subexpressions
    //commonExtrapBases.foreach { case (initVal, varAcc) => innerStmts += IR_VariableDeclaration(varAcc, initVal) }

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

case class IR_LinearInterpPackingC2FRemote(
    var send : Boolean,
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_RemotePackInfo,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[IR_Expression],
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

    def commBuffer = IR_IV_CommBuffer(field, send, indices.getTotalSize, neighborIdx, concurrencyId, indexOfRefinedNeighbor)

    def tmpBufAccess(index : IR_ExpressionIndex, offset : IR_ExpressionIndex, stride : IR_ExpressionIndex) =
      IR_TempBufferAccess(commBuffer, index + offset, stride)

    var innerStmts : ListBuffer[IR_Statement] = ListBuffer()

    if (send) {
      // store final interp results for fine ghost neighbor cells (2 in 2D, 4 in 3D)
      val interpResults = (0 until Knowledge.refinement_maxFineNeighborsForCommAxis).map(i =>
        IR_VariableAccess(s"finalInterpResult2D_$i", IR_RealDatatype)).toArray

      // declare variables for final interp results
      for (res <- interpResults)
        innerStmts += IR_VariableDeclaration(res)

      // perform computation and fill variables
      innerStmts ++= LinearInterpPackingC2FHelper.generateInterpStmts(interpResults, field, slot, packInfo)

      // write result variables to buffer
      for ((res, i) <- interpResults.zipWithIndex) {
        val off = IR_ExpressionIndex(Array.fill(numDims)(0).updated(0, i))
        val stride = IR_ExpressionIndex(indices.end, indices.begin, _ - _)

        // index mapping for temporary buffer
        val adaptedIdx = {
          IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims).indices.zipWithIndex.map { case (idx, i) =>
            if (getDimFromDir(neighbor.dir) != i)
              (idx - indices.begin(i)) * Knowledge.refinement_maxFineNeighborsForCommAxis : IR_Expression
            else
              idx - indices.begin(i) : IR_Expression
          })
        }

        innerStmts += IR_Assignment(tmpBufAccess(adaptedIdx, off, stride), res)
      }
    } else {
      // interp values from fine neighbor already aligned in correct order
      val idx = IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), indices.begin, _ - _)
      val off = IR_ExpressionIndex(Array.fill(numDims)(0))
      val stride = IR_ExpressionIndex(indices.end, indices.begin, _ - _)

      innerStmts += IR_Assignment(IR_DirectFieldLikeAccess(field, Duplicate(slot), defIt),
        tmpBufAccess(idx, off, stride))
    }

    val loop = new IR_LoopOverDimensions(numDims, indices, innerStmts, condition = condition)
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true
    loop.parallelization.noVect = !send // different indexing of field iterator and tmp buffer for recv
    ret += loop

    ret
  }
}

case class IR_LinearInterpPackingC2FLocal(
    var send : Boolean,
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_LocalPackInfo,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  import IR_InterpPackingHelper._

  def numDims : Int = field.layout.numDimsData

  override def expand() : OutputType = {
    if (condition.isDefined)
      Logger.error("Conditions for refined communication are not supported yet.")

    // TODO: pull scheme for local comm, only push implemented
    if (!send)
      Logger.warn("Pull comm scheme is not yet implemented for quadratic C2F interp.")

    val packIntervalDest = packInfo.getPackIntervalDest()
    val packIntervalSrc = packInfo.getPackIntervalSrc()

    val neighbor = packInfo.neighbor
    val domainIdx = field.domain.index
    val neighborIdx = neighbor.index

    val commDir = packInfo.neighDir

    var innerStmts : ListBuffer[IR_Statement] = ListBuffer()

    // store final interp results for fine ghost neighbor cells (2 in 2D, 4 in 3D)
    val interpResults = (0 until Knowledge.refinement_maxFineNeighborsForCommAxis).map(i =>
      IR_VariableAccess(s"finalInterpResult2D_$i", IR_RealDatatype)).toArray

    // declare variables for final interp results
    for (res <- interpResults)
      innerStmts += IR_VariableDeclaration(res)

    // perform computation and fill variables
    innerStmts ++= LinearInterpPackingC2FHelper.generateInterpStmts(interpResults, field, slot, packInfo)

    // push result to destination
    for ((res, resIdx) <- interpResults.zipWithIndex) {
      // store each result at a different field index (offset in upwind ortho dir)
      val offset = IR_ExpressionIndex(getCrossSumOfUpwindOrthogonals(commDir)(resIdx))

      // index mapping between the (local) fine/coarse iteration space
      val originSrc = IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims).indices.zipWithIndex.map { case (idx, i) =>
        if (getDimFromDir(neighbor.dir) != i)
          packIntervalSrc.begin(i) + (Knowledge.refinement_maxFineNeighborsPerDim * (idx - packIntervalDest.begin(i))) + offset(i) : IR_Expression
        else
          packIntervalSrc.begin(i) + idx - packIntervalDest.begin(i) + offset(i) : IR_Expression
      })

      innerStmts += IR_Assignment(
        IR_DirectFieldLikeAccess(field, Duplicate(slot),
          IR_IV_NeighborFragmentIdx(domainIdx, neighborIdx, indexOfRefinedNeighbor),
          originSrc),
        res)
    }

    val loop = new IR_LoopOverDimensions(numDims, packIntervalDest, innerStmts, condition = condition)
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true

    loop
  }
}

