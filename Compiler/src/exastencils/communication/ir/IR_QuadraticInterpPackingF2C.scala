package exastencils.communication.ir

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
import exastencils.optimization.ir.EvaluationException
import exastencils.optimization.ir.IR_SimplifyExpression

object QuadraticInterpPackingF2CHelper {

  import IR_InterpPackingBaseHelper._
  import IR_InterpPackingHelper._

  // shifts for accessing cell centers for interpolation with bases at [0, h, -h]
  private val CenteredBasesShifts = QuadraticBaseShifts(0, 1, -1)

  // shifts for accessing cell centers for extrapolation/interpolation with bases at [0, h, 2h]
  private val RemappedBasesShifts = QuadraticBaseShifts(0, 1, 2)

  // wrapper to check for equality of conditions
  def isSameCondition(condA : IR_Expression, condB : IR_Expression) : Boolean = (condA, condB) match {
    case (c1 : IR_AndAnd, c2 : IR_AndAnd) if c1.left == c2.right && c1.right == c2.left => true
    case (c1 : IR_Expression, c2 : IR_Expression) if c1 == c2                           => true
    case _                                                                              => false
  }

  def generateInterpStmts(result : IR_VariableAccess, field : IR_FieldLike, slot : IR_Expression, packInfo : IR_PackInfo) : ListBuffer[IR_Statement] = {

    def level : Int = field.level
    def localization = field.localization
    def numDims : Int = field.layout.numDimsData
    def defIt : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(numDims)


    def commDir : Array[Int] = packInfo.neighDir
    def invCommDir : Array[Int] = commDir.map(_ * -1)
    def commDirDim : Int = getDimFromDir(commDir)

    def isRegularCase() : IR_Expression = IR_BooleanConstant(true)

    def getSplitPackInterval() = packInfo match {
      case rp : IR_RemotePackInfo => rp.getPackInterval()
      case lp : IR_LocalPackInfo  => lp.getPackIntervalDest()
      case _                      => Logger.error("Unsupported PackInfo type used in (cell-based) quadratic F2C interp")
    }

    def getUnsplitPackInterval() = packInfo match {
      case rp : IR_RefinementPackInfoGhost => rp.unsplitPackIntervalSend(commDir)
      case _                               => Logger.error("Unsupported PackInfo type used in (cell-based) quadratic F2C interp")
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

    def getBasePositionsForInterp(it : IR_ExpressionIndex) =
      getBasePositions(level, localization, invCommDir, it, RemappedBasesShifts)

    def getBaseValuesForInterp(it : IR_ExpressionIndex) =
      getBaseValues(field, slot, invCommDir, it, RemappedBasesShifts)

    // generate statements
    var innerStmts : ListBuffer[IR_Statement] = ListBuffer()

    // debug...
    innerStmts += IR_Comment("Comm dir = " + dirToString(commDir))

    // Step 1 (1D): compute base on neighboring coarse ghost layer by interpolating fine cells in inverse comm direction
    val basePosInvCommDir = getBasePositionsForInterp(defIt)
    val baseValsInvCommDir = getBaseValuesForInterp(defIt)
    val x0 = IR_RealConstant(0.5) * getCellWidth(level, commDirDim, defIt) // target location at: 0.5h

    val f0 = IR_VariableAccess("f0_center_ext", IR_RealDatatype)
    innerStmts += IR_VariableDeclaration(f0, interpolate1D(x0, basePosInvCommDir, baseValsInvCommDir))

    // Step 2 (2D): also compute new (interpolated) neighbor bases in orthogonal axis directions (2 dirs in 2D, 4 in 4D)

    // store conditions, extrapolation results and statements for each case (3 in 2D, 9 in 3D)
    var extrapResults : HashMap[Array[Int], ListBuffer[(IR_Expression, IR_Expression)]] = HashMap()

    val orthogonalNeighDirs = getOrthogonalNeighborDirs(commDir)
    var stencilAdaptedForCase : HashMap[IR_Expression, ListBuffer[(Array[Int], Boolean)]] = HashMap()

    def markStencilAdapted(currCase : IR_Expression, orthoDir : Array[Int], adapted : Boolean) : Unit = {
      stencilAdaptedForCase = stencilAdaptedForCase.updated(currCase,
        stencilAdaptedForCase.getOrElse(currCase, ListBuffer()) :+ (orthoDir -> adapted))
    }

    for (orthoDir <- orthogonalNeighDirs) {
      val remappedOrthoDir = orthoDir.map(_ * -2)
      val remappedOrthoBaseVals = getBaseValues(field, slot, invCommDir, defIt + IR_ExpressionIndex(remappedOrthoDir), RemappedBasesShifts)
      val remappedOrthoBasePos = getBasePositions(level, localization, invCommDir, defIt + IR_ExpressionIndex(remappedOrthoDir), RemappedBasesShifts)

      // target location at: 0.5h
      val x0_ortho = IR_RealConstant(0.5) * getCellWidth(level, commDirDim, defIt + IR_ExpressionIndex(orthoDir))
      val x0_remapOrtho = IR_RealConstant(0.5) * getCellWidth(level, commDirDim, defIt + IR_ExpressionIndex(remappedOrthoDir))

      // cases where neighbor values for extrap has to be remapped
      var casesForOrthoDir : ListBuffer[(IR_Expression, IR_Expression)] = ListBuffer()
      Knowledge.dimensionality match {
        case 2 =>
          val isCorner = isAtBlockCornerForDir2D(orthoDir)

          casesForOrthoDir += (isCorner -> interpolate1D(x0_remapOrtho, remappedOrthoBasePos, remappedOrthoBaseVals))

          markStencilAdapted(isCorner, orthoDir, adapted = true)
        case 3 =>
          val isMinCorner = isAtBlockCornerForDir3D(commDir, orthoDir, min = true)
          val isMaxCorner = isAtBlockCornerForDir3D(commDir, orthoDir, min = false)

          casesForOrthoDir += (isMinCorner -> interpolate1D(x0_remapOrtho, remappedOrthoBasePos, remappedOrthoBaseVals))
          casesForOrthoDir += (isMaxCorner -> interpolate1D(x0_remapOrtho, remappedOrthoBasePos, remappedOrthoBaseVals))

          val remainingDirs = orthogonalNeighDirs.filter(dir => !(dir sameElements orthoDir) && !(dir sameElements orthoDir.map(_ * -1)))
          markStencilAdapted(isMinCorner, orthoDir, adapted = true)
          markStencilAdapted(isMinCorner, remainingDirs(0), adapted = true)

          markStencilAdapted(isMaxCorner, orthoDir, adapted = true)
          markStencilAdapted(isMaxCorner, remainingDirs(1), adapted = true)

          val isEdge = isAtBlockCornerForDir2D(orthoDir)

          casesForOrthoDir += (isEdge -> interpolate1D(x0_remapOrtho, remappedOrthoBasePos, remappedOrthoBaseVals))

          markStencilAdapted(isEdge, orthoDir, adapted = true)
      }

      // regular case without remap in second direction
      val orthoBasePosInvCommDir = getBasePositionsForInterp(defIt + IR_ExpressionIndex(orthoDir))
      val orthoBaseValsInvCommDir = getBaseValuesForInterp(defIt + IR_ExpressionIndex(orthoDir))

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

    // sort cases to remove ambiguity ...
    val caseOrdering : Ordering[IR_IfCondition] = Ordering.by {
      case _ @ IR_IfCondition(_ : IR_AndAnd, _, _)             => 0 // corners in 3D
      case _ @ IR_IfCondition(_ : IR_EqEq, _, _)               => 1 // corners in 2D, edges in 3D
      case _ @ IR_IfCondition(b, _, _) if b == isRegularCase() => 3 // least precedence as it should be the "default case"
      case _                                                   => 2
    }

    val fillStmtsPerCase = cases.map { case (cond, interps) =>
      val interpStmts : ListBuffer[IR_Statement] = ListBuffer()

      def interpolate2D(
          center : IR_ExpressionIndex,
          centerValue : IR_Expression,
          upwindDir : Array[Int], downwindDir : Array[Int],
          upwindValue : IR_Expression, downwindValue : IR_Expression) : IR_Expression = {

        // target location at 0.5h
        val dirDim = getDimFromDir(upwindDir)
        val x1 = getCellWidth(level, dirDim, center) / IR_RealConstant(2)

        // 3 Cases:
        // stencil adapted for upwind ortho dir -> bases at remapped ortho positions -> first value interp, second extrap
        // stencil adapted for downwind ortho dir -> bases at remapped ortho positions -> first value extrap, second interp
        // stencil NOT adapted -> bases at regular ortho positions -> both values interpolated
        if (stencilAdapted(cond, upwindDir)) {
          // upwind remap
          val basePosOrtho = getBasePositions(level, localization, upwindDir, center, RemappedBasesShifts)
          val remappedBaseValues = QuadraticBaseValues(centerValue, downwindValue, upwindValue)

          interpolate1D(x1, basePosOrtho, remappedBaseValues)
        } else if (stencilAdapted(cond, downwindDir)) {
          // downwind remap
          val basePosOrtho = getBasePositions(level, localization, upwindDir, center, RemappedBasesShifts)
          val remappedBaseValues = QuadraticBaseValues(centerValue, upwindValue, downwindValue)

          interpolate1D(x1, basePosOrtho, remappedBaseValues)
        } else {
          // no remap
          val basePosOrtho = getBasePositions(level, localization, upwindDir, center, CenteredBasesShifts)
          val baseValues = QuadraticBaseValues(centerValue, upwindValue, downwindValue)

          interpolate1D(x1, basePosOrtho, baseValues)
        }
      }

      // fetch orthogonal upwind/downwind neighbors (in second dir) and their corresponding extrap values ...
      val orthoDirDownwind2D = orthogonalNeighDirs(0)
      val orthoDirUpwind2D = orthogonalNeighDirs(1)

      val f0_down_2D = IR_VariableAccess("f0_down_2D", IR_RealDatatype)
      val f0_up_2D = IR_VariableAccess("f0_up_2D", IR_RealDatatype)

      interpStmts += IR_VariableDeclaration(f0_down_2D, interps(orthoDirDownwind2D))
      interpStmts += IR_VariableDeclaration(f0_up_2D, interps(orthoDirUpwind2D))

      val f1_val = interpolate2D(defIt, f0, orthoDirUpwind2D, orthoDirDownwind2D, f0_up_2D, f0_down_2D)

      Knowledge.dimensionality match {
        case 2 =>
          interpStmts += IR_Assignment(result, f1_val)
        case 3 =>
          // fetch orthogonal upwind/downwind neighbors (in third dir) and their corresponding interp values
          val orthoDirDownwind3D = orthogonalNeighDirs(2)
          val orthoDirUpwind3D = orthogonalNeighDirs(3)

          val f0_down_3D = IR_VariableAccess("f0_down_3D", IR_RealDatatype)
          val f0_up_3D = IR_VariableAccess("f0_up_3D", IR_RealDatatype)

          interpStmts += IR_VariableDeclaration(f0_down_3D, interps(orthoDirDownwind3D))
          interpStmts += IR_VariableDeclaration(f0_up_3D, interps(orthoDirUpwind3D))

          // get directions to diagonal neighbor cells
          val diagOrigins : Array[Array[Int]] = {
            def adaptForRemap(d : Array[Int]) = if (stencilAdapted(cond, d)) d.map(_ * -2) else d

            Array(
              dirSum(adaptForRemap(orthoDirDownwind3D), adaptForRemap(orthoDirUpwind2D)),
              dirSum(adaptForRemap(orthoDirDownwind3D), adaptForRemap(orthoDirDownwind2D)),
              dirSum(adaptForRemap(orthoDirUpwind3D), adaptForRemap(orthoDirUpwind2D)),
              dirSum(adaptForRemap(orthoDirUpwind3D), adaptForRemap(orthoDirDownwind2D)),
            )
          }

          // construct further extrap bases on diagonal neighbor cells
          val f0_diag_3D : Array[IR_VariableAccess] = diagOrigins.zipWithIndex.map { case (dir, i) =>
            val diagOrigin = defIt + IR_ExpressionIndex(dir)
            val basePosInvCommDir = getBasePositionsForInterp(diagOrigin)
            val baseValsInvCommDir = getBaseValuesForInterp(diagOrigin)

            val diagExtrapBase = IR_VariableAccess(s"diagExtrapBase_$i", IR_RealDatatype)
            interpStmts += IR_VariableDeclaration(diagExtrapBase, interpolate1D(x0, basePosInvCommDir, baseValsInvCommDir))

            diagExtrapBase
          }

          val f2_val = interpolate2D(defIt + IR_ExpressionIndex(orthoDirDownwind3D), f0_down_3D,
            orthoDirUpwind2D, orthoDirDownwind2D, f0_diag_3D(0), f0_diag_3D(1))

          val f3_val = interpolate2D(defIt + IR_ExpressionIndex(orthoDirUpwind3D), f0_up_3D,
            orthoDirUpwind2D, orthoDirDownwind2D, f0_diag_3D(2), f0_diag_3D(3))

          val f1 = IR_VariableAccess("f1", IR_RealDatatype)
          val f2 = IR_VariableAccess("f2", IR_RealDatatype)
          val f3 = IR_VariableAccess("f3", IR_RealDatatype)

          interpStmts += IR_VariableDeclaration(f1, f1_val)
          interpStmts += IR_VariableDeclaration(f2, f2_val)
          interpStmts += IR_VariableDeclaration(f3, f3_val)

          interpStmts += IR_Assignment(result, interpolate2D(defIt, f1, orthoDirUpwind3D, orthoDirDownwind3D, f3, f2))
      }

      IR_IfCondition(cond, interpStmts)
    }

    // ... and fold into a single nested if-else construct
    val sortedCases = fillStmtsPerCase.toSeq.sorted(caseOrdering)
    innerStmts += sortedCases.dropRight(1).reverse.foldLeft(sortedCases.reverse.head)((c1, c2) => IR_IfCondition(c2.condition, c2.trueBody, c1))
  }
}

case class IR_QuadraticInterpPackingF2CRemote(
    var send : Boolean,
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_RemotePackInfo,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  import IR_InterpPackingHelper._
  import QuadraticInterpPackingF2CHelper._

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
      val off = IR_ExpressionIndex(Array.fill(numDims)(0))
      val stride = IR_ExpressionIndex(indices.end, indices.begin, _ - _)

      // index mapping for temporary buffer
      val adaptedIdx = {
        IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims).indices.zipWithIndex.map { case (idx, i) =>
          if (getDimFromDir(neighbor.dir) != i)
            (idx - indices.begin(i)) / Knowledge.refinement_maxFineNeighborsPerDim : IR_Expression
          else
            idx - indices.begin(i) : IR_Expression
        })
      }

      // stride mapping for temporary buffer
      val adaptedStride = IR_ExpressionIndex(stride.indices.zipWithIndex.map { case (s, i) =>
        if (i != getDimFromDir(neighbor.dir))
          s / Knowledge.refinement_maxFineNeighborsPerDim
        else
          s
      })

      // store final interp result for coarse ghost neighbor cell
      val interpResult = IR_VariableAccess("finalInterpResult2D", IR_RealDatatype)
      innerStmts += IR_VariableDeclaration(interpResult)
      innerStmts ++= generateInterpStmts(interpResult, field, slot, packInfo)

      innerStmts += IR_Assignment(tmpBufAccess(adaptedIdx, off, adaptedStride), interpResult)
    } else {
      // interp/extrap values from coarse neighbor are written in order
      // of the upwind orthogonal dirs (in regards to comm dir) and their cross sums

      // read from buffer into field in order of cross sum of orthogonal upwind dirs
      for ((offset, i) <- getCrossSumOfUpwindOrthogonals(commDir).distinct.zipWithIndex) {
        val idx = IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), indices.begin, _ - _)
        val off = IR_ExpressionIndex(Array.fill(numDims)(0).updated(0, i))
        val stride = IR_ExpressionIndex(indices.end, indices.begin, _ - _)

        val adaptedIdx = IR_ExpressionIndex(idx.indices.zipWithIndex.map { case (idx, i) =>
          (idx - (idx / Knowledge.refinement_maxFineNeighborsPerDim)) * Knowledge.refinement_maxFineNeighborsForCommAxis : IR_Expression
        })

        // stride mapping for temporary buffer
        val adaptedStride = IR_ExpressionIndex(stride.indices.zipWithIndex.map { case (s, i) =>
          if (i != getDimFromDir(neighbor.dir))
            s / Knowledge.refinement_maxFineNeighborsPerDim
          else
            s
        })

        innerStmts += IR_Assignment(
          IR_DirectFieldLikeAccess(field, Duplicate(slot), defIt + IR_ExpressionIndex(offset)), tmpBufAccess(adaptedIdx, off, adaptedStride))
      }
    }

    // fine neighbor cells (2 in 2D, 4 in 3D) are quadratícally interpolated and the result is sent to the coarse neighbor
    val stride = IR_ExpressionIndex(Array.fill(Knowledge.dimensionality)(2).updated(getDimFromDir(commDir), 1))

    val loop = new IR_LoopOverDimensions(numDims, indices, innerStmts, stride, condition = condition)
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true
    loop.parallelization.noVect = true // different indexing of field iterator and tmp buffer for send/recv
    ret += loop

    ret
  }
}

case class IR_QuadraticInterpPackingF2CLocal(
    var send : Boolean,
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_LocalPackInfo,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  import IR_InterpPackingHelper._
  import QuadraticInterpPackingF2CHelper._

  def numDims : Int = field.layout.numDimsData
  def commDir : Array[Int] = packInfo.neighDir

  override def expand() : OutputType = {
    if (condition.isDefined)
      Logger.error("Conditions for refined communication are not supported yet.")

    // TODO: pull scheme for local comm
    if (send)
      Logger.warn("Push comm scheme is not yet implemented for quadratic F2C interp.")

    val packIntervalDest = packInfo.getPackIntervalDest()
    val packIntervalSrc = packInfo.getPackIntervalSrc()

    val neighbor = packInfo.neighbor
    val domainIdx = field.domain.index
    val neighborIdx = neighbor.index

    // index mapping between the (local) fine/coarse iteration space
    val originSrc = IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims).indices.zipWithIndex.map { case (idx, i) =>
      if (getDimFromDir(neighbor.dir) != i)
        packIntervalSrc.begin(i) - packIntervalDest.begin(i) + (idx - (idx / Knowledge.refinement_maxFineNeighborsPerDim)) : IR_Expression
      else
        packIntervalSrc.begin(i) - packIntervalDest.begin(i) + idx
    })

    var innerStmts : ListBuffer[IR_Statement] = ListBuffer()

    // store final interp result for coarse ghost neighbor cell
    val interpResult = IR_VariableAccess("finalInterpResult2D", IR_RealDatatype)
    innerStmts += IR_VariableDeclaration(interpResult)
    innerStmts ++= generateInterpStmts(interpResult, field, slot, packInfo)

    // push result to destination
    innerStmts += IR_Assignment(
      IR_DirectFieldLikeAccess(field, Duplicate(slot),
        IR_IV_NeighborFragmentIdx(domainIdx, neighborIdx, indexOfRefinedNeighbor),
        originSrc),
      interpResult)

    // fine neighbor cells (2 in 2D, 4 in 3D) are linearly interpolated and the result is sent to the coarse neighbor
    val stride = IR_ExpressionIndex(Array.fill(Knowledge.dimensionality)(2).updated(getDimFromDir(commDir), 1))

    val loop = new IR_LoopOverDimensions(numDims, packIntervalDest, innerStmts, stride, condition = condition)
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true
    loop
  }
}


