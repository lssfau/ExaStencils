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

      //(for (baseIdx <- asArray.indices) yield factors(baseIdx).reduce(_ * _ : IR_Expression)).toArray

      // TODO: weights wrong when simplified here
      val ret = asArray.indices.map(i => IR_Multiplication(factors(i) : _*))
      for (e <- ret)
        IR_GeneralSimplify.doUntilDoneStandalone(IR_ExpressionStatement(e))

      ret.toArray
    }
  }

  sealed case class BaseValues(var f0 : IR_Expression, var f1 : IR_Expression, var f2 : IR_Expression) {
    private val asArray : Array[IR_Expression] = Array(Duplicate(f0), Duplicate(f1), Duplicate(f2))

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

  def extrapVariableNeighbor(dir : Array[Int]) =
    IR_VariableAccess(s"f_neighbor_${ dirToString(dir) }_ext", IR_RealDatatype)

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

      val extrapVariableCenter = IR_VariableAccess("f0_center_ext", IR_RealDatatype)
      innerStmts += IR_VariableDeclaration(extrapVariableCenter, interpolate(x0, basePosInvCommDir, baseValsInvCommDir))

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

            casesForOrthoDir += (isCorner -> interpolate(x0_remapOrtho, remappedOrthoBasePosCommDir, remappedOrthoBaseVals))

            markStencilAdapted(isCorner, adapted = true)
          case 3 =>
            val isMinCorner = isAtBlockCornerForDir3D(commDir, orthoDir, min = true)
            val isMaxCorner = isAtBlockCornerForDir3D(commDir, orthoDir, min = false)

            casesForOrthoDir += (isMinCorner -> interpolate(x0_remapOrtho, remappedOrthoBasePosCommDir, remappedOrthoBaseVals))
            casesForOrthoDir += (isMaxCorner -> interpolate(x0_remapOrtho, remappedOrthoBasePosCommDir, remappedOrthoBaseVals))

            markStencilAdapted(isMinCorner, adapted = true)
            markStencilAdapted(isMaxCorner, adapted = true)

            val isEdge = isAtBlockCornerForDir2D(orthoDir)

            casesForOrthoDir += (isEdge -> interpolate(x0_remapOrtho, remappedOrthoBasePosCommDir, remappedOrthoBaseVals))

            markStencilAdapted(isEdge, adapted = true)
        }

        // regular case without remap in second direction
        val orthoBasePosInvCommDir = getBasePositionsForExtrapolation(invCommDir, defIt + IR_ExpressionIndex(orthoDir))
        val orthoBaseValsInvCommDir = getBaseValues(invCommDir, defIt + IR_ExpressionIndex(orthoDir), RemappedBasesShifts)

        casesForOrthoDir += (isRegularCase() -> interpolate(x0_ortho, orthoBasePosInvCommDir, orthoBaseValsInvCommDir))

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

      def pushToBuffer(stmts : IR_Statement*) : ListBuffer[IR_Statement] = {
        stmts.to[ListBuffer] :+ (IR_PreIncrement(it) : IR_Statement)
      }

      // Step 3 (2D): extrap bases already built -> extrap-/interpolation in second dir
      val fillStmtsPerCase = cases.map { case (cond, interps) =>
        val interpStmts : ListBuffer[IR_Statement] = ListBuffer()

        for ((orthoDir, interp) <- interps if isUpwindDir(orthoDir)) {
          // stencil adapted for upwind ortho dir -> first value interp, second extrap
          // stencil adapted for downwind ortho dir -> first value extrap, second interp

          // target location at +0.25h for interp, -0.25h for extrap
          val orthoDirDim = getDimFromDir(orthoDir)
          val x1_int = getCellWidth(orthoDirDim, defIt) / IR_RealConstant(4)
          val x1_ext = -1 * getCellWidth(orthoDirDim, defIt) / IR_RealConstant(4)

          val basePosOrthoDirExt = getBasePositionsForExtrapolation(orthoDir, defIt)
          val basePosOrthoDirInt = getBasePositionsForInterpolation(orthoDir, defIt)

          val invOrthoDir = orthoDir.map(_ * -1)

          val orthoDirVal = extrapVariableNeighbor(orthoDir)
          val invOrthoDirVal = extrapVariableNeighbor(invOrthoDir)

          // declare extrap results in ortho neighbors
          interpStmts += IR_VariableDeclaration(orthoDirVal, interp)
          interpStmts += IR_VariableDeclaration(invOrthoDirVal, interps.find(_._1 sameElements invOrthoDir).get._2)

          def stencilAdapted(dir : Array[Int]) =
            stencilAdaptedForCase(cond).exists(e => (e._1 sameElements dir) && e._2)

          // depending on case: use orthogonal extrap values for interp/extrap in second dir
          val (f1_center, f2_center) = {
            if (stencilAdapted(orthoDir)) {
              // upwind remap -> bases at remapped ortho positions. interp -> extrap
              val remappedBaseVals = BaseValues(extrapVariableCenter, invOrthoDirVal, orthoDirVal)

              ( interpolate(x1_int, basePosOrthoDirExt, remappedBaseVals),
                interpolate(x1_ext, basePosOrthoDirExt, remappedBaseVals) )
            } else if (stencilAdapted(invOrthoDir)) {
              // downwind remap -> bases at remapped ortho positions. extrap -> interp
              val remappedBaseVals = BaseValues(extrapVariableCenter, orthoDirVal, invOrthoDirVal)

              ( interpolate(x1_ext, basePosOrthoDirExt, remappedBaseVals),
                interpolate(x1_int, basePosOrthoDirExt, remappedBaseVals) )
            } else {
              // no remap -> bases at regular ortho positions -> only interp
              val baseValsForInterp = BaseValues(invOrthoDirVal, extrapVariableCenter, orthoDirVal)

              ( interpolate(x1_ext, basePosOrthoDirInt, baseValsForInterp),
                interpolate(x1_int, basePosOrthoDirInt, baseValsForInterp) )
            }
          }

          Knowledge.dimensionality match {
            case 2 =>
              interpStmts ++= pushToBuffer(IR_Assignment(tmpBufAccess, f1_center))
              interpStmts ++= pushToBuffer(IR_Assignment(tmpBufAccess, f2_center))
            case 3 =>

          }
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

      /*
      // debug output
      for ((cond, interps) <- cases) {
        innerStmts += IR_IfCondition(cond,
          ListBuffer[IR_Statement]() ++ (interps flatMap {
            case (orthoDir, interp) =>
              List(
                IR_Comment("Neighbor dir = " + dirToString(commDir) + ". Ortho dir = " + dirToString(orthoDir) + ". Cond = " + cond.prettyprint),
                IR_VariableDeclaration(extrapVariableNeighbor(orthoDir), interp))
          }))
      }
      */

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

