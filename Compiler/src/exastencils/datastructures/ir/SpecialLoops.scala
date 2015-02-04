package exastencils.datastructures.ir

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors.StackCollector
import exastencils.data._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.mpi._
import exastencils.omp._
import exastencils.optimization._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.strategies._
import exastencils.util._

case class ContractingLoop(var number : Int, var iterator : Option[Expression], var statements : ListBuffer[Statement]) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ContractingLoop\n"

  private def extendBounds(start : MultiIndex, end : MultiIndex, extent : Int, field : Field) : Unit = {
    for (dim <- 0 until Knowledge.dimensionality) {
      start(dim) -= IntegerConstant(extent) * (1 - ArrayAccess(iv.IterationOffsetBegin(field.domain.index), dim))
      end(dim) += IntegerConstant(extent) * (1 + ArrayAccess(iv.IterationOffsetEnd(field.domain.index), dim))
    }
  }

  // IMPORTANT: must match and extend all possible bounds for LoopOverDimensions inside a ContractingLoop
  private def extendBounds(expr : Expression, extent : Int) : Expression = {
    expr match {
      case oInd @ OffsetIndex(0, 1, _, ArrayAccess(_ : iv.IterationOffsetBegin, _)) =>
        oInd.maxOffset += extent
        oInd.index = SimplifyExpression.simplifyIntegralExpr(oInd.index - extent)
        oInd.offset = SimplifyExpression.simplifyIntegralExpr(oInd.offset * (extent + 1))
        oInd

      case oInd @ OffsetIndex(-1, 0, _, ArrayAccess(_ : iv.IterationOffsetEnd, _)) =>
        oInd.minOffset -= extent
        oInd.index = SimplifyExpression.simplifyIntegralExpr(oInd.index + extent)
        oInd.offset = SimplifyExpression.simplifyIntegralExpr(oInd.offset * (extent + 1))
        oInd
    }
  }

  private type FieldKey = (String, Int)
  private def FieldKey(field : Field) : FieldKey = {
    return (field.identifier, field.level)
  }

  private def updateSlots(stmts : ListBuffer[Statement], fieldOffset : HashMap[FieldKey, Int]) : Unit = {
    object AdaptFieldSlots extends DefaultStrategy("Adapt field slots") {
      this += new Transformation("now", {
        case fs @ FieldSelection(field, level, SlotAccess(slot, offset), _, _) =>
          fs.slot = new SlotAccess(slot, offset + fieldOffset.getOrElse(FieldKey(field), 0))
          fs
      })
    }
    val oldLvl = Logger.getLevel
    Logger.setLevel(1)
    AdaptFieldSlots.applyStandalone(new Scope(stmts))
    Logger.setLevel(oldLvl)
  }

  private def processLoopOverDimensions(l : LoopOverDimensions, extent : Int, fieldOffset : HashMap[FieldKey, Int]) : LoopOverDimensions = {
    val nju : LoopOverDimensions = Duplicate(l)
    for (dim <- 0 until Knowledge.dimensionality) {
      nju.indices.begin(dim) = extendBounds(nju.indices.begin(dim), extent)
      nju.indices.end(dim) = extendBounds(nju.indices.end(dim), extent)
    }
    updateSlots(nju.body, fieldOffset)
    return nju
  }

  def expandSpecial : Output[NodeList] = {
    val res = new ListBuffer[Statement]()
    val fieldOffset = new HashMap[FieldKey, Int]()
    val fields = new HashMap[FieldKey, Field]()
    var condStmt : ConditionStatement = null
    for (i <- 1 to number)
      for (stmt <- statements)
        stmt match {
          case AdvanceSlotStatement(iv.CurrentSlot(field, fragment)) =>
            val fKey = FieldKey(field)
            fieldOffset(fKey) = fieldOffset.getOrElse(fKey, 0) + 1
            fields(fKey) = field

          case cStmt @ ConditionStatement(cond, ListBuffer(l : LoopOverDimensions), ListBuffer()) =>
            val nju = processLoopOverDimensions(l, (number - i) * 1, fieldOffset) // TODO: currently independent from used stencils (const factor 1)
            if (condStmt == null || cond != condStmt.condition) {
              condStmt = Duplicate(cStmt)
              condStmt.trueBody.clear()
              res += condStmt
            }
            condStmt.trueBody += nju

          case l : LoopOverDimensions =>
            res += processLoopOverDimensions(l, (number - i) * 1, fieldOffset) // TODO: currently independent from used stencils (const factor 1)

          case loop : LoopOverPointsInOneFragment =>
            val nju = Duplicate(loop)
            extendBounds(nju.startOffset, nju.endOffset, (number - i) * 1, loop.field) // TODO: currently independent from used stencils
            updateSlots(nju.body, fieldOffset)
            res += nju
        }

    for ((fKey, offset) <- fieldOffset) {
      val field = fields(fKey)
      res += AssignmentStatement(iv.CurrentSlot(field), (iv.CurrentSlot(field) + offset) Mod field.numSlots)
    }

    return res
  }
}

case class LoopOverPoints(var field : Field,
    var seq : Boolean, // FIXME: seq HACK
    var startOffset : MultiIndex,
    var endOffset : MultiIndex,
    var increment : MultiIndex,
    var body : ListBuffer[Statement],
    var reduction : Option[Reduction] = None,
    var condition : Option[Expression] = None) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverPoints\n"

  def expandSpecial(collector : StackCollector) : Output[Statement] = {
    val insideFragLoop = collector.stack.map(node => node match { case loop : LoopOverFragments => true; case _ => false }).reduce((left, right) => left || right)
    val innerLoop = LoopOverPointsInOneFragment(field.domain.index, field, seq, startOffset, endOffset, increment, body, reduction, condition)

    if (insideFragLoop)
      innerLoop
    else {
      if (seq)
        new LoopOverFragments(innerLoop, reduction)
      else
        new LoopOverFragments(innerLoop, reduction) with OMP_PotentiallyParallel
    }
  }
}

case class LoopOverPointsInOneFragment(var domain : Int,
    var field : Field,
    var seq : Boolean, // FIXME: seq HACK
    var startOffset : MultiIndex,
    var endOffset : MultiIndex,
    var increment : MultiIndex,
    var body : ListBuffer[Statement],
    var reduction : Option[Reduction] = None,
    var condition : Option[Expression] = None) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverPointsInOneFragment\n"

  def expand : Output[Statement] = {
    var start = new MultiIndex()
    var stop = new MultiIndex()
    for (i <- 0 until Knowledge.dimensionality) {
      start(i) = OffsetIndex(0, 1, field.fieldLayout(i).idxDupLeftBegin - field.referenceOffset(i) + startOffset(i), ArrayAccess(iv.IterationOffsetBegin(field.domain.index), i))
      stop(i) = OffsetIndex(-1, 0, field.fieldLayout(i).idxDupRightEnd - field.referenceOffset(i) - endOffset(i), ArrayAccess(iv.IterationOffsetEnd(field.domain.index), i))
    }

    var indexRange = IndexRange(start, stop)
    SimplifyStrategy.doUntilDoneStandalone(indexRange)

    val ret = (
      if (seq)
        new LoopOverDimensions(Knowledge.dimensionality, indexRange, body, increment, reduction, condition)
      else {
        val ret = new LoopOverDimensions(Knowledge.dimensionality, indexRange, body, increment, reduction, condition) with OMP_PotentiallyParallel with PolyhedronAccessable
        ret.optLevel = (
          if (Knowledge.maxLevel - field.level < Knowledge.poly_numFinestLevels)
            Knowledge.poly_optLevel_fine
          else
            Knowledge.poly_optLevel_coarse)
        ret
      })

    if (domain >= 0)
      new ConditionStatement(iv.IsValidForSubdomain(domain), ret)
    else
      ret
  }
}

object LoopOverDimensions {
  def defIt = {
    Knowledge.dimensionality match {
      case 1 => new MultiIndex(dimToString(0), dimToString(1))
      case 2 => new MultiIndex(dimToString(0), dimToString(1), dimToString(2))
      case 3 => new MultiIndex(dimToString(0), dimToString(1), dimToString(2), dimToString(3))
    }
  }
}

case class LoopOverDimensions(var numDimensions : Int,
    var indices : IndexRange,
    var body : ListBuffer[Statement],
    var stepSize : MultiIndex = new MultiIndex(Array.fill(Knowledge.dimensionality + 1)(1)),
    var reduction : Option[Reduction] = None,
    var condition : Option[Expression] = None) extends Statement {
  def this(numDimensions : Int, indices : IndexRange, body : Statement, stepSize : MultiIndex, reduction : Option[Reduction], condition : Option[Expression]) = this(numDimensions, indices, ListBuffer[Statement](body), stepSize, reduction, condition)
  def this(numDimensions : Int, indices : IndexRange, body : Statement, stepSize : MultiIndex, reduction : Option[Reduction]) = this(numDimensions, indices, ListBuffer[Statement](body), stepSize, reduction)
  def this(numDimensions : Int, indices : IndexRange, body : Statement, stepSize : MultiIndex) = this(numDimensions, indices, ListBuffer[Statement](body), stepSize)
  def this(numDimensions : Int, indices : IndexRange, body : Statement) = this(numDimensions, indices, ListBuffer[Statement](body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverDimensions\n"

  def maxIterationCount() : Array[Long] = {
    numDimensions match {
      case 2 => indices match {
        // basic version assuming Integer constants
        case IndexRange(MultiIndex(IntegerConstant(xStart), IntegerConstant(yStart), _, _),
          MultiIndex(IntegerConstant(xEnd), IntegerConstant(yEnd), _, _)) =>
          Array(xEnd - xStart, yEnd - yStart)

        // extended version assuming OffsetIndex nodes with Integer constants
        // INFO: this uses the maximum number of points as criteria as this defines the case displaying the upper performance bound
        case IndexRange(
          MultiIndex(
            OffsetIndex(xStartOffMin, _, IntegerConstant(xStart), _),
            OffsetIndex(yStartOffMin, _, IntegerConstant(yStart), _), _, _),
          MultiIndex(
            OffsetIndex(_, xEndOffMax, IntegerConstant(xEnd), _),
            OffsetIndex(_, yEndOffMax, IntegerConstant(yEnd), _), _, _)) =>
          Array((xEnd + xEndOffMax) - (xStart + xStartOffMin),
            (yEnd + yEndOffMax) - (yStart + yStartOffMin))

        // no match so far... try if all expressions can be evaluated to integers
        case IndexRange(start @ MultiIndex(xStart, yStart, _, _),
          end @ MultiIndex(xEnd, yEnd, _, _)) =>
          try {
            val (xStarti, xEndi) = (SimplifyExpression.evalIntegral(xStart), SimplifyExpression.evalIntegral(xEnd))
            val (yStarti, yEndi) = (SimplifyExpression.evalIntegral(yStart), SimplifyExpression.evalIntegral(yEnd))
            start(0) = IntegerConstant(xStarti)
            start(1) = IntegerConstant(yStarti)
            end(0) = IntegerConstant(xEndi)
            end(1) = IntegerConstant(yEndi)
            Array(xEndi - xStarti, yEndi - yStarti)
          } catch {
            case _ : EvaluationException => null
          }

        // could not match
        case _ => null
      }

      case 3 => indices match {
        // basic version assuming Integer constants
        case IndexRange(MultiIndex(IntegerConstant(xStart), IntegerConstant(yStart), IntegerConstant(zStart), _),
          MultiIndex(IntegerConstant(xEnd), IntegerConstant(yEnd), IntegerConstant(zEnd), _)) =>
          Array(xEnd - xStart, yEnd - yStart, zEnd - zStart)

        // extended version assuming OffsetIndex nodes with Integer constants
        // INFO: this uses the maximum number of points as criteria as this defines the case displaying the upper performance bound
        case IndexRange(
          MultiIndex(
            OffsetIndex(xStartOffMin, _, IntegerConstant(xStart), _),
            OffsetIndex(yStartOffMin, _, IntegerConstant(yStart), _),
            OffsetIndex(zStartOffMin, _, IntegerConstant(zStart), _), _),
          MultiIndex(
            OffsetIndex(_, xEndOffMax, IntegerConstant(xEnd), _),
            OffsetIndex(_, yEndOffMax, IntegerConstant(yEnd), _),
            OffsetIndex(_, zEndOffMax, IntegerConstant(zEnd), _), _)) =>
          Array((xEnd + xEndOffMax) - (xStart + xStartOffMin),
            (yEnd + yEndOffMax) - (yStart + yStartOffMin),
            (zEnd + zEndOffMax) - (zStart + zStartOffMin))

        // no match so far... try if all expressions can be evaluated to integers
        case IndexRange(start @ MultiIndex(xStart, yStart, zStart, _),
          end @ MultiIndex(xEnd, yEnd, zEnd, _)) =>
          try {
            val (xStarti, xEndi) = (SimplifyExpression.evalIntegral(xStart), SimplifyExpression.evalIntegral(xEnd))
            val (yStarti, yEndi) = (SimplifyExpression.evalIntegral(yStart), SimplifyExpression.evalIntegral(yEnd))
            val (zStarti, zEndi) = (SimplifyExpression.evalIntegral(zStart), SimplifyExpression.evalIntegral(zEnd))
            start(0) = IntegerConstant(xStarti)
            start(1) = IntegerConstant(yStarti)
            start(2) = IntegerConstant(zStarti)
            end(0) = IntegerConstant(xEndi)
            end(1) = IntegerConstant(yEndi)
            end(2) = IntegerConstant(zEndi)
            Array(xEndi - xStarti, yEndi - yStarti, zEndi - zStarti)
          } catch {
            case _ : EvaluationException => null
          }

        // could not match
        case _ => null
      }

      case 4 => indices match {
        // basic version assuming Integer constants
        case IndexRange(MultiIndex(IntegerConstant(xStart), IntegerConstant(yStart), IntegerConstant(zStart), IntegerConstant(wStart)),
          MultiIndex(IntegerConstant(xEnd), IntegerConstant(yEnd), IntegerConstant(zEnd), IntegerConstant(wEnd))) =>
          Array(xEnd - xStart, yEnd - yStart, zEnd - zStart, wEnd - wStart)

        // extended version assuming OffsetIndex nodes with Integer constants
        // INFO: this uses the maximum number of points as criteria as this defines the case displaying the upper performance bound
        case IndexRange(
          MultiIndex(
            OffsetIndex(xStartOffMin, _, IntegerConstant(xStart), _),
            OffsetIndex(yStartOffMin, _, IntegerConstant(yStart), _),
            OffsetIndex(zStartOffMin, _, IntegerConstant(zStart), _),
            OffsetIndex(wStartOffMin, _, IntegerConstant(wStart), _)),
          MultiIndex(
            OffsetIndex(_, xEndOffMax, IntegerConstant(xEnd), _),
            OffsetIndex(_, yEndOffMax, IntegerConstant(yEnd), _),
            OffsetIndex(_, zEndOffMax, IntegerConstant(zEnd), _),
            OffsetIndex(_, wEndOffMax, IntegerConstant(wEnd), _))) =>
          Array((xEnd + xEndOffMax) - (xStart + xStartOffMin),
            (yEnd + yEndOffMax) - (yStart + yStartOffMin),
            (zEnd + zEndOffMax) - (zStart + zStartOffMin),
            (wEnd + wEndOffMax) - (wStart + wStartOffMin))

        // no match so far... try if all expressions can be evaluated to integers
        case IndexRange(start @ MultiIndex(xStart, yStart, zStart, wStart),
          end @ MultiIndex(xEnd, yEnd, zEnd, wEnd)) =>
          try {
            val (xStarti, xEndi) = (SimplifyExpression.evalIntegral(xStart), SimplifyExpression.evalIntegral(xEnd))
            val (yStarti, yEndi) = (SimplifyExpression.evalIntegral(yStart), SimplifyExpression.evalIntegral(yEnd))
            val (zStarti, zEndi) = (SimplifyExpression.evalIntegral(zStart), SimplifyExpression.evalIntegral(zEnd))
            val (wStarti, wEndi) = (SimplifyExpression.evalIntegral(wStart), SimplifyExpression.evalIntegral(wEnd))
            start(0) = IntegerConstant(xStarti)
            start(1) = IntegerConstant(yStarti)
            start(2) = IntegerConstant(zStarti)
            start(3) = IntegerConstant(wStarti)
            end(0) = IntegerConstant(xEndi)
            end(1) = IntegerConstant(yEndi)
            end(2) = IntegerConstant(zEndi)
            end(3) = IntegerConstant(wEndi)
            Array(xEndi - xStarti, yEndi - yStarti, zEndi - zStarti, wEndi - wStarti)
          } catch {
            case _ : EvaluationException => null
          }

        // could not match
        case _ => null
      }
    }
  }

  def parallelizationIsReasonable : Boolean = {
    val maxItCount = maxIterationCount()
    if (maxItCount == null)
      return true // cannot determine iteration count, default is no change in parallelizability, i.e. true

    var totalNumPoints : Long = 1
    for (i <- maxItCount)
      totalNumPoints *= i
    return (totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
  }

  def expandSpecial : ForLoopStatement = {
    val parallelizable = Knowledge.omp_parallelizeLoopOverDimensions && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false })
    val parallelize = parallelizable && parallelizationIsReasonable

    var wrappedBody : ListBuffer[Statement] = (
      if (condition.isDefined)
        ListBuffer[Statement](new ConditionStatement(condition.get, body))
      else
        body)

    var ret : ForLoopStatement with OptimizationHint = null
    for (d <- 0 until numDimensions) {
      def it = VariableAccess(dimToString(d), Some(IntegerDatatype()))
      val decl = VariableDeclarationStatement(IntegerDatatype(), dimToString(d), Some(indices.begin(d)))
      val cond = LowerExpression(it, indices.end(d))
      val incr = AssignmentStatement(it, stepSize(d), "+=")
      if (parallelize && d == numDimensions - 1) {
        val omp = new ForLoopStatement(decl, cond, incr, wrappedBody, reduction) with OptimizationHint with OMP_PotentiallyParallel
        omp.collapse = numDimensions
        ret = omp
      } else {
        ret = new ForLoopStatement(decl, cond, incr, wrappedBody, reduction) with OptimizationHint
        wrappedBody = ListBuffer[Statement](ret)
      }
      // set optimization hints
      ret.isInnermost = d == 0
      ret.isParallel = parallelizable
    }

    return ret
  }
}

object LoopOverFragments { def defIt = "fragmentIdx" }

case class LoopOverFragments(var body : ListBuffer[Statement], var reduction : Option[Reduction] = None) extends Statement with Expandable {
  import LoopOverFragments._

  def this(body : Statement, reduction : Option[Reduction]) = this(ListBuffer(body), reduction)
  def this(body : Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverFragments\n"

  def expand : Output[StatementList] = {
    val parallelizable = Knowledge.omp_parallelizeLoopOverFragments && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false })
    var statements = new ListBuffer[Statement]

    if (parallelizable)
      statements += new ForLoopStatement(
        VariableDeclarationStatement(new IntegerDatatype, defIt, Some(0)),
        LowerExpression(defIt, Knowledge.domain_numFragsPerBlock),
        PreIncrementExpression(defIt),
        body,
        reduction) with OMP_PotentiallyParallel
    else
      statements += new ForLoopStatement(
        VariableDeclarationStatement(new IntegerDatatype, defIt, Some(0)),
        LowerExpression(defIt, Knowledge.domain_numFragsPerBlock),
        PreIncrementExpression(defIt),
        body,
        reduction)

    if (Knowledge.useMPI && reduction.isDefined) {
      statements += new MPI_Allreduce("&" ~ reduction.get.target, new RealDatatype, 1, reduction.get.op) // FIXME: get dt and cnt from reduction
    }

    statements
  }
}

object LoopOverDomains { def defIt = "domainIdx" }

case class LoopOverDomains(var body : ListBuffer[Statement]) extends Statement with Expandable {
  import LoopOverDomains._

  def this(body : Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverDomains\n"

  def expand : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(new IntegerDatatype, defIt, Some(0)),
      LowerExpression(defIt, DomainCollection.domains.size),
      PreIncrementExpression(defIt),
      body)
  }
}

object LoopOverFields { def defIt = "fieldIdx" }

case class LoopOverFields(var body : ListBuffer[Statement]) extends Statement with Expandable {
  import LoopOverFields._

  def this(body : Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverFields\n"

  def expand : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(new IntegerDatatype, defIt, Some(0)),
      LowerExpression(defIt, FieldCollection.fields.size),
      PreIncrementExpression(defIt),
      body)
  }
}

object LoopOverLevels { def defIt = "levelIdx" }

case class LoopOverLevels(var body : ListBuffer[Statement]) extends Statement with Expandable {
  import LoopOverLevels._

  def this(body : Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverLevels\n"

  def expand : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(new IntegerDatatype, defIt, Some(0)),
      LowerExpression(defIt, Knowledge.numLevels),
      PreIncrementExpression(defIt),
      body)
  }
}

object LoopOverNeighbors { def defIt = "neighborIdx" }

case class LoopOverNeighbors(var body : ListBuffer[Statement]) extends Statement with Expandable {
  import LoopOverNeighbors._

  def this(body : Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverNeighbors\n"

  def expand : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(new IntegerDatatype, defIt, Some(0)),
      LowerExpression(defIt, Fragment.neighbors.size),
      PreIncrementExpression(defIt),
      body)
  }
}

