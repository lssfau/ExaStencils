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
import exastencils.mpi._
import exastencils.omp._
import exastencils.optimization._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.strategies._
import exastencils.util._

case class RegionSpecification(var region : String, var dir : Array[Int], var onlyOnBoundary : Boolean) {}

case class ContractingLoop(var number : Int, var iterator : Option[Expression], var statements : ListBuffer[Statement]) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ContractingLoop\n"

  // TODO: some error here? see expandSpecial
  // private def extendBounds(start : MultiIndex, end : MultiIndex, extent : Int, field : Field) : Unit = {
  //   for (dim <- 0 until Knowledge.dimensionality) {
  //     start(dim) -= IntegerConstant(extent) * (1 - ArrayAccess(iv.IterationOffsetBegin(field.domain.index), dim))
  //     end(dim) += IntegerConstant(extent) * (1 + ArrayAccess(iv.IterationOffsetEnd(field.domain.index), dim))
  //   }
  // }

  // IMPORTANT: must match and extend all possible bounds for LoopOverDimensions inside a ContractingLoop
  private def extendBounds(expr : Expression, extent : Int) : Expression = {
    expr match {
      case oInd @ OffsetIndex(0, 1, _, ArrayAccess(_ : iv.IterationOffsetBegin, _, _)) =>
        oInd.maxOffset += extent
        oInd.index = SimplifyExpression.simplifyIntegralExpr(oInd.index - extent)
        oInd.offset = SimplifyExpression.simplifyIntegralExpr(oInd.offset * (extent + 1))
        oInd

      case oInd @ OffsetIndex(-1, 0, _, ArrayAccess(_ : iv.IterationOffsetEnd, _, _)) =>
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
    object AdaptFieldSlots extends QuietDefaultStrategy("Adapt field slots") {
      this += new Transformation("now", {
        case fs @ FieldSelection(field, level, SlotAccess(slot, offset), _, _) =>
          fs.slot = new SlotAccess(slot, offset + fieldOffset.getOrElse(FieldKey(field), 0))
          fs
      })
    }
    AdaptFieldSlots.applyStandalone(new Scope(stmts))
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

          // TODO: fix! results differ from them generated when a LoopOverDimensions instead of a LoopOverPointsInOneFragment is present
          // case loop : LoopOverPointsInOneFragment =>
          //   val nju = Duplicate(loop)
          //   extendBounds(nju.startOffset, nju.endOffset, (number - i) * 1, loop.field) // TODO: currently independent from used stencils
          //   updateSlots(nju.body, fieldOffset)
          //   res += nju
        }

    for ((fKey, offset) <- fieldOffset) {
      val field = fields(fKey)
      res += AssignmentStatement(iv.CurrentSlot(field), (iv.CurrentSlot(field) + offset) Mod field.numSlots)
    }

    return res
  }
}

case class LoopOverPoints(var field : Field,
    var region : Option[RegionSpecification],
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
    val innerLoop = LoopOverPointsInOneFragment(field.domain.index, field, region, seq, startOffset, endOffset, increment, body, reduction, condition)

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
    var region : Option[RegionSpecification],
    var seq : Boolean, // FIXME: seq HACK
    var startOffset : MultiIndex,
    var endOffset : MultiIndex,
    var increment : MultiIndex,
    var body : ListBuffer[Statement],
    var reduction : Option[Reduction] = None,
    var condition : Option[Expression] = None) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverPointsInOneFragment\n"

  def expandSpecial : Output[Statement] = {
    var start = new MultiIndex()
    var stop = new MultiIndex()
    if (region.isDefined) {
      // case where a special region is to be traversed
      val regionCode = region.get.region.toUpperCase().charAt(0)

      start = new MultiIndex(DimArray().map(i => (i match {
        case i if region.get.dir(i) == 0 => field.fieldLayout.layoutsPerDim(i).idxById(regionCode + "LB") - field.referenceOffset(i)
        case i if region.get.dir(i) < 0  => field.fieldLayout.layoutsPerDim(i).idxById(regionCode + "LB") - field.referenceOffset(i)
        case i if region.get.dir(i) > 0  => field.fieldLayout.layoutsPerDim(i).idxById(regionCode + "RB") - field.referenceOffset(i)
      }) : Expression))

      stop = new MultiIndex(
        DimArray().map(i => (i match {
          case i if region.get.dir(i) == 0 => field.fieldLayout.layoutsPerDim(i).idxById(regionCode + "RE") - field.referenceOffset(i)
          case i if region.get.dir(i) < 0  => field.fieldLayout.layoutsPerDim(i).idxById(regionCode + "LE") - field.referenceOffset(i)
          case i if region.get.dir(i) > 0  => field.fieldLayout.layoutsPerDim(i).idxById(regionCode + "RE") - field.referenceOffset(i)
        }) : Expression))
    } else {
      // basic case -> just eliminate 'real' boundaries
      for (i <- 0 until Knowledge.dimensionality) {
        field.fieldLayout.discretization match {
          case d if "node" == d
            || ("face_x" == d && 0 == i)
            || ("face_y" == d && 1 == i)
            || ("face_z" == d && 2 == i) =>
            start(i) = OffsetIndex(0, 1, field.fieldLayout(i).idxDupLeftBegin - field.referenceOffset(i) + startOffset(i), ArrayAccess(iv.IterationOffsetBegin(field.domain.index), i))
            stop(i) = OffsetIndex(-1, 0, field.fieldLayout(i).idxDupRightEnd - field.referenceOffset(i) - endOffset(i), ArrayAccess(iv.IterationOffsetEnd(field.domain.index), i))
          case d if "cell" == d
            || ("face_x" == d && 0 != i)
            || ("face_y" == d && 1 != i)
            || ("face_z" == d && 2 != i) =>
            start(i) = field.fieldLayout(i).idxDupLeftBegin - field.referenceOffset(i) + startOffset(i)
            stop(i) = field.fieldLayout(i).idxDupRightEnd - field.referenceOffset(i) - endOffset(i)
        }
      }
    }

    var indexRange = IndexRange(start, stop)
    SimplifyStrategy.doUntilDoneStandalone(indexRange)

    var ret : Statement = (
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

    if (region.isDefined) {
      if (region.get.onlyOnBoundary) {
        val neighIndex = Fragment.getNeighIndex(region.get.dir)
        ret = new ConditionStatement(NegationExpression(iv.NeighborIsValid(domain, neighIndex)), ret)
      }
    }
    if (domain >= 0)
      ret = new ConditionStatement(iv.IsValidForSubdomain(domain), ret)

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

  def expandSpecial : Statement = {
    val parallelizable = Knowledge.omp_parallelizeLoopOverDimensions && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false })
    val parallelize = parallelizable && parallelizationIsReasonable
    val resolveOmpReduction = (
      parallelize
      && Knowledge.omp_enabled
      && Knowledge.omp_version < 3.1
      && reduction.isDefined
      && ("min" == reduction.get.op || "max" == reduction.get.op))

    // add internal condition (e.g. RB)
    var wrappedBody : ListBuffer[Statement] = (
      if (condition.isDefined)
        ListBuffer[Statement](new ConditionStatement(condition.get, body))
      else
        body)

    // compile loop(s)
    var ret : ForLoopStatement with OptimizationHint = null
    for (d <- 0 until numDimensions) {
      def it = VariableAccess(dimToString(d), Some(IntegerDatatype))
      val decl = VariableDeclarationStatement(IntegerDatatype, dimToString(d), Some(indices.begin(d)))
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

    // resolve omp reduction if necessary
    if (!resolveOmpReduction) {
      ret
    } else {
      // resolve max reductions
      val redOp = reduction.get.op
      val redExpName = reduction.get.target.name
      def redExp = VariableAccess(redExpName, None)
      val redExpLocalName = redExpName + "_red"
      def redExpLocal = VariableAccess(redExpLocalName, None)

      // FIXME: this assumes real data types -> data type should be determined according to redExp
      val decl = VariableDeclarationStatement(ArrayDatatype(RealDatatype, Knowledge.omp_numThreads), redExpLocalName, None)
      var init = (0 until Knowledge.omp_numThreads).map(fragIdx => AssignmentStatement(ArrayAccess(redExpLocal, fragIdx), redExp))
      val redOperands = ListBuffer[Expression](redExp) ++ (0 until Knowledge.omp_numThreads).map(fragIdx => ArrayAccess(redExpLocal, fragIdx) : Expression)
      val red = AssignmentStatement(redExp, if ("min" == redOp) MinimumExpression(redOperands) else MaximumExpression(redOperands))

      ReplaceStringConstantsStrategy.toReplace = redExp.prettyprint
      ReplaceStringConstantsStrategy.replacement = ArrayAccess(redExpLocal, VariableAccess("omp_tid", Some(IntegerDatatype)))
      ReplaceStringConstantsStrategy.applyStandalone(Scope(body)) // FIXME: remove Scope
      body.prepend(VariableDeclarationStatement(IntegerDatatype, "omp_tid", Some("omp_get_thread_num()")))

      Scope(ListBuffer[Statement](decl)
        ++ init
        ++ ListBuffer[Statement](ret, red))
    }
  }
}

object LoopOverFragments { def defIt = "fragmentIdx" }

case class LoopOverFragments(var body : ListBuffer[Statement], var reduction : Option[Reduction] = None) extends Statement with Expandable {
  import LoopOverFragments._

  def this(body : Statement, reduction : Option[Reduction]) = this(ListBuffer(body), reduction)
  def this(body : Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverFragments\n"

  def generateBasicLoop(parallelize : Boolean) = {
    if (parallelize)
      new ForLoopStatement(
        VariableDeclarationStatement(IntegerDatatype, defIt, Some(0)),
        LowerExpression(defIt, Knowledge.domain_numFragmentsPerBlock),
        PreIncrementExpression(defIt),
        body,
        reduction) with OMP_PotentiallyParallel
    else
      new ForLoopStatement(
        VariableDeclarationStatement(IntegerDatatype, defIt, Some(0)),
        LowerExpression(defIt, Knowledge.domain_numFragmentsPerBlock),
        PreIncrementExpression(defIt),
        body,
        reduction)
  }

  def expand : Output[StatementList] = {
    var statements = new ListBuffer[Statement]

    val parallelize = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false })
    val resolveOmpReduction = (
      parallelize
      && Knowledge.omp_version < 3.1
      && reduction.isDefined
      && ("min" == reduction.get.op || "max" == reduction.get.op))

    // basic loop

    if (!resolveOmpReduction) {
      statements += generateBasicLoop(parallelize)
    } else {
      // resolve max reductions
      val redOp = reduction.get.op
      val redExpName = reduction.get.target.name
      def redExp = VariableAccess(redExpName, None)
      val redExpLocalName = redExpName + "_red"
      def redExpLocal = VariableAccess(redExpLocalName, None)

      // FIXME: this assumes real data types -> data type should be determined according to redExp
      val decl = VariableDeclarationStatement(ArrayDatatype(RealDatatype, Knowledge.omp_numThreads), redExpLocalName, None)
      var init = (0 until Knowledge.omp_numThreads).map(fragIdx => AssignmentStatement(ArrayAccess(redExpLocal, fragIdx), redExp))
      val redOperands = ListBuffer[Expression](redExp) ++ (0 until Knowledge.omp_numThreads).map(fragIdx => ArrayAccess(redExpLocal, fragIdx) : Expression)
      val red = AssignmentStatement(redExp, if ("min" == redOp) MinimumExpression(redOperands) else MaximumExpression(redOperands))

      ReplaceStringConstantsStrategy.toReplace = redExp.prettyprint
      ReplaceStringConstantsStrategy.replacement = ArrayAccess(redExpLocal, VariableAccess("omp_tid", Some(IntegerDatatype)))
      ReplaceStringConstantsStrategy.applyStandalone(Scope(body)) // FIXME: remove Scope
      body.prepend(VariableDeclarationStatement(IntegerDatatype, "omp_tid", Some("omp_get_thread_num()")))

      statements += Scope(ListBuffer[Statement](decl)
        ++ init
        ++ ListBuffer[Statement](generateBasicLoop(parallelize), red))
    }

    if (Knowledge.mpi_enabled && reduction.isDefined) {
      statements += new MPI_Allreduce("&" ~ reduction.get.target, RealDatatype, 1, reduction.get.op) // FIXME: get dt and cnt from reduction
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
      VariableDeclarationStatement(IntegerDatatype, defIt, Some(0)),
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
      VariableDeclarationStatement(IntegerDatatype, defIt, Some(0)),
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
      VariableDeclarationStatement(IntegerDatatype, defIt, Some(Knowledge.minLevel)),
      LowerExpression(defIt, Knowledge.maxLevel + 1),
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
      VariableDeclarationStatement(IntegerDatatype, defIt, Some(0)),
      LowerExpression(defIt, Fragment.neighbors.size),
      PreIncrementExpression(defIt),
      body)
  }
}

