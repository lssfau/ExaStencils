package exastencils.datastructures.ir

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.communication._
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

case class RegionSpecification(var region : String, var dir : Array[Int], var onlyOnBoundary : Boolean) {}
case class ContractionSpecification(var posExt : Array[Int], var negExt : Array[Int])

case class ContractingLoop(var number : Int, var iterator : Option[Expression], var statements : ListBuffer[Statement],
    var spec : ContractionSpecification) extends Statement {
  // TODO: validate spec
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ContractingLoop\n"

  // IMPORTANT: must match and extend all possible bounds for LoopOverDimensions inside a ContractingLoop
  private def extendBoundsBegin(expr : Expression, extent : Int) : Expression = {
    expr match {
      case IntegerConstant(i) =>
        IntegerConstant(i - extent)
      case oInd @ OffsetIndex(0, 1, _, ArrayAccess(_ : iv.IterationOffsetBegin, _, _)) =>
        oInd.maxOffset += extent
        oInd.index = SimplifyExpression.simplifyIntegralExpr(oInd.index - extent)
        oInd.offset = SimplifyExpression.simplifyIntegralExpr(oInd.offset * (extent + 1))
        oInd
    }
  }

  // IMPORTANT: must match and extend all possible bounds for LoopOverDimensions inside a ContractingLoop
  private def extendBoundsEnd(expr : Expression, extent : Int) : Expression = {
    expr match {
      case IntegerConstant(i) =>
        IntegerConstant(i + extent)
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
    AdaptFieldSlots.applyStandalone(stmts)
  }

  private def processLoopOverDimensions(l : LoopOverDimensions, extent : Int, fieldOffset : HashMap[FieldKey, Int]) : LoopOverDimensions = {
    val nju : LoopOverDimensions = Duplicate(l)
    for (dim <- 0 until Knowledge.dimensionality) {
      nju.indices.begin(dim) = extendBoundsBegin(nju.indices.begin(dim), extent * spec.negExt(dim))
      nju.indices.end(dim) = extendBoundsEnd(nju.indices.end(dim), extent * spec.posExt(dim))
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
            val nju = processLoopOverDimensions(l, (number - i), fieldOffset)
            if (condStmt == null || cond != condStmt.condition) {
              condStmt = Duplicate(cStmt)
              condStmt.trueBody.clear()
              res += condStmt
            }
            condStmt.trueBody += nju

          case l : LoopOverDimensions =>
            res += processLoopOverDimensions(l, (number - i), fieldOffset)
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
    var preComms : ListBuffer[CommunicateStatement] = ListBuffer(),
    var postComms : ListBuffer[CommunicateStatement] = ListBuffer(),
    var reduction : Option[Reduction] = None,
    var condition : Option[Expression] = None) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverPoints\n"

  def expandSpecial(collector : StackCollector) : Output[StatementList] = {
    val insideFragLoop = collector.stack.map(node => node match { case loop : LoopOverFragments => true; case _ => false }).reduce((left, right) => left || right)
    val innerLoop = LoopOverPointsInOneFragment(field.domain.index, field, region, seq, startOffset, endOffset, increment, body, reduction, condition)

    val actLoop =
      if (insideFragLoop)
        innerLoop
      else {
        if (seq)
          new LoopOverFragments(innerLoop, reduction)
        else
          new LoopOverFragments(innerLoop, reduction) with OMP_PotentiallyParallel
      }

    preComms ++ ListBuffer(actLoop) ++ postComms
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

      start = new MultiIndex(DimArray().map(dim => (dim match {
        case dim if region.get.dir(dim) == 0 => field.fieldLayout.idxById(regionCode + "LB", dim) - field.referenceOffset(dim) + startOffset(dim)
        case dim if region.get.dir(dim) < 0  => field.fieldLayout.idxById(regionCode + "LB", dim) - field.referenceOffset(dim) + startOffset(dim)
        case dim if region.get.dir(dim) > 0  => field.fieldLayout.idxById(regionCode + "RB", dim) - field.referenceOffset(dim) + startOffset(dim)
      }) : Expression))

      stop = new MultiIndex(
        DimArray().map(dim => (dim match {
          case dim if region.get.dir(dim) == 0 => field.fieldLayout.idxById(regionCode + "RE", dim) - field.referenceOffset(dim) - endOffset(dim)
          case dim if region.get.dir(dim) < 0  => field.fieldLayout.idxById(regionCode + "LE", dim) - field.referenceOffset(dim) - endOffset(dim)
          case dim if region.get.dir(dim) > 0  => field.fieldLayout.idxById(regionCode + "RE", dim) - field.referenceOffset(dim) - endOffset(dim)
        }) : Expression))
    } else {
      // basic case -> just eliminate 'real' boundaries
      for (dim <- 0 until Knowledge.dimensionality) {
        field.fieldLayout.discretization match {
          case discr if "node" == discr
            || ("face_x" == discr && 0 == dim)
            || ("face_y" == discr && 1 == dim)
            || ("face_z" == discr && 2 == dim) =>
            if (Knowledge.experimental_disableIterationOffsets) {
              start(dim) = field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim)
              stop(dim) = field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim)
            } else {
              // FIXME
              //              if (Knowledge.experimental_genVariableFieldSizes) {
              //                start(dim) = OffsetIndex(0, 1, ArrayAccess(iv.IndexFromField(field.identifier, field.level, "DLB"), dim) - field.referenceOffset(dim) + startOffset(dim), ArrayAccess(iv.IterationOffsetBegin(field.domain.index), dim))
              //                stop(dim) = OffsetIndex(-1, 0, ArrayAccess(iv.IndexFromField(field.identifier, field.level, "DRE"), dim) - field.referenceOffset(dim) - endOffset(dim), ArrayAccess(iv.IterationOffsetEnd(field.domain.index), dim))
              //              } else {
              val numDupLeft = field.fieldLayout.layoutsPerDim(dim).numDupLayersLeft
              val numDupRight = field.fieldLayout.layoutsPerDim(dim).numDupLayersRight
              if (numDupLeft > 0)
                start(dim) = OffsetIndex(0, numDupLeft, field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim), numDupLeft * ArrayAccess(iv.IterationOffsetBegin(field.domain.index), dim))
              else
                start(dim) = field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim)
              if (numDupRight > 0)
                stop(dim) = OffsetIndex(-numDupRight, 0, field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim), numDupRight * ArrayAccess(iv.IterationOffsetEnd(field.domain.index), dim))
              else
                stop(dim) = field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim)
              //              }
            }
          case discr if "cell" == discr
            || ("face_x" == discr && 0 != dim)
            || ("face_y" == discr && 1 != dim)
            || ("face_z" == discr && 2 != dim) =>
            start(dim) = field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim)
            stop(dim) = field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim)
        }
      }
    }

    var indexRange = IndexRange(start, stop)
    SimplifyStrategy.doUntilDoneStandalone(indexRange)

    // fix iteration space for reduction operations if required
    if (Knowledge.experimental_trimBoundsForReductionLoops && reduction.isDefined && !region.isDefined) {
      if (!condition.isDefined) condition = Some(BooleanConstant(true))
      for (dim <- 0 until Knowledge.dimensionality)
        if (field.fieldLayout.layoutsPerDim(dim).numDupLayersLeft > 0)
          /*if ("node" == field.fieldLayout.discretization
          || ("face_x" == field.fieldLayout.discretization && 0 == dim)
          || ("face_y" == field.fieldLayout.discretization && 1 == dim)
          || ("face_z" == field.fieldLayout.discretization && 2 == dim))*/
          condition = Some(AndAndExpression(condition.get, GreaterEqualExpression(VariableAccess(dimToString(dim), Some(IntegerDatatype)), field.fieldLayout.layoutsPerDim(dim).numDupLayersLeft)))
    }

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
        val neighIndex = Fragment.getNeigh(region.get.dir).index
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
    var start = Array.fill[Long](numDimensions)(0)
    var end = Array.fill[Long](numDimensions)(0)

    indices match {
      case indexRange : IndexRange =>
        indexRange.begin match {
          case startIndex : MultiIndex => {
            for (dim <- 0 until numDimensions) {
              startIndex(dim) match {
                case IntegerConstant(xStart)                                  => start(dim) = xStart
                case OffsetIndex(xStartOffMin, _, IntegerConstant(xStart), _) => start(dim) = xStart + xStartOffMin
                case _ => { // no use -> try evaluation as last resort
                  try {
                    val simplified = SimplifyExpression.evalIntegral(startIndex(dim))
                    startIndex(dim) = IntegerConstant(simplified)
                    start(dim) = simplified
                  } catch {
                    case _ : EvaluationException => return null // evaluation failed -> abort
                  }
                }
              }
            }
          }
          case _ => Logger.warn("Loop index range begin is not a MultiIndex"); return null
        }
        indexRange.end match {
          case endIndex : MultiIndex => {
            for (dim <- 0 until numDimensions) {
              endIndex(dim) match {
                case IntegerConstant(xEnd)                                => end(dim) = xEnd
                case OffsetIndex(_, xEndOffMax, IntegerConstant(xEnd), _) => end(dim) = xEnd + xEndOffMax
                case _ => { // no use -> try evaluation as last resort
                  try {
                    val simplified = SimplifyExpression.evalIntegral(endIndex(dim))
                    endIndex(dim) = IntegerConstant(simplified)
                    end(dim) = simplified
                  } catch {
                    case _ : EvaluationException => return null // evaluation failed -> abort
                  }
                }
              }
            }
          }
          case _ => Logger.warn("Loop index range end is not a MultiIndex"); return null
        }
      case _ => Logger.warn("Loop indices are not of type IndexRange"); return null
    }
    return (0 until numDimensions).toArray.map(dim => end(dim) - start(dim))
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

  def expandSpecial : ListBuffer[Statement] = {
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
    var compiledLoop : ForLoopStatement with OptimizationHint = null
    for (d <- 0 until numDimensions) {
      def it = VariableAccess(dimToString(d), Some(IntegerDatatype))
      val decl = VariableDeclarationStatement(IntegerDatatype, dimToString(d), Some(indices.begin(d)))
      val cond = LowerExpression(it, indices.end(d))
      val incr = AssignmentStatement(it, stepSize(d), "+=")
      if (parallelize && d == numDimensions - 1) {
        val omp = new ForLoopStatement(decl, cond, incr, wrappedBody, reduction) with OptimizationHint with OMP_PotentiallyParallel
        omp.collapse = numDimensions
        compiledLoop = omp
      } else {
        compiledLoop = new ForLoopStatement(decl, cond, incr, wrappedBody, reduction) with OptimizationHint
        wrappedBody = ListBuffer[Statement](compiledLoop)
      }
      // set optimization hints
      compiledLoop.isInnermost = d == 0
      compiledLoop.isParallel = parallelizable
    }

    var retStmts = ListBuffer[Statement]()

    // resolve omp reduction if necessary
    if (!resolveOmpReduction) {
      retStmts += compiledLoop
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
      ReplaceStringConstantsStrategy.applyStandalone(body)
      body.prepend(VariableDeclarationStatement(IntegerDatatype, "omp_tid", Some("omp_get_thread_num()")))

      retStmts += Scope(ListBuffer[Statement](decl)
        ++ init
        ++ ListBuffer[Statement](compiledLoop, red))
    }

    retStmts
  }
}

object LoopOverFragments { def defIt = "fragmentIdx" }

case class LoopOverFragments(var body : ListBuffer[Statement], var reduction : Option[Reduction] = None) extends Statement with Expandable {
  import LoopOverFragments._

  def this(body : Statement, reduction : Option[Reduction]) = this(ListBuffer(body), reduction)
  def this(body : Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverFragments\n"

  def generateBasicLoop(parallelize : Boolean) = {
    var loop = if (parallelize)
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
    loop.add(Annotation("numLoopIterations", Knowledge.domain_numFragmentsPerBlock))
    loop
  }

  override def expand : Output[StatementList] = {
    var statements = new ListBuffer[Statement]

    if (Knowledge.experimental_resolveUnreqFragmentLoops && Knowledge.domain_numFragmentsPerBlock <= 1) {
      // eliminate fragment loops in case of only one fragment per block
      statements = ListBuffer(Scope(body))

      // replace references to old loop iterator
      ReplaceStringConstantsStrategy.toReplace = defIt
      ReplaceStringConstantsStrategy.replacement = IntegerConstant(0)
      ReplaceStringConstantsStrategy.applyStandalone(statements)
    } else {
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
        ReplaceStringConstantsStrategy.applyStandalone(body)
        body.prepend(VariableDeclarationStatement(IntegerDatatype, "omp_tid", Some("omp_get_thread_num()")))

        statements += Scope(ListBuffer[Statement](decl)
          ++ init
          ++ ListBuffer[Statement](generateBasicLoop(parallelize), red))
      }
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

  override def expand : Output[ForLoopStatement] = {
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

  override def expand : Output[ForLoopStatement] = {
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

  override def expand : Output[ForLoopStatement] = {
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

  override def expand : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(IntegerDatatype, defIt, Some(0)),
      LowerExpression(defIt, Fragment.neighbors.size),
      PreIncrementExpression(defIt),
      body)
  }
}
