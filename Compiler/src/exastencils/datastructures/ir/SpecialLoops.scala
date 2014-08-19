package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.omp._
import exastencils.optimization._
import exastencils.polyhedron._
import exastencils.strategies._
import exastencils.core.collectors.StackCollector

case class LoopOverPoints(var iterationSet : IterationSet, var field : Field, var body : ListBuffer[Statement], var reduction : Option[Reduction] = None) extends Statement {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = LoopOverPoints\n"

  def expandSpecial(collector : StackCollector) : Output[Statement] = {
    val insideFragLoop = collector.stack.map(node => node match { case loop : LoopOverFragments => true; case _ => false }).reduce((left, right) => left || right)
    val innerLoop = LoopOverPointsInOneFragment(field.domain.index, iterationSet, field, body, reduction)

    if (insideFragLoop)
      innerLoop
    else
      new LoopOverFragments(innerLoop, reduction) with OMP_PotentiallyParallel
  }
}

case class LoopOverPointsInOneFragment(var domain : Int, var iterationSet : IterationSet, var field : Field, var body : ListBuffer[Statement], var reduction : Option[Reduction] = None) extends Statement with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = LoopOverPointsInOneFragment\n"

  def expand : Output[Statement] = {
    var start : ListBuffer[Expression] = ListBuffer()
    var stop : ListBuffer[Expression] = ListBuffer()
    for (i <- 0 until Knowledge.dimensionality) {
      start += OffsetIndex(0, 1, field.layout(i).idxDupLeftBegin - field.referenceOffset(i) + iterationSet.begin(i), ArrayAccess(iv.IterationOffsetBegin(field.domain.index), i))
      stop += OffsetIndex(-1, 0, field.layout(i).idxDupRightEnd - field.referenceOffset(i) - iterationSet.end(i), ArrayAccess(iv.IterationOffsetEnd(field.domain.index), i))
    }

    var indexRange = IndexRange(new MultiIndex(start.toArray), new MultiIndex(stop.toArray))
    SimplifyStrategy.doUntilDoneStandalone(indexRange)

    var ret : Statement = new LoopOverDimensions(Knowledge.dimensionality, indexRange, body, iterationSet.increment, reduction, iterationSet.condition) with OMP_PotentiallyParallel with PolyhedronAccessable
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

  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = LoopOverDimensions\n"

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
      val it = VariableAccess(dimToString(d), Some(IntegerDatatype()))
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

  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = LoopOverFragments\n"

  def expand : Output[StatementList] = {
    val parallelizable = Knowledge.omp_parallelizeLoopOverFragments && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false })
    var statements = new ListBuffer[Statement]

    if (parallelizable)
      statements += new ForLoopStatement(s"int $defIt = 0", s"$defIt < " ~ Knowledge.domain_numFragsPerBlock, s"++$defIt",
        body, reduction) with OMP_PotentiallyParallel
    else
      statements += new ForLoopStatement(s"int $defIt = 0", s"$defIt < " ~ Knowledge.domain_numFragsPerBlock, s"++$defIt",
        body, reduction)

    if (Knowledge.useMPI && reduction.isDefined) {
      statements += new MPI_Allreduce("&" ~ reduction.get.target, new RealDatatype, 1, reduction.get.op) // FIXME: get dt and cnt from reduction
    }

    statements
  }
}

object LoopOverDomains { def defIt = "domainIdx" }

case class LoopOverDomains(var body : ListBuffer[Statement]) extends Statement with Expandable {
  def this(body : Statement) = this(ListBuffer(body))

  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = LoopOverDomains\n"

  def expand : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(new IntegerDatatype, LoopOverDomains.defIt, Some(0)),
      new LowerExpression(LoopOverDomains.defIt, DomainCollection.domains.size),
      AssignmentStatement(LoopOverDomains.defIt, 1, "+="),
      body)
  }
}

object LoopOverFields { def defIt = "fieldIdx" }

case class LoopOverFields(var body : ListBuffer[Statement]) extends Statement with Expandable {
  def this(body : Statement) = this(ListBuffer(body))

  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = LoopOverFields\n"

  def expand : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(new IntegerDatatype, LoopOverFields.defIt, Some(0)),
      new LowerExpression(LoopOverFields.defIt, FieldCollection.fields.size),
      AssignmentStatement(LoopOverFields.defIt, 1, "+="),
      body)
  }
}

object LoopOverLevels { def defIt = "levelIdx" }

case class LoopOverLevels(var body : ListBuffer[Statement]) extends Statement with Expandable {
  def this(body : Statement) = this(ListBuffer(body))

  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = LoopOverLevels\n"

  def expand : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(new IntegerDatatype, LoopOverLevels.defIt, Some(0)),
      new LowerExpression(LoopOverLevels.defIt, Knowledge.numLevels),
      AssignmentStatement(LoopOverLevels.defIt, 1, "+="),
      body)
  }
}

object LoopOverNeighbors { def defIt = "neighborIdx" }

case class LoopOverNeighbors(var body : ListBuffer[Statement]) extends Statement with Expandable {
  def this(body : Statement) = this(ListBuffer(body))

  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = LoopOverNeighbors\n"

  def expand : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(new IntegerDatatype, LoopOverNeighbors.defIt, Some(0)),
      new LowerExpression(LoopOverNeighbors.defIt, Fragment.neighbors.size),
      AssignmentStatement(LoopOverNeighbors.defIt, 1, "+="),
      body)
  }
}

