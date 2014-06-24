package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer
//import exastencils.core._
//import exastencils.core.collectors._
import exastencils.knowledge._
//import exastencils.datastructures._
//import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.prettyprinting._
import exastencils.strategies._
import exastencils.omp._
import exastencils.mpi._
import exastencils.polyhedron._
import exastencils.optimization._

case class LoopOverDomain(var iterationSet : IterationSet, var field : Field, var body : ListBuffer[Statement], var reduction : Option[Reduction] = None) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = LoopOverDomain\n"

  var expCount = 0

  def expand : Statement /*FIXME: ForLoopStatement*/ = {
    var start : ListBuffer[Expression] = ListBuffer()
    var stop : ListBuffer[Expression] = ListBuffer()
    for (i <- 0 until Knowledge.dimensionality) {
      // Stefan: exchange the following 2 lines for const loop boundaries
      start += OffsetIndex(0, 1, field.layout(i).idxGhostLeftBegin - field.referenceOffset(i) + iterationSet.begin(i), ArrayAccess(iv.IterationOffsetBegin(field.domain.index), i))
      stop += OffsetIndex(-1, 0, field.layout(i).idxGhostRightEnd - field.referenceOffset(i) - iterationSet.end(i), ArrayAccess(iv.IterationOffsetEnd(field.domain.index), i))
      //      start += field.layout(i).idxGhostLeftBegin - field.referenceOffset(i) + iterationSet.begin(i)
      //      stop += field.layout(i).idxGhostRightEnd - field.referenceOffset(i) - iterationSet.end(i)
    }

    var indexRange = IndexRange(new MultiIndex(start.toArray), new MultiIndex(stop.toArray))
    SimplifyStrategy.doUntilDoneStandalone(indexRange)

    new LoopOverFragments(field.domain.index, // FIXME: define LoopOverFragments in L4 DSL
      new LoopOverDimensions(Knowledge.dimensionality, indexRange, body, iterationSet.increment, reduction, iterationSet.condition) with OMP_PotentiallyParallel with PolyhedronAccessable,
      reduction) with OMP_PotentiallyParallel
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

  override def cpp : String = "NOT VALID ; CLASS = LoopOverDimensions\n"

  def parallelizationIsReasonable : Boolean = {
    numDimensions match {
      case 2 => indices match {
        // basic version assuming Integer constants
        case IndexRange(MultiIndex(xStart : IntegerConstant, yStart : IntegerConstant, _, _),
          MultiIndex(xEnd : IntegerConstant, yEnd : IntegerConstant, _, _)) => {
          val totalNumPoints = (xEnd.v - xStart.v) * (yEnd.v - yStart.v)
          return (totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
        }
        // extended version assuming OffsetIndex nodes with Integer constants
        // INFO: this uses the maximum number of points as criteria as this defines the case displaying the upper performance bound
        case IndexRange(
          MultiIndex(
            OffsetIndex(xStartOffMin, xStartOffMax, xStart : IntegerConstant, _),
            OffsetIndex(yStartOffMin, yStartOffMax, yStart : IntegerConstant, _),
            _, _),
          MultiIndex(
            OffsetIndex(xEndOffMin, xEndOffMax, xEnd : IntegerConstant, _),
            OffsetIndex(yEndOffMin, yEndOffMax, yEnd : IntegerConstant, _),
            _, _)) => {
          val totalNumPoints = (((xEnd.v + xEndOffMax) - (xStart.v + xStartOffMin))
            * ((yEnd.v + yEndOffMax) - (yStart.v + yStartOffMin)))
          return (totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
        }
        // could not match, default is no change in parallelizability, i.e. true
        case _ => true
      }
      case 3 => indices match {
        // basic version assuming Integer constants
        case IndexRange(MultiIndex(xStart : IntegerConstant, yStart : IntegerConstant, zStart : IntegerConstant, _),
          MultiIndex(xEnd : IntegerConstant, yEnd : IntegerConstant, zEnd : IntegerConstant, _)) => {
          val totalNumPoints = (xEnd.v - xStart.v) * (yEnd.v - yStart.v) * (zEnd.v - zStart.v)
          return (totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
        }
        // extended version assuming OffsetIndex nodes with Integer constants
        // INFO: this uses the maximum number of points as criteria as this defines the case displaying the upper performance bound
        case IndexRange(
          MultiIndex(
            OffsetIndex(xStartOffMin, xStartOffMax, xStart : IntegerConstant, _),
            OffsetIndex(yStartOffMin, yStartOffMax, yStart : IntegerConstant, _),
            OffsetIndex(zStartOffMin, zStartOffMax, zStart : IntegerConstant, _), _),
          MultiIndex(
            OffsetIndex(xEndOffMin, xEndOffMax, xEnd : IntegerConstant, _),
            OffsetIndex(yEndOffMin, yEndOffMax, yEnd : IntegerConstant, _),
            OffsetIndex(zEndOffMin, zEndOffMax, zEnd : IntegerConstant, _), _)) => {
          val totalNumPoints = (((xEnd.v + xEndOffMax) - (xStart.v + xStartOffMin))
            * ((yEnd.v + yEndOffMax) - (yStart.v + yStartOffMin))
            * ((zEnd.v + zEndOffMax) - (zStart.v + zStartOffMin)))
          return (totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
        }
        // could not match, default is no change in parallelizability, i.e. true
        case _ => true
      }
      case 4 => indices match {
        // basic version assuming Integer constants
        case IndexRange(MultiIndex(xStart : IntegerConstant, yStart : IntegerConstant, zStart : IntegerConstant, wStart : IntegerConstant),
          MultiIndex(xEnd : IntegerConstant, yEnd : IntegerConstant, zEnd : IntegerConstant, wEnd : IntegerConstant)) => {
          val totalNumPoints = (xEnd.v - xStart.v) * (yEnd.v - yStart.v) * (zEnd.v - zStart.v) * (wEnd.v - wStart.v)
          return (totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
        }
        // extended version assuming OffsetIndex nodes with Integer constants
        // INFO: this uses the maximum number of points as criteria as this defines the case displaying the upper performance bound
        case IndexRange(
          MultiIndex(
            OffsetIndex(xStartOffMin, xStartOffMax, xStart : IntegerConstant, _),
            OffsetIndex(yStartOffMin, yStartOffMax, yStart : IntegerConstant, _),
            OffsetIndex(zStartOffMin, zStartOffMax, zStart : IntegerConstant, _),
            OffsetIndex(wStartOffMin, wStartOffMax, wStart : IntegerConstant, _)),
          MultiIndex(
            OffsetIndex(xEndOffMin, xEndOffMax, xEnd : IntegerConstant, _),
            OffsetIndex(yEndOffMin, yEndOffMax, yEnd : IntegerConstant, _),
            OffsetIndex(zEndOffMin, zEndOffMax, zEnd : IntegerConstant, _),
            OffsetIndex(wEndOffMin, wEndOffMax, wEnd : IntegerConstant, _))) => {
          val totalNumPoints = (((xEnd.v + xEndOffMax) - (xStart.v + xStartOffMin))
            * ((yEnd.v + yEndOffMax) - (yStart.v + yStartOffMin))
            * ((zEnd.v + zEndOffMax) - (zStart.v + zStartOffMin))
            * ((wEnd.v + zEndOffMax) - (wStart.v + wStartOffMin)))
          return (totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
        }
        // could not match, default is no change in parallelizability, i.e. true
        case _ => true
      }
    }
  }

  def expandSpecial : ForLoopStatement = {
    var parallelizable = Knowledge.omp_parallelizeLoopOverDimensions && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false })
    parallelizable = parallelizable && parallelizationIsReasonable

    var wrappedBody : ListBuffer[Statement] = (
      if (condition.isDefined)
        ListBuffer[Statement](new ConditionStatement(condition.get, body))
      else
        body)

    for (d <- 0 until numDimensions - 1) {
      wrappedBody = ListBuffer[Statement](new ForLoopStatement(
        s"int ${dimToString(d)} = " ~ indices.begin(d), s"${dimToString(d)} < " ~ indices.end(d), s"${dimToString(d)} +=" ~ stepSize(d),
        wrappedBody, reduction) with PrecalcAddresses)
    }
    val d = numDimensions - 1
    if (parallelizable) {
      var ret = new ForLoopStatement(s"int ${dimToString(d)} = " ~ indices.begin(d), s"${dimToString(d)} < " ~ indices.end(d), s"${dimToString(d)} +=" ~ stepSize(d), wrappedBody,
        reduction) with PrecalcAddresses with OMP_PotentiallyParallel
      ret.collapse = numDimensions
      ret
    } else {
      new ForLoopStatement(s"int ${dimToString(d)} = " ~ indices.begin(d), s"${dimToString(d)} < " ~ indices.end(d), s"${dimToString(d)} +=" ~ stepSize(d), wrappedBody,
        reduction) with PrecalcAddresses
    }
  }
}

case class LoopOverFragments(var domain : Int, var body : ListBuffer[Statement], var reduction : Option[Reduction] = None) extends Statement with Expandable {
  def this(domain : Int, body : Statement, reduction : Option[Reduction]) = this(domain, ListBuffer(body), reduction)
  def this(domain : Int, body : Statement) = this(domain, ListBuffer(body))

  def cpp = "NOT VALID ; CLASS = LoopOverFragments\n"

  def expand : StatementBlock = {
    val parallelizable = Knowledge.omp_parallelizeLoopOverFragments && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false })
    var statements = new ListBuffer[Statement]

    var modifiedBody : ListBuffer[Statement] = new ListBuffer
    if (domain >= 0)
      modifiedBody += new ConditionStatement(iv.IsValidForSubdomain(domain), body)
    else
      modifiedBody ++= body

    if (parallelizable)
      statements += new ForLoopStatement(s"int fragmentIdx = 0", s"fragmentIdx < " ~ Knowledge.domain_numFragsPerBlock, s"++fragmentIdx",
        modifiedBody, reduction) with OMP_PotentiallyParallel
    else
      statements += new ForLoopStatement(s"int fragmentIdx = 0", s"fragmentIdx < " ~ Knowledge.domain_numFragsPerBlock, s"++fragmentIdx",
        modifiedBody, reduction)

    if (Knowledge.useMPI && reduction.isDefined) {
      statements += new MPI_Allreduce("&" ~ reduction.get.target, new RealDatatype, 1, reduction.get.op) // FIXME: get dt and cnt from reduction
    }

    StatementBlock(statements)
  }
}
