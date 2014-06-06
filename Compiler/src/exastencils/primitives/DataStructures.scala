package exastencils.primitives

import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.core.collectors._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.prettyprinting._
import exastencils.omp._
import exastencils.mpi._
import exastencils.polyhedron._
import exastencils.strategies.SimplifyStrategy

// TODO: Move accepted nodes to appropriate packages

case class LoopOverDomain(var iterationSetIdentifier : String, var fieldIdentifier : String, var level : Int, var body : ListBuffer[Statement], var reduction : Option[Reduction] = None) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = LoopOverDomain\n"

  var expCount = 0

  def expand : Statement /*FIXME: ForLoopStatement*/ = {
    val fieldCollection = StateManager.findFirst[FieldCollection]().get
    val iterationSetCollection = StateManager.findFirst[IterationSetCollection]().get

    val field = fieldCollection.getFieldByIdentifier(fieldIdentifier, level).get
    val iterationSet = iterationSetCollection.getIterationSetByIdentifier(iterationSetIdentifier).get

    var start : ListBuffer[Expression] = ListBuffer()
    var stop : ListBuffer[Expression] = ListBuffer()
    for (i <- 0 until Knowledge.dimensionality) {
      // Stefan: exchange the following 2 lines for const loop boundaries
      start += OffsetIndex(0, 1, field.layout(i).idxGhostLeftBegin - field.referenceOffset(i) + iterationSet.begin(i), s"curFragment.iterationOffsetBegin[${field.domain}][$i]")
      stop += OffsetIndex(-1, 0, field.layout(i).idxGhostRightEnd - field.referenceOffset(i) - iterationSet.end(i), s"curFragment.iterationOffsetEnd[${field.domain}][$i]")
      //      start += field.layout(i).idxGhostLeftBegin - field.referenceOffset(i) + iterationSet.begin(i)
      //      stop += field.layout(i).idxGhostRightEnd - field.referenceOffset(i) - iterationSet.end(i)
    }

    var indexRange = IndexRange(new MultiIndex(start.toArray), new MultiIndex(stop.toArray))
    SimplifyStrategy.doUntilDoneStandalone(indexRange)

    new LoopOverFragments(field.domain, // FIXME: define LoopOverFragments in L4 DSL
      new LoopOverDimensions(indexRange, body, iterationSet.increment, reduction, iterationSet.condition) with OMP_PotentiallyParallel with PolyhedronAccessable,
      reduction) with OMP_PotentiallyParallel
  }
}

case class LoopOverDimensions(var indices : IndexRange,
    var body : ListBuffer[Statement],
    var stepSize : MultiIndex = new MultiIndex(Array.fill(3)(1)),
    var reduction : Option[Reduction] = None,
    var condition : Option[Expression] = None) extends Statement {
  def this(indices : IndexRange, body : Statement, stepSize : MultiIndex, reduction : Option[Reduction], condition : Option[Expression]) = this(indices, ListBuffer[Statement](body), stepSize, reduction, condition)
  def this(indices : IndexRange, body : Statement, stepSize : MultiIndex, reduction : Option[Reduction]) = this(indices, ListBuffer[Statement](body), stepSize, reduction)
  def this(indices : IndexRange, body : Statement, stepSize : MultiIndex) = this(indices, ListBuffer[Statement](body), stepSize)
  def this(indices : IndexRange, body : Statement) = this(indices, ListBuffer[Statement](body))

  override def cpp : String = "NOT VALID ; CLASS = LoopOverDimensions\n"

  def parallelizationIsReasonable : Boolean = {
    indices match {
      // basic version assuming Integer constants
      // 2D
      case IndexRange(MultiIndex(xStart : IntegerConstant, yStart : IntegerConstant, _ : NullExpression),
        MultiIndex(xEnd : IntegerConstant, yEnd : IntegerConstant, _ : NullExpression)) => {
        val totalNumPoints = (xEnd.v - xStart.v) * (yEnd.v - yStart.v)
        return (totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
      }
      // 3D
      case IndexRange(MultiIndex(xStart : IntegerConstant, yStart : IntegerConstant, zStart : IntegerConstant),
        MultiIndex(xEnd : IntegerConstant, yEnd : IntegerConstant, zEnd : IntegerConstant)) => {
        val totalNumPoints = (xEnd.v - xStart.v) * (yEnd.v - yStart.v) * (zEnd.v - zStart.v)
        return (totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
      }
      // extended version assuming OffsetIndex nodes with Integer constants
      // INFO: this uses the maximum number of points as criteria as this defines the case displaying the upper performance bound
      // 2D
      case IndexRange(
        MultiIndex(
          OffsetIndex(xStartOffMin, xStartOffMax, xStart : IntegerConstant, _),
          OffsetIndex(yStartOffMin, yStartOffMax, yStart : IntegerConstant, _),
          _ : NullExpression),
        MultiIndex(
          OffsetIndex(xEndOffMin, xEndOffMax, xEnd : IntegerConstant, _),
          OffsetIndex(yEndOffMin, yEndOffMax, yEnd : IntegerConstant, _),
          _ : NullExpression)) => {
        val totalNumPoints = (((xEnd.v + xEndOffMax) - (xStart.v + xStartOffMin))
          * ((yEnd.v + yEndOffMax) - (yStart.v + yStartOffMin)))
        return (totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
      }
      // 3D
      case IndexRange(
        MultiIndex(
          OffsetIndex(xStartOffMin, xStartOffMax, xStart : IntegerConstant, _),
          OffsetIndex(yStartOffMin, yStartOffMax, yStart : IntegerConstant, _),
          OffsetIndex(zStartOffMin, zStartOffMax, zStart : IntegerConstant, _)),
        MultiIndex(
          OffsetIndex(xEndOffMin, xEndOffMax, xEnd : IntegerConstant, _),
          OffsetIndex(yEndOffMin, yEndOffMax, yEnd : IntegerConstant, _),
          OffsetIndex(zEndOffMin, zEndOffMax, zEnd : IntegerConstant, _))) => {
        val totalNumPoints = (((xEnd.v + xEndOffMax) - (xStart.v + xStartOffMin))
          * ((yEnd.v + yEndOffMax) - (yStart.v + yStartOffMin))
          * ((zEnd.v + zEndOffMax) - (zStart.v + zStartOffMin)))
        return (totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
      }
      // could not match, default is no change in parallelizability, i.e. true
      case _ => true
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

    for (d <- 0 until Knowledge.dimensionality - 1) {
      wrappedBody = ListBuffer[Statement](new ForLoopStatement(
        s"int ${dimToString(d)} = " ~ indices.begin(d), s"${dimToString(d)} < " ~ indices.end(d), s"${dimToString(d)} +=" ~ stepSize(d),
        wrappedBody, reduction))
    }
    val d = Knowledge.dimensionality - 1
    if (parallelizable) {
      var ret = new ForLoopStatement(s"int ${dimToString(d)} = " ~ indices.begin(d), s"${dimToString(d)} < " ~ indices.end(d), s"${dimToString(d)} +=" ~ stepSize(d), wrappedBody,
        reduction) with OMP_PotentiallyParallel
      ret.collapse = Knowledge.dimensionality
      ret
    } else {
      new ForLoopStatement(s"int ${dimToString(d)} = " ~ indices.begin(d), s"${dimToString(d)} < " ~ indices.end(d), s"${dimToString(d)} +=" ~ stepSize(d), wrappedBody,
        reduction)
    }
  }
}

case class LoopOverFragments(var domain : Int, var body : ListBuffer[Statement], var reduction : Option[Reduction] = None, var createFragRef : Boolean = true) extends Statement with Expandable {
  def this(domain : Int, body : Statement, reduction : Option[Reduction], createFragRef : Boolean) = this(domain, ListBuffer(body), reduction, createFragRef)
  def this(domain : Int, body : Statement, reduction : Option[Reduction]) = this(domain, ListBuffer(body), reduction)
  def this(domain : Int, body : Statement) = this(domain, ListBuffer(body))

  def cpp = "NOT VALID ; CLASS = LoopOverFragments\n"

  def expand : StatementBlock = {
    val parallelizable = Knowledge.omp_parallelizeLoopOverFragments && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false })
    var statements = new ListBuffer[Statement]

    var modifiedBody : ListBuffer[Statement] = new ListBuffer
    if (createFragRef)
      modifiedBody += "Fragment3DCube& curFragment = *fragments[f]"
    if (domain >= 0 && createFragRef)
      modifiedBody += new ConditionStatement(FragMember_IsValidForSubdomain(domain), body)
    else
      modifiedBody ++= body

    if (parallelizable)
      statements += new ForLoopStatement(s"int f = 0", s"f < " ~ Knowledge.domain_numFragsPerBlock, s"++f",
        modifiedBody, reduction) with OMP_PotentiallyParallel
    else
      statements += new ForLoopStatement(s"int f = 0", s"f < " ~ Knowledge.domain_numFragsPerBlock, s"++f",
        modifiedBody, reduction)

    if (Knowledge.useMPI && reduction.isDefined) {
      statements += new MPI_Allreduce("&" ~ reduction.get.target, 1, reduction.get.op)
    }

    StatementBlock(statements)
  }
}

case class CommunicationFunctions(var functions : ListBuffer[AbstractFunctionStatement] = ListBuffer()) extends Node with FilePrettyPrintable {
  override def printToFile = {
    {
      val writer = PrettyprintingManager.getPrinter(s"Primitives/CommunicationFunctions.h")

      writer << (
        "#define _USE_MATH_DEFINES\n"
        + "#include <cmath>\n"
        + (if (Knowledge.useMPI) "#pragma warning(disable : 4800)\n" else "")
        + (if (Knowledge.useMPI) "#include <mpi.h>\n" else "")
        + "#include \"Globals/Globals.h\"\n"
        + "#include \"Util/Log.h\"\n"
        + "#include \"Util/Vector.h\"\n"
        + "#include \"Primitives/Fragment3DCube.h\"\n")

      for (func <- functions) {
        val function = func.asInstanceOf[FunctionStatement]
        writer << s"${function.returntype.cpp} ${function.name.cpp}(" + function.parameters.map(param => s"${param.dType.get.cpp} ${param.name}").mkString(", ") + ");\n"
      }
    }

    var i = 0
    for (f <- functions) {
      var s : String = ""

      val writer = PrettyprintingManager.getPrinter(s"Primitives/CommunicationFunction_$i.cpp")

      writer << "#include \"Primitives/CommunicationFunctions.h\"\n\n"
      writer << f.cpp + "\n"

      i += 1
    }
  }
}
