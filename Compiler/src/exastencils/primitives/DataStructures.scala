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

// TODO: Move accepted nodes to appropriate packages

case class LoopOverDomain(var iterationSetIdentifier : String, var fieldIdentifier : String, var level : Int, var body : ListBuffer[Statement], var reduction : Option[Reduction] = None) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = LoopOverDomain\n"

  var expCount = 0

  def expand : Statement /*FIXME: ForLoopStatement*/ = {
    // HACK: pre-expand body
    //if (0 == expCount) {
    //  expCount = 1
    //  return this
    //}

    val fieldCollection = StateManager.findFirst[FieldCollection]().get
    val iterationSetCollection = StateManager.findFirst[IterationSetCollection]().get

    val field = fieldCollection.getFieldByIdentifier(fieldIdentifier, level).get
    val iterationSet = iterationSetCollection.getIterationSetByIdentifier(iterationSetIdentifier).get

    var start : ListBuffer[Expression] = ListBuffer()
    var stop : ListBuffer[Expression] = ListBuffer()
    for (i <- 0 until Knowledge.dimensionality) {
      start += field.layout(i).idxGhostLeftBegin - field.referenceOffset(i) + iterationSet.begin(i)
      stop += field.layout(i).idxGhostRightEnd - field.referenceOffset(i) - iterationSet.end(i)
    }

    //    var temp = new Temp(field,
    //      ListBuffer[NeighborInfo](
    //        new NeighborInfo(Array(-1, 0, 0), 0),
    //        new NeighborInfo(Array(+1, 0, 0), 1),
    //        new NeighborInfo(Array(0, -1, 0), 2),
    //        new NeighborInfo(Array(0, +1, 0), 3),
    //        new NeighborInfo(Array(0, 0, -1), 4),
    //        new NeighborInfo(Array(0, 0, +1), 5)),
    //      new MultiIndex(start.toArray), new MultiIndex(stop.toArray), iterationSet.increment, body, reduction)
    //
    //    return new LoopOverFragments( // FIXME: define LoopOverFragments in L4 DSL
    //      // TODO: add sth like new ConditionStatement(s"curFragment.isValidForSubdomain[${field.domain}]",
    //      temp.gen,
    //      reduction) with OMP_PotentiallyParallel

    return new LoopOverFragments( // FIXME: define LoopOverFragments in L4 DSL
      // TODO: add sth like new ConditionStatement(s"curFragment.isValidForSubdomain[${field.domain}]",
      new LoopOverDimensions(IndexRange(new MultiIndex(start.toArray), new MultiIndex(stop.toArray)), body, iterationSet.increment, reduction) with OMP_PotentiallyParallel,
      reduction) with OMP_PotentiallyParallel
  }
}

//class Temp(var field : Field, var neighbors : ListBuffer[NeighborInfo], var start : MultiIndex, var stop : MultiIndex, var increment : MultiIndex, var body : ListBuffer[Statement], var reduction : Option[Reduction]) {
//
//  def gen() : ListBuffer[Statement] = {
//    var conditions : ListBuffer[Statement] = new ListBuffer
//
//    println("Starting Temp gen")
//
//    for (i0 <- 0 to 1)
//      for (i1 <- 0 to 1)
//        for (i2 <- 0 to 1)
//          for (i3 <- 0 to 1)
//            for (i4 <- 0 to 1)
//              for (i5 <- 0 to 1) {
//                var newStart = Duplicate(start)
//                var newStop = Duplicate(stop)
//
//                if (0 == i0) newStart.index_0 = newStart.index_0 + increment.index_0
//                if (0 == i1) newStop.index_0 = newStop.index_0 - increment.index_0
//                if (0 == i2) newStart.index_1 = newStart.index_1 + increment.index_1
//                if (0 == i3) newStop.index_1 = newStop.index_1 - increment.index_1
//                if (0 == i4) newStart.index_2 = newStart.index_2 + increment.index_2
//                if (0 == i5) newStop.index_2 = newStop.index_2 - increment.index_2
//
//                conditions += new ConditionStatement(
//                  (if (0 == i0) getNeighInfo_IsInvalid(neighbors(0), field.domain) else getNeighInfo_IsValid(neighbors(0), field.domain))
//                    And (if (0 == i1) getNeighInfo_IsInvalid(neighbors(1), field.domain) else getNeighInfo_IsValid(neighbors(1), field.domain))
//                    And (if (0 == i2) getNeighInfo_IsInvalid(neighbors(2), field.domain) else getNeighInfo_IsValid(neighbors(2), field.domain))
//                    And (if (0 == i3) getNeighInfo_IsInvalid(neighbors(3), field.domain) else getNeighInfo_IsValid(neighbors(3), field.domain))
//                    And (if (0 == i4) getNeighInfo_IsInvalid(neighbors(4), field.domain) else getNeighInfo_IsValid(neighbors(4), field.domain))
//                    And (if (0 == i5) getNeighInfo_IsInvalid(neighbors(5), field.domain) else getNeighInfo_IsValid(neighbors(5), field.domain)),
//                  new LoopOverDimensions(IndexRange(newStart, newStop), Duplicate(body), Duplicate(increment), Duplicate(reduction)) with OMP_PotentiallyParallel)
//              }
//
//    println("Finished Temp gen with " + conditions.size + " items")
//
//    conditions
//  }
//}

case class LoopOverDimensions(var indices : IndexRange, var body : ListBuffer[Statement], var stepSize : MultiIndex = new MultiIndex(Array.fill(3)(1)), var reduction : Option[Reduction] = None) extends Statement {
  def this(indices : IndexRange, body : Statement, stepSize : MultiIndex, reduction : Option[Reduction]) = this(indices, ListBuffer[Statement](body), stepSize, reduction)
  def this(indices : IndexRange, body : Statement, stepSize : MultiIndex) = this(indices, ListBuffer[Statement](body), stepSize)
  def this(indices : IndexRange, body : Statement) = this(indices, ListBuffer[Statement](body))

  override def cpp : String = "NOT VALID ; CLASS = LoopOverDimensions\n"

  def expandSpecial : ForLoopStatement = {
    var parallelizable = Knowledge.omp_parallelizeLoopOverDimensions && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false })

    indices match {
      case IndexRange(MultiIndex(xStart : IntegerConstant, yStart : IntegerConstant, zStart : IntegerConstant),
        MultiIndex(xEnd : IntegerConstant, yEnd : IntegerConstant, zEnd : IntegerConstant)) => {
        val totalNumPoints = (xEnd.v - xStart.v) * (yEnd.v - yStart.v) * (zEnd.v - zStart.v)
        if (totalNumPoints <= Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
          parallelizable = false;
      }
      case _ => ;
    }

    var wrappedBody : ListBuffer[Statement] = body // TODO: clone?

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

case class LoopOverFragments(var body : ListBuffer[Statement], var reduction : Option[Reduction] = None, var createFragRef : Boolean = true) extends Statement with Expandable {
  def this(body : Statement, reduction : Option[Reduction], createFragRef : Boolean) = this(ListBuffer(body), reduction, createFragRef)
  def this(body : Statement, reduction : Option[Reduction]) = this(ListBuffer(body), reduction)
  def this(body : Statement) = this(ListBuffer(body))

  def cpp = "NOT VALID ; CLASS = LoopOverFragments\n"

  def expand : StatementBlock = {
    val parallelizable = Knowledge.omp_parallelizeLoopOverFragments && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false })
    var statements = new ListBuffer[Statement]

    if (parallelizable)
      statements += new ForLoopStatement(s"int f = 0", s"f < " ~ Knowledge.domain_numFragsPerBlock, s"++f",
        (if (createFragRef) ListBuffer[Statement]("Fragment3DCube& curFragment = *fragments[f]") else ListBuffer[Statement]())
          ++ body, reduction) with OMP_PotentiallyParallel
    else
      statements += new ForLoopStatement(s"int f = 0", s"f < " ~ Knowledge.domain_numFragsPerBlock, s"++f",
        (if (createFragRef) ListBuffer[Statement]("Fragment3DCube& curFragment = *fragments[f]") else ListBuffer[Statement]())
          ++ body, reduction)

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
        (if (Knowledge.useMPI) "#pragma warning(disable : 4800)\n" else "")
        + (if (Knowledge.useMPI) "#include <mpi.h>\n" else "")
        + "#include \"Globals/Globals.h\"\n"
        + "#include \"Util/Log.h\"\n"
        + "#include \"Util/Vector.h\"\n"
        + "#include \"Primitives/Fragment3DCube.h\"\n")

      for (func <- functions) {
        val function = func.asInstanceOf[FunctionStatement]
        writer << s"${function.returntype.cpp} ${function.name}(" + function.parameters.map(param => s"${param.dType.get.cpp} ${param.name}").mkString(", ") + ");\n"
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
