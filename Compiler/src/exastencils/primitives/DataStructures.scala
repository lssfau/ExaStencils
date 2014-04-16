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
  override def cpp : String = "NOT VALID ; CLASS = LoopOverDomain\n";

  def expand(collector : StackCollector) : Statement /*FIXME: ForLoopStatement*/ = {
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

    return new LoopOverFragments( // FIXME: define LoopOverFragments in L4 DSL
      // TODO: add sth like new ConditionStatement(s"curFragment.isValidForSubdomain[${field.domain}]",
      new LoopOverDimensions(IndexRange(new MultiIndex(start.toArray), new MultiIndex(stop.toArray)), body, iterationSet.increment, reduction) with OMP_PotentiallyParallel,
      reduction) with OMP_PotentiallyParallel
  }
}

case class LoopOverDimensions(var indices : IndexRange, var body : ListBuffer[Statement], var stepSize : MultiIndex = new MultiIndex(Array.fill(3)(1)), var reduction : Option[Reduction] = None) extends Statement with Expandable {
  def this(indices : IndexRange, body : Statement, stepSize : MultiIndex, reduction : Option[Reduction]) = this(indices, ListBuffer[Statement](body), stepSize, reduction);
  def this(indices : IndexRange, body : Statement, stepSize : MultiIndex) = this(indices, ListBuffer[Statement](body), stepSize);
  def this(indices : IndexRange, body : Statement) = this(indices, ListBuffer[Statement](body));

  override def cpp : String = "NOT VALID ; CLASS = LoopOverDimensions\n";

  def expand(collector : StackCollector) : ForLoopStatement = {
    val parallelizable = Knowledge.domain_summarizeBlocks && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false });

    var wrappedBody : ListBuffer[Statement] = body; // TODO: clone?

    for (d <- 0 until Knowledge.dimensionality - 1) {
      wrappedBody = ListBuffer[Statement](new ForLoopStatement(
        s"int ${dimToString(d)} = " ~ indices.begin(d), s"${dimToString(d)} < " ~ indices.end(d), s"${dimToString(d)} +=" ~ stepSize(d),
        wrappedBody, reduction));
    }
    val d = Knowledge.dimensionality - 1;
    if (parallelizable)
      return new ForLoopStatement(s"int ${dimToString(d)} = " ~ indices.begin(d), s"${dimToString(d)} < " ~ indices.end(d), s"${dimToString(d)} +=" ~ stepSize(d), wrappedBody,
        reduction) with OMP_PotentiallyParallel;
    else
      return new ForLoopStatement(s"int ${dimToString(d)} = " ~ indices.begin(d), s"${dimToString(d)} < " ~ indices.end(d), s"${dimToString(d)} +=" ~ stepSize(d), wrappedBody,
        reduction);
  }
}

case class LoopOverFragments(var body : ListBuffer[Statement], var reduction : Option[Reduction] = None, var createFragRef : Boolean = true) extends Statement with Expandable {
  def this(body : Statement, reduction : Option[Reduction], createFragRef : Boolean) = this(ListBuffer(body), reduction, createFragRef);
  def this(body : Statement, reduction : Option[Reduction]) = this(ListBuffer(body), reduction);
  def this(body : Statement) = this(ListBuffer(body));

  def cpp = "NOT VALID ; CLASS = LoopOverFragments\n";

  def expand(collector : StackCollector) : StatementBlock = {
    val parallelizable = !Knowledge.domain_summarizeBlocks && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false });
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

abstract class Class extends Statement {
  var className : String = "CLASS_NAME";
  var declarations : ListBuffer[Statement] = ListBuffer();
  // FIXME: use specialized c'tor and d'tor nodes
  var cTorArgs : ListBuffer[Expression] = ListBuffer();
  var cTorInitList : ListBuffer[Expression] = ListBuffer();
  var cTorBody : ListBuffer[Statement] = ListBuffer();
  var dTorBody : ListBuffer[Statement] = ListBuffer();
  var functions : ListBuffer[AbstractFunctionStatement] = ListBuffer();

  def cpp : String = {
    var s : String = "";

    s += s"class $className\n{\n";

    s += s"public:\n";

    for (decl <- declarations)
      s += s"${decl.cpp}\n";

    s += s"$className (${cTorArgs.map(stat => stat.cpp).mkString(", ")})\n:\n";
    s += cTorInitList.map(stat => stat.cpp).mkString(",\n");
    s += s"{\n"
    for (stat <- cTorBody)
      s += s"${stat.cpp}\n";
    s += s"}\n";

    s += s"~$className ()\n";
    s += s"{\n"
    for (stat <- dTorBody)
      s += s"${stat.cpp}\n";
    s += s"}\n";

    for (func <- functions) {
      val function = func.asInstanceOf[FunctionStatement];
      s += s"${function.returntype.cpp} ${function.name.split("::")(1) /*FIXME: handle with reason*/ }(" + function.parameters.map(param => s"${param.dType.get.cpp} ${param.name}").mkString(", ") + ");\n";
    }

    s += s"};\n";

    return s;
  }
}

case class CommunicationFunctions() extends Node with FilePrettyPrintable {
  var functions : ListBuffer[AbstractFunctionStatement] = ListBuffer();

  override def printToFile = {
    {
      val writer = PrettyprintingManager.getPrinter(s"Primitives/CommunicationFunctions.h");

      writer << (
        (if (Knowledge.useMPI) "#pragma warning(disable : 4800)\n" else "")
        + (if (Knowledge.useMPI) "#include <mpi.h>\n" else "")
        + "#include \"Globals/Globals.h\"\n"
        + "#include \"Util/Log.h\"\n"
        + "#include \"Util/Vector.h\"\n"
        + "#include \"Primitives/Fragment3DCube.h\"\n");

      for (func <- functions) {
        val function = func.asInstanceOf[FunctionStatement];
        writer << s"${function.returntype.cpp} ${function.name}(" + function.parameters.map(param => s"${param.dType.get.cpp} ${param.name}").mkString(", ") + ");\n";
      }
    }

    var i = 0;
    for (f <- functions) {
      var s : String = "";

      val writer = PrettyprintingManager.getPrinter(s"Primitives/CommunicationFunction_$i.cpp");

      writer << "#include \"Primitives/CommunicationFunctions.h\"\n\n";
      writer << f.cpp + "\n";

      i += 1;
    }
  }
}
