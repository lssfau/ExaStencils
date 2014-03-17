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

// TODO: Move accepted nodes to appropriate packages

case class LoopOverDimensions(var indices : IndexRange, var body : ListBuffer[Statement], var addOMPStatements : String = "") extends Statement with Expandable {
  def this(indices : IndexRange, body : Statement, addOMPStatements : String) = this(indices, ListBuffer[Statement](body), addOMPStatements);
  def this(indices : IndexRange, body : Statement) = this(indices, ListBuffer[Statement](body));

  override def cpp : String = "NOT VALID ; CLASS = LoopOverDimensions\n";

  def expand(collector : StackCollector) : ForLoopStatement = {
    val parallelizable = Knowledge.domain_summarizeBlocks && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false });

    var wrappedBody : ListBuffer[Statement] = body; // TODO: clone?

    for (d <- 0 until Knowledge.dimensionality - 1) {
      wrappedBody = ListBuffer[Statement](new ForLoopStatement(
        s"int ${dimToString(d)} = ${indices.begin(d).cpp}", s"${dimToString(d)} <= ${indices.end(d).cpp}", s"++${dimToString(d)}",
        wrappedBody));
    }
    val d = Knowledge.dimensionality - 1;
    if (parallelizable)
      return new ForLoopStatement(s"int ${dimToString(d)} = ${indices.begin(d).cpp}", s"${dimToString(d)} <= ${indices.end(d).cpp}", s"++${dimToString(d)}", wrappedBody, addOMPStatements + " schedule(static)") with OMP_PotentiallyParallel;
    else
      return new ForLoopStatement(s"int ${dimToString(d)} = ${indices.begin(d).cpp}", s"${dimToString(d)} <= ${indices.end(d).cpp}", s"++${dimToString(d)}", wrappedBody);
  }
}

case class LoopOverFragments(var body : ListBuffer[Statement], var createFragRef : Boolean = true, var addOMPStatements : String = "") extends Statement with Expandable {
  def this(body : Statement, createFragRef : Boolean, addOMPStatements : String) = this(ListBuffer(body), createFragRef, addOMPStatements);
  def this(body : Statement, createFragRef : Boolean) = this(ListBuffer(body), createFragRef);
  def this(body : Statement) = this(ListBuffer(body));

  def cpp = "NOT VALID ; CLASS = LoopOverFragments\n";

  def expand(collector : StackCollector) : ForLoopStatement = {
    val parallelizable = !Knowledge.domain_summarizeBlocks && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false });

    if (parallelizable)
      new ForLoopStatement(s"int f = 0", s"f < ${Knowledge.domain_numFragsPerBlock}", s"++f",
        (if (createFragRef) ListBuffer[Statement]("Fragment3DCube& curFragment = *fragments[f];") else ListBuffer[Statement]())
          ++ body, addOMPStatements + " schedule(static, 1)") with OMP_PotentiallyParallel
    else
      new ForLoopStatement(s"int f = 0", s"f < ${Knowledge.domain_numFragsPerBlock}", s"++f",
        (if (createFragRef) ListBuffer[Statement]("Fragment3DCube& curFragment = *fragments[f];") else ListBuffer[Statement]())
          ++ body)
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
        "#pragma warning(disable : 4800)\n"
        + "#include <mpi.h>\n"
        + "#include \"Globals/Globals.h\"\n"
        + "#include \"Util/Log.h\"\n"
        + "#include \"Util/Vector.h\"\n"
        + "#include \"Container/Container.h\"\n"
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

case class FieldCollection() extends Node {
  var fields : ListBuffer[Field] = ListBuffer();

  def getFieldByIdentifier(identifier : String, level : Int) : Option[Field] = {
    fields.find(f => f.identifier == identifier && f.level == level)
  }
  def getFieldByCodeName(codeName : String, level : Int) : Option[Field] = {
    fields.find(f => f.codeName == codeName && f.level == level)
  }
}