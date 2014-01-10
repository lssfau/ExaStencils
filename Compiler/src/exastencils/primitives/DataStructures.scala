package exastencils.primitives

import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.prettyprinting._
import exastencils.omp._

case class Field(name : String, codeName : String, dataType : String, numSlots : Int, bcDir0 : Boolean) extends Node {
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class LoopOverDimensions(var indices : IndexRange, var body : ListBuffer[Statement], var addOMPStatements : String = "") extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def this(indices : IndexRange, body : Statement, addOMPStatements : String) = this(indices, ListBuffer[Statement](body), addOMPStatements);
  def this(indices : IndexRange, body : Statement) = this(indices, ListBuffer[Statement](body));

  override def cpp : String = "NOT VALID ; CLASS = LoopOverDimensions\n";

  def expand : ForLoopStatement = {
        val parallelizable = Knowledge.summarizeBlocks && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false });
    
        if (parallelizable)
      new ForLoopStatement( /*s"unsigned -> f***ing omp*/ s"int ${dimToString(2)} = ${indices.begin(2)}", s"${dimToString(2)} <= ${indices.end(2)}", s"++${dimToString(2)}",
          new ForLoopStatement(s"unsigned int ${dimToString(1)} = ${indices.begin(1)}", s"${dimToString(1)} <= ${indices.end(1)}", s"++${dimToString(1)}",
            new ForLoopStatement(s"unsigned int ${dimToString(0)} = ${indices.begin(0)}", s"${dimToString(0)} <= ${indices.end(0)}", s"++${dimToString(0)}",
              body)), addOMPStatements + " schedule(static)") with OMP_PotentiallyParallel
    else
      new ForLoopStatement( /*s"unsigned -> f***ing omp*/ s"int ${dimToString(2)} = ${indices.begin(2)}", s"${dimToString(2)} <= ${indices.end(2)}", s"++${dimToString(2)}",
          new ForLoopStatement(s"unsigned int ${dimToString(1)} = ${indices.begin(1)}", s"${dimToString(1)} <= ${indices.end(1)}", s"++${dimToString(1)}",
            new ForLoopStatement(s"unsigned int ${dimToString(0)} = ${indices.begin(0)}", s"${dimToString(0)} <= ${indices.end(0)}", s"++${dimToString(0)}",
              body)))
  }
}

case class FieldAccess(var field : Field, var level : Expression, var slot : Expression, var index : Expression) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = {
    s"curFragment.${field.codeName}[${slot.cpp}][${level.cpp}]->data[${index.cpp}]";
  }
}

case class LocalNeighborFieldAccess(var neighborPtr : Expression, var field : Field, var level : Expression, var slot : Expression, var index : Expression) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = {
    s"${neighborPtr.cpp}->${field.codeName}[${slot.cpp}][${level.cpp}]->data[${index.cpp}]";
  }
}

case class LoopOverFragments(var body : ListBuffer[Statement], var createFragRef : Boolean = true, var addOMPStatements : String = "") extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def this(body : Statement, createFragRef : Boolean, addOMPStatements : String) = this(ListBuffer(body), createFragRef, addOMPStatements);
  def this(body : Statement, createFragRef : Boolean) = this(ListBuffer(body), createFragRef);
  def this(body : Statement) = this(ListBuffer(body));

  def cpp = "NOT VALID ; CLASS = LoopOverFragments\n";

  def expand : ForLoopStatement = {
    val parallelizable = !Knowledge.summarizeBlocks && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false });

    if (parallelizable)
      new ForLoopStatement(s"int f = 0", s"f < ${Knowledge.numFragsPerBlock}", s"++f",
        (if (createFragRef) ListBuffer[Statement]("Fragment3DCube& curFragment = *fragments[f];") else ListBuffer[Statement]())
          ++ body, addOMPStatements + " schedule(static, 1)") with OMP_PotentiallyParallel
    else
      new ForLoopStatement(s"int f = 0", s"f < ${Knowledge.numFragsPerBlock}", s"++f",
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
      s += s"${function.returntype.cpp} ${function.name.split("::")(1) /*FIXME: handle with reason*/ }(" + function.parameters.map(param => s"${param.datatype.cpp} ${param.name}").mkString(", ") + ");\n";
    }

    s += s"};\n";

    return s;
  }
}

case class CommunicationFunctions() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  var functions : ListBuffer[AbstractFunctionStatement] = ListBuffer();

  override def printToFile = {
    {
      val writer = PrettyprintingManager.getPrinter(s"Primitives/CommunicationFunctions.h");

      writer << ("#ifndef	COMMUNICATION_FUNCTIONS_H\n"
        + "#define	COMMUNICATION_FUNCTIONS_H\n"
        + "#pragma warning(disable : 4800)\n"
        + "#include <mpi.h>\n"
        + "#include \"Util/Log.h\"\n"
        + "#include \"Util/Vector.h\"\n"
        + "#include \"Container/Container.h\"\n"
        + "#include \"Primitives/Fragment3DCube.h\"\n");

      for (func <- functions) {
        val function = func.asInstanceOf[FunctionStatement];
        writer << s"${function.returntype.cpp} ${function.name}(" + function.parameters.map(param => s"${param.datatype.cpp} ${param.name}").mkString(", ") + ");\n";
      }

      writer << "#endif\n";

      writer.close(); // FIXME: finalize
    }

    var i = 0;
    for (f <- functions) {
      var s : String = "";

      val writer = PrettyprintingManager.getPrinter(s"Primitives/CommunicationFunction_$i.cpp");

      writer << "#include \"Primitives/CommunicationFunctions.h\"\n\n";
      writer << f.cpp + "\n";

      writer.close(); // FIXME: finalize

      i += 1;
    }
  }
}

case class FieldCollection() extends Node {
  override def duplicate = this.copy().asInstanceOf[this.type]

  var fields : ListBuffer[Field] = ListBuffer();

  def getFieldByName(name : String) : Option[Field] = {
    fields.find(f => f.name == name)
  }
  def getFieldByCodeName(codeName : String) : Option[Field] = {
    fields.find(f => f.codeName == codeName)
  }
}