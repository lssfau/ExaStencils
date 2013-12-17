package exastencils.primitives

import java.io.File
import java.io.PrintWriter

import scala.collection.mutable.ListBuffer

import exastencils.core._

import exastencils.knowledge._

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class Field(name : String, codeName : String, dataType : String, numSlots : Int, bcDir0 : Boolean) extends Node {
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class LoopOverDimensions(var indices : IndexRange, var body : ListBuffer[Statement]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def this(indices : IndexRange, body : Statement) = this(indices, ListBuffer[Statement](body));

  override def cpp : String = "NOT VALID ; CLASS = LoopOverDimensions\n";

  def expand : ForLoopStatement = {
    new ForLoopStatement(s"unsigned int ${dimToString(2)} = ${indices.begin(2)}", s"${dimToString(2)} <= ${indices.end(2)}", s"++${dimToString(2)}",
      new ForLoopStatement(s"unsigned int ${dimToString(1)} = ${indices.begin(1)}", s"${dimToString(1)} <= ${indices.end(1)}", s"++${dimToString(1)}",
        new ForLoopStatement(s"unsigned int ${dimToString(0)} = ${indices.begin(0)}", s"${dimToString(0)} <= ${indices.end(0)}", s"++${dimToString(0)}",
          body)));
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

case class LoopOverFragments(var body : ListBuffer[Statement]) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def this(body : Statement) = this(ListBuffer(body));

  def cpp = "NOT VALID ; CLASS = LoopOverFragments\n";

  def expand : StatementBlock = {
    new StatementBlock(
      ListBuffer[Statement](
        "#pragma omp parallel for schedule(static, 1)", // FIXME: move to own Node
        ForLoopStatement(s"int e = 0", s"e < fragments.size()", s"++e",
          ListBuffer[Statement]("Fragment3DCube& curFragment = *(fragments[e].get());")
            ++ body)));
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
      s += s"${function.returntype.cpp} ${function.name}(" + function.parameters.map(param => s"${param.datatype.cpp} ${param.name}").mkString(", ") + ");\n";
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
      val writer = new PrintWriter(new File(Globals.printPath + s"Primitives/CommunicationFunctions.h"));

      writer.write(
        "#ifndef	COMMUNICATION_FUNCTIONS_H\n"
          + "#define	COMMUNICATION_FUNCTIONS_H\n"
          + "#pragma warning(disable : 4800)\n"
          + "#include <mpi.h>\n"
          + "#include \"Util/Defines.h\"\n"
          + "#include \"Util/Log.h\"\n"
          + "#include \"Util/TypeDefs.h\"\n"
          + "#include \"Util/Vector.h\"\n"
          + "#include \"Container/Container.h\"\n"
          + "#include \"Primitives/Fragment3DCube.h\"\n");

      for (func <- functions) {
        val function = func.asInstanceOf[FunctionStatement];
        writer.write(s"${function.returntype.cpp} ${function.name}(" + function.parameters.map(param => s"${param.datatype.cpp} ${param.name}").mkString(", ") + ");\n");
      }

      writer.write("#endif\n");

      writer.close();
    }

    var i = 0;
    for (f <- functions) {
      var s : String = "";

      s += "#include \"Primitives/CommunicationFunctions.h\"\n\n";

      s += s"${f.cpp}\n";

      val writer = new PrintWriter(new File(Globals.printPath + s"Primitives/CommunicationFunction_$i.cpp"));
      writer.write(s);
      writer.close();

      i += 1;
    }
  }
}

case class FieldCollection() extends Node {
  override def duplicate = this.copy().asInstanceOf[this.type]

  var fields : ListBuffer[Field] = ListBuffer();
}