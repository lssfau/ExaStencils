package exastencils.primitives

import java.io.File
import java.io.PrintWriter

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

trait Expandable {
  def expand() : Node
}

case class Field(name : String, codeName : String, dataType : String, numSlots : String, bcDir0 : Boolean) extends Node {
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

abstract class Class extends Expression {
  var className : String = "CLASS_NAME";
  var declarations : ListBuffer[Statement] = ListBuffer();
  var cTorInitList : ListBuffer[Expression] = ListBuffer();
  var cTorBody : ListBuffer[Statement] = ListBuffer();
  var dTorBody : ListBuffer[Statement] = ListBuffer();
  var functions : ListBuffer[AbstractFunctionStatement] = ListBuffer();

  def cpp : String = {
    var s : String = "";

    for (decl <- declarations)
      s += s"${decl.cpp}\n";

    s += s"$className::$className ()\n:\n";
    s += cTorInitList.map(stat => stat.cpp).mkString(",\n");

    s += s"{\n"

    for (stat <- cTorBody)
      s += s"${stat.cpp}\n";

    s += s"}\n";

    s += s"$className::~$className ()\n";
    s += s"{\n"

    for (stat <- dTorBody)
      s += s"${stat.cpp}\n";

    s += s"}\n";

    return s;
  }
}

case class FragmentClass extends Class {
  override def duplicate = this.copy().asInstanceOf[this.type]

  className = "Fragment3DCube";

  var fields : ListBuffer[Field] = ListBuffer();
  var neighbors : ListBuffer[NeighborInfo] = ListBuffer();

  def init = {
    for (z <- -1 to 1; y <- -1 to 1; x <- -1 to 1; if (0 != x || 0 != y || 0 != z)) {
      neighbors += new NeighborInfo(Array(x, y, z));
    }

    // FIXME: use Number of neighbors
    declarations += s"bool neighbor_isValid[27];";
    declarations += s"bool neighbor_isRemote[27];";
    declarations += s"Fragment3DCube* neighbor_localPtr[27];";
    declarations += s"exa_id_t neighbor_fragmentId[27];";
    declarations += s"int neighbor_remoteRank[27];";

    cTorBody += new ForLoopStatement(s"unsigned int i = 0", s"i < 27", s"++i",
      ListBuffer[Statement](
        s"neighbor_isValid[i] = false;",
        s"neighbor_isRemote[i] = false;",
        s"neighbor_localPtr[i] = NULL;",
        s"neighbor_fragmentId[i] = -1;",
        s"neighbor_remoteRank[i] = MPI_PROC_NULL;"));
  }

  override def cpp : String = {
    {
      val writer = new PrintWriter(new File(s"C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ExaStencils_DSL/Poisson3D/Primitives/Fragment3DCube_TEMP.h"));
      writer.write(super.cpp);
      writer.close();
    }

    var i = 0;
    for (f <- functions) {
      var s : String = "";

      s += "#include \"Primitives/Fragment3DCube.h\"\n\n";

      s += s"${f.cpp}\n";

      val writer = new PrintWriter(new File(s"C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ExaStencils_DSL/Poisson3D/Primitives/Fragment3DCube_$i.cpp"));
      writer.write(s);
      writer.close();

      i += 1;
    }

    return "";
  }
}