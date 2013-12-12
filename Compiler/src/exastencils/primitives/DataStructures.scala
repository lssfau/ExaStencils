package exastencils.primitives

import java.io.File
import java.io.PrintWriter

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.datastructures.ir._

import exastencils.datastructures.ir.ImplicitConversions

trait Expandable {
  def expand() : Node
}

case class Field(name : String, codeName : String, dataType : String, numSlots : String, bcDir0 : Boolean) extends Node {
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class Scope(var body : ListBuffer[Statement]) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";

    s += s"{\n";
    for (stat <- body)
      s += s"${stat.cpp}\n";
    s += s"}\n";

    return s;
  }
}

case class ifCond(var cond : Expression, var trueBranch : ListBuffer[Statement], var falseBranch : ListBuffer[Statement]) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def this(cond : Expression, trueBranch : ListBuffer[Statement]) = this(cond, trueBranch, ListBuffer[Statement]());
  def this(cond : Expression, trueBranch : Statement) = this(cond, ListBuffer(trueBranch));

  def this(cond : Expression, trueBranch : Statement, falseBranch : Statement) = this(cond, ListBuffer(trueBranch), ListBuffer(falseBranch));
  def this(cond : Expression, trueBranch : ListBuffer[Statement], falseBranch : Statement) = this(cond, trueBranch, ListBuffer(falseBranch));
  def this(cond : Expression, trueBranch : Statement, falseBranch : ListBuffer[Statement]) = this(cond, ListBuffer(trueBranch), falseBranch);

  def cpp : String = {
    var s : String = "";

    s += s"if (${cond.cpp})\n{\n";
    for (stat <- trueBranch)
      s += s"${stat.cpp}\n";
    s += s"}\n";
    if (falseBranch.length > 0) {
      s += s"else\n{\n";
      for (stat <- falseBranch)
        s += s"${stat.cpp}\n";
      s += s"}\n";
    }

    return s;
  }
}

case class forLoop(var head : Expression, var body : ListBuffer[Statement]) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def this(head : Expression, body : Statement) = this(head, ListBuffer[Statement](body));

  def cpp : String = {
    var s : String = "";

    // HACK
    if (StringLiteral("int e = 0; e < fragments.size(); ++e") == head) {
      s += "#pragma omp parallel for schedule(static, 1)\n";
    }

    s += s"for (${head.cpp})\n{\n";
    for (stat <- body)
      s += s"${stat.cpp}\n";
    s += s"}\n";

    return s;
  }
}

case class LoopOverFragments(var body : ListBuffer[Statement]) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def this(body : Statement) = this(ListBuffer(body));

  def cpp = "NOT VALID ; CLASS = LoopOverFragments\n";

  def toForLoop : forLoop = {
    return forLoop(StringLiteral(s"int e = 0; e < fragments.size(); ++e"),
      ListBuffer(ImplicitConversions.StringToStatement("Fragment3DCube& curFragment = *(fragments[e].get());"))
        ++ body);
  }
}

abstract class Class extends Expression {
  var className : String = "CLASS_NAME";
  var declarations : ListBuffer[Statement] = ListBuffer();
  var cTorInitList : ListBuffer[Expression] = ListBuffer();
  var cTorBody : ListBuffer[Statement] = ListBuffer();
  var dTorBody : ListBuffer[Statement] = ListBuffer();
  var functions : ListBuffer[Function] = ListBuffer();

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

abstract class Function(var head : Expression, var body : ListBuffer[Statement]) extends Expression {
  def cpp : String = {
    var s : String = "";

    s += s"${head.cpp}\n";
    s += s"{\n"

    for (stat <- body)
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

    // FIXME: use Number of neighbors // TODO: wtf can the conversion not be applied automatically?
    declarations += ImplicitConversions.StringToStatement(s"bool neighbor_isValid[27];");
    declarations += ImplicitConversions.StringToStatement(s"bool neighbor_isRemote[27];");
    declarations += ImplicitConversions.StringToStatement(s"Fragment3DCube* neighbor_localPtr[27];");
    declarations += ImplicitConversions.StringToStatement(s"exa_id_t neighbor_fragmentId[27];");
    declarations += ImplicitConversions.StringToStatement(s"int neighbor_remoteRank[27];");
    
    cTorBody += new forLoop(StringLiteral(s"unsigned int i = 0; i < 27; ++i"), ListBuffer(
      ImplicitConversions.StringToStatement(s"neighbor_isValid[i] = false;"),
      ImplicitConversions.StringToStatement(s"neighbor_isRemote[i] = false;"),
      ImplicitConversions.StringToStatement(s"neighbor_localPtr[i] = NULL;"),
      ImplicitConversions.StringToStatement(s"neighbor_fragmentId[i] = -1;"),
      ImplicitConversions.StringToStatement(s"neighbor_remoteRank[i] = MPI_PROC_NULL;")));
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