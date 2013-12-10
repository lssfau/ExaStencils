package exastencils.primitives

import java.io.File
import java.io.PrintWriter

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.datastructures.ir._

case class Field(name : String, codeName : String, dataType : String, numSlots : String, bcDir0 : Boolean) extends Node {
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class Scope(var body : ListBuffer[Node/*FIXME: specialization*/]) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";

    s += s"{\n";
    for (stat <- body)
      s += s"${stat.asInstanceOf[Expression]/*FIXME: remove cast*/.cpp}\n";
    s += s"}\n";

    return s;
  }
}

case class ifCond(cond : Expression, trueBranch : Array[Expression], falseBranch : Array[Expression] = Array()) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

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

case class forLoop(head : Expression, body : Array[Node/*FIXME: specialization*/]) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    var s : String = "";

    // HACK
    if (StringLiteral("int e = 0; e < fragments.size(); ++e") == head) {
      s += "#pragma omp parallel for schedule(static, 1)\n";
    }

    s += s"for (${head.cpp})\n{\n";
    for (stat <- body)
      s += s"${stat.asInstanceOf[Expression]/*FIXME: remove cast*/.cpp}\n";
    s += s"}\n";

    return s;
  }
}

case class LoopOverFragments(var body : Array[Node/*FIXME: specialization*/]) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp = "NOT VALID";
  
  def toForLoop : forLoop = {
    return forLoop(StringLiteral(s"int e = 0; e < fragments.size(); ++e"), body);
  }
}

abstract class Class extends Expression {
  var className : String = "CLASS_NAME";
  var declarations : ListBuffer[Expression] = new ListBuffer[Expression];
  var cTorInitList : ListBuffer[Expression] = new ListBuffer[Expression];
  var cTorBody : ListBuffer[Expression] = new ListBuffer[Expression];
  var dTorBody : ListBuffer[Expression] = new ListBuffer[Expression];
  var functions : ListBuffer[Function] = new ListBuffer[Function];

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

abstract class Function(var head : Expression, var body : ListBuffer[Expression]) extends Expression {
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

  var fields : ListBuffer[Field] = new ListBuffer;
  var neighbors = new ListBuffer[NeighborInfo]();

  def init = {
    for (z <- -1 to 1; y <- -1 to 1; x <- -1 to 1; if (0 != x || 0 != y || 0 != z)) {
      neighbors += new NeighborInfo(Array(x, y, z));
    }
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