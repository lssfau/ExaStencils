package exastencils.primitives

import java.io.File
import java.io.PrintWriter

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._

case class Field(name : String, codeName : String, dataType : String, numSlots : String, bcDir0 : Boolean) extends Node {
  override def duplicate = this.copy().asInstanceOf[this.type]
}

case class ifCond(cond : String, trueBranch : Array[String], falseBranch : Array[String] = Array[String]()) extends Node {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def toString_cpp : String = {
    var s : String = "";

    s += s"if ($cond)\n{\n";
    for (stat <- trueBranch)
      s += s"$stat\n";
    s += s"}\n";
    if (falseBranch.length > 0) {
      s += s"else\n{\n";
      for (stat <- falseBranch)
        s += s"$stat\n";
      s += s"}\n";
    }

    return s;
  }
}

case class forLoop(head : String, body : Array[String]) extends Node {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def toString_cpp : String = {
    var s : String = "";

    // HACK
    if ("int e = 0; e < fragments.size(); ++e" == head) {
      s += "#pragma omp parallel for schedule(static, 1)\n";
    }

    s += s"for ($head)\n{\n";
    for (stat <- body)
      s += s"$stat\n";
    s += s"}\n";

    return s;
  }
}

abstract class Class extends Node {
  var className : String = "CLASS_NAME";
  var declarations : ListBuffer[String] = new ListBuffer[String];
  var cTorInitList : ListBuffer[String] = new ListBuffer[String];
  var cTorBody : ListBuffer[String] = new ListBuffer[String];
  var dTorBody : ListBuffer[String] = new ListBuffer[String];
  var functions : ListBuffer[Function] = new ListBuffer[Function];

  def toString_cpp : String = {
    var s : String = "";

    for (decl <- declarations)
      s += s"$decl\n";

    s += s"$className::$className ()\n:\n";
    s += cTorInitList.map(stat => stat).mkString(",\n");

    s += s"{\n"

    for (stat <- cTorBody)
      s += s"$stat\n";

    s += s"}\n";

    s += s"$className::~$className ()\n";
    s += s"{\n"

    for (stat <- dTorBody)
      s += s"$stat\n";

    s += s"}\n";

    return s;
  }
}

abstract class Function(var head : String, var body : ListBuffer[String]) extends Node {
  def toString_cpp : String = {
    var s : String = "";

    s += s"$head\n";
    s += s"{\n"

    for (stat <- body)
      s += s"$stat\n";

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

  override def toString_cpp : String = {
    {
      val writer = new PrintWriter(new File(s"C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ExaStencils_DSL/Poisson3D/Primitives/Fragment3DCube_TEMP.h"));
      writer.write(super.toString_cpp);
      writer.close();
    }
    
    var i = 0;
    for (f <- functions) {
      var s : String = "";

      s += "#include \"Primitives/Fragment3DCube.h\"\n\n";

      s += s"${f.toString_cpp}\n";

      val writer = new PrintWriter(new File(s"C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ExaStencils_DSL/Poisson3D/Primitives/Fragment3DCube_$i.cpp"));
      writer.write(s);
      writer.close();

      i += 1;
    }

    return "";
  }
}