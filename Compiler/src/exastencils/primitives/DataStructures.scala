package exastencils.primitives

import java.io.File
import java.io.PrintWriter

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._

case class Class extends Node {
  override def duplicate = this.copy().asInstanceOf[this.type]
  
  var className : String = "CLASS_NAME";
  var declarations : ListBuffer[String] = new ListBuffer[String];
  var cTorInitList : ListBuffer[String] = new ListBuffer[String];
  var cTorBody : ListBuffer[String] = new ListBuffer[String];
  var dTorBody : ListBuffer[String] = new ListBuffer[String];

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

object FragmentClass extends Class {
  override def duplicate = this.copy().asInstanceOf[this.type]

  var neighbors = new ListBuffer[NeighborInfo]();
  className = "Fragment3DCube";

  def init = {
    for (z <- -1 to 1; y <- -1 to 1; x <- -1 to 1; if (0 != x || 0 != y || 0 != z)) {
      neighbors += new NeighborInfo(Array(x, y, z));
    }
  }

  override def toString_cpp : String = {
    val writer = new PrintWriter(new File(s"C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ExaStencils_DSL/Poisson3D/Primitives/Fragment3DCube_TEMP.h"));
    writer.write(super.toString_cpp);
    writer.close();
    return "";
  }
}

case class Fragment extends Node {
  override def duplicate = this.copy().asInstanceOf[this.type]

  FragmentClass.init;
//  FragmentClass.addDeclarations;

  val functions : ListBuffer[String] = new ListBuffer()
  val members : ListBuffer[String] = new ListBuffer()
  var fields : ListBuffer[Field] = new ListBuffer;
  
  def addCtor(fields : ListBuffer[Field]) : Unit = {
    var s : String = "";
    s += "\tFragment ()\n\t{\n";
    for (f <- fields)
      s += s"\t\t${f.codeName} = new ${f.dataType}[ENTER_SIZE_HERE];\n";
    s += "\t}\n";

    functions += s;
  }

  def addDtor(fields : ListBuffer[Field]) : Unit = {
    var s : String = "";
    s += "\t~Fragment ()\n\t{\n";
    for (f <- fields)
      s += s"\t\tif (${f.codeName}) { delete[] ${f.codeName}; ${f.codeName} = 0; }\n";
    s += "\t}\n";

    functions += s;
  }

  def addSync(fields : ListBuffer[Field]) : Unit = {
    functions += (new WaitForMPIReq).toString_cpp;
    for (field <- fields) {
      val maxLevel = 9;
      functions += (new ExchangeDataSplitter(field, maxLevel)).toString_cpp;
      for (level <- (0 to maxLevel)) {
        functions += (new ExchangeData(field, level)).toString_cpp;
      }
    }
  }

  def addFields(fields : ListBuffer[Field]) : Unit = {
    for (f <- fields)
      members += s"\t${f.dataType}* ${f.codeName};\n";
  }

  def toString_cpp : String = {
    var i = 0;
    for (f <- functions) {
      var s : String = "";

      s += "#include \"Primitives/Fragment3DCube.h\"\n\n";

      s += s"${f}\n";

      val writer = new PrintWriter(new File(s"C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ExaStencils_DSL/Poisson3D/Primitives/Fragment3DCube_$i.cpp"));
      writer.write(s);
      writer.close();

      i += 1;
    }

    return "";
  }
};