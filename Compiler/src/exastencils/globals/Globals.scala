package exastencils.globals

import scala.collection.mutable.ListBuffer

import java.io.PrintWriter
import java.io.File

import exastencils.core._
import exastencils.core.collectors._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.prettyprinting._

case class Globals() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  var variables : ListBuffer[VariableDeclarationStatement] = new ListBuffer;

  override def printToFile = {
    val writerHeader = PrettyprintingManager.getPrinter(s"Globals/Globals.h");

    writerHeader << ("#ifndef	GLOBALS_H\n"
      + "#define	GLOBALS_H\n"
      + "#include \"Util/Vector.h\"\n");

    writerHeader << "class Fragment3DCube;\n"; // FIXME: find a way to extract necessary forward defines from variables
    
    for (variable <- variables) { writerHeader << s"extern ${variable.cpp}\n"; }

    writerHeader << "#endif\n";

    writerHeader.close(); // FIXME: finalize

    val writerSource = PrettyprintingManager.getPrinter(s"Globals/Globals.cpp");

    writerSource << "#include \"Globals/Globals.h\"\n\n";
    for (variable <- variables) { writerSource << s"${variable.cpp}\n"; }

    writerSource.close(); // FIXME: finalize
  }
}
