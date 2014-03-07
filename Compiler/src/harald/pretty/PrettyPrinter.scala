package harald.pretty

import java.io._
import scala.collection.mutable.ListBuffer
import harald.ast.TreeL2
import harald.dsl.DomainKnowledge
import harald.Impl._
import exastencils.core._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.globals._
import exastencils.prettyprinting._

class PrettyPrinter(treel2 : TreeL2) {
  def prettycpp(path : String, fname : String) {
    val globals : Globals = StateManager.root.asInstanceOf[Root].nodes.find(node => node.isInstanceOf[Globals]).get.asInstanceOf[Globals];

    val extlib : String = ""; // scala.io.Source.fromFile(path+"mglib.cpp").getLines.reduceLeft(_ + "\n" + _)

    var extlibcuda : String = ""
    var extlibmpi : String = ""

    if (DomainKnowledge.use_MPI)
      extlibmpi = scala.io.Source.fromFile(path + "mpilib.cpp").getLines.reduceLeft(_ + "\n" + _)

    if (DomainKnowledge.use_gpu)
      extlibcuda = scala.io.Source.fromFile(path + "mglib.cu").getLines.reduceLeft(_ + "\n" + _)

    // publish global field variables
    for (c <- treel2.Fields)
      globals.variables += new VariableDeclarationStatement(new VariableAccess(c.name, Some(s"${c.arrname}<${c.datatype}>*")))
    if (DomainKnowledge.use_MPI)
      for (c <- treel2.GhostFields)
        globals.variables += new VariableDeclarationStatement(new VariableAccess(c.name, Some(s"MyArray<${c.datatype}>*")))

    // publish global stencils
    for (c <- treel2.Stencils)
      if (c.weakform.equals(""))
        globals.variables += new VariableDeclarationStatement(new VariableAccess(c.name, Some(s"MyStencil<${c.datatype}>*")))
      else
        globals.variables += new VariableDeclarationStatement(new VariableAccess(c.name, Some(s"${treel2.ExternalClasses.get("StencilVar").get.name}<${c.datatype}>*")))

    println(DomainKnowledge.global_variables)
    println(DomainKnowledge.global_fields.toString)
    println(DomainKnowledge.global_stencils.toString)

    var i = 0;
    val writerHeader = PrettyprintingManager.getPrinter(s"Functions.h");
    writerHeader << ("#ifndef	FUNCTIONS_H\n"
      + "#define	FUNCTIONS_H\n")
    writerHeader <<< "#include <cstdlib>" // required for rand
    for (extClass <- treel2.ExternalClasses)
      writerHeader <<< "#include \"" + extClass._2.cname + ".h\""
    writerHeader <<< "#include \"Primitives/Fragment3DCube.h\""
    writerHeader <<< "#include \"Globals/Globals.h\""
    writerHeader <<< "#include \"Primitives/CommunicationFunctions.h\""
    for (func <- treel2.Functions) {
      writerHeader <<< func._2.toString_cpp_signature;

      val writerSource = PrettyprintingManager.getPrinter(s"Functions_$i.cpp");
      writerSource <<< "#include \"Functions.h\"";

      writerSource <<< func._2.toString_cpp;

      i += 1;
    }
    for (func <- treel2.extfunctions) {
      if ("Main" != func._1) {
        writerHeader <<< func._2.toString_cpp_signature;

        val writerSource = PrettyprintingManager.getPrinter(s"Functions_$i.cpp");
        writerSource <<< "#include \"Functions.h\"";

        writerSource <<< func._2.toString_cpp;

        i += 1;
      }
    }
    writerHeader <<< "#endif"

    val writer = PrettyprintingManager.getPrinter(fname)

    // Write actual main function	    
    writer.write("#include <iostream>\n")
    writer.write("#include <vector>\n")
    writer.write("#include <complex>\n")
    writer.write("#include <stdlib.h>\n")
    if (DomainKnowledge.use_MPI)
      writer.write("#include \"mpi.h\"\n")
    if (DomainKnowledge.use_Openmp)
      writer.write("#include \"omp.h\"\n")

    if (DomainKnowledge.use_FE)
      writer.write("#include \"Jochen/Colsamm.h\"\n")

    writer <<< "#include \"Globals/Globals.h\""
    for (extClass <- treel2.ExternalClasses)
      writer <<< "#include \"" + extClass._2.cname + ".h\""
    writer <<< "#include \"Functions.h\"";

    writer.write("\n")

    // BAD HACK
    writer <<< "#include \"Poisson3D.h\"";
    treel2.extfunctions.get("Main").get.body = ListBuffer[Statement](new StringLiteral("main2(argc, argv);\n")) ++ treel2.extfunctions.get("Main").get.body;

    // library classes
    writer.write(extlib)
    writer.write("\n")
    if (DomainKnowledge.use_gpu) {
      writer.write(extlibcuda)
      writer.write("\n")
    }
    if (DomainKnowledge.use_MPI) {
      writer.write(extlibmpi)
      writer.write("\n")
    }

    writer.write(treel2.extfunctions.get("Main").get.toString_cpp)

    writer.close()
  }
}
