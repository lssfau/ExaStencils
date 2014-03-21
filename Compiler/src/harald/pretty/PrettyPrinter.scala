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

    writer.close()
  }
}
