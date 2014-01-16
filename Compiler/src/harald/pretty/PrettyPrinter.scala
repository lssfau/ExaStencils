package harald.pretty

import java.io._
import harald.ast.TreeL2
import harald.dsl.DomainKnowledge
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

    val writer = PrettyprintingManager.getPrinter(fname)

    // header	    
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
    var h : String = "cpp"

    // FIXME: this should be done automatically
    for (extClass <- treel2.ExternalClasses)
      extClass._2.printToFile;

    val arrayname : String = treel2.ExternalClasses.get("Array").get.name
    val stencilname : String = treel2.ExternalClasses.get("Stencil").get.name
    val nlevels : Int = DomainKnowledge.nlevels_L3.getOrElse(1)
    val pnlevels : String = nlevels.toString()

    // publish global field variables
    for (c <- treel2.Fields)
      globals.variables += new VariableDeclarationStatement(new Variable(s"${c.arrname}<${c.datatype}>*", s"${c.name}"))
    if (DomainKnowledge.use_MPI)
      for (c <- treel2.GhostFields)
        globals.variables += new VariableDeclarationStatement(new Variable(s"${arrayname}<${c.datatype}>*", s"${c.name}"))

    // publish global stencils
    for (c <- treel2.Stencils)
      if (c.weakform.equals(""))
        globals.variables += new VariableDeclarationStatement(new Variable(s"${stencilname}<${c.datatype}>*", s"${c.name}"))
      else
        globals.variables += new VariableDeclarationStatement(new Variable(s"${treel2.ExternalClasses.get("StencilVar").get.name}<${c.datatype}>*", s"${c.name}"))

    writer.write("\n")

    // FIXME: this was a quick way to remove globals from the main while letting them still be available in the modules  
    for (g <- DomainKnowledge.global_variables) {
      globals.defines += new DefineStatement(s"${g.name}", Some(s"${g.value}"))

      // TODO: find out what the purpose of the next two seemingly irrelevant statements was
      //if (g.name.equals("xsize"))
      //  g.v = (g.v.toFloat + 5).toInt
    }

    println(DomainKnowledge.global_variables)
    println(DomainKnowledge.global_fields.toString)
    println(DomainKnowledge.global_stencils.toString)

    var i = 0;
    val writerHeader = PrettyprintingManager.getPrinter(s"Functions.h");
    writerHeader << ("#ifndef	FUNCTIONS_H\n"
      + "#define	FUNCTIONS_H\n")
    for (extClass <- treel2.ExternalClasses)
      writerHeader <<< "#include \"" + extClass._2.cname + ".h\""
    for (func <- treel2.Functions) {
      writerHeader <<< func._2.toString_cpp_signature;

      val writerSource = PrettyprintingManager.getPrinter(s"Functions_$i.cpp");
      writerSource <<< "#include \"Functions.h\"";

      writerSource <<< func._2.toString_cpp;

      i += 1;
    }
    writerHeader <<< "#endif"

    writer.write("\n")
    if (!DomainKnowledge.use_gpu)
      writer.write(treel2.extfunctions.get("BC").get.toString_cpp)
    writer.write("\n")
    if (DomainKnowledge.use_MPI) {
      writer.write(treel2.extfunctions.get("copyToBuffers").get.toString_cpp)
      writer.write("\n")
      writer.write(treel2.extfunctions.get("copyFromBuffers").get.toString_cpp)
      writer.write("\n")
    }

    /* for (c <- treel2.Functions) {
      if (!c._2.name.equals("Application")) {
        writer.write(c._2.toString_cpp_signature)
        writer.write("\n")
      }
    }

    for (c <- treel2.Functions) {
      if (c._1.contains("cuda")) {
        writer.write(c._2.toString_cpp)
        writer.write("\n")
      }
    }

    for (c <- treel2.Functions) {
      if (!c._1.equals("VCycle") && !c._1.contains("cuda") && !c._2.name.equals("Application")) {
        writer.write(c._2.toString_cpp)
        writer.write("\n")
      }
    }

    for (c <- treel2.Functions) {
      if (c._1.equals("VCycle")) {
        writer.write(c._2.toString_cpp)
        writer.write("\n")
      }
    }*/

    writer.write("\n")
    writer.write(treel2.extfunctions.get("Main").get.toString_cpp)

    writer.close()
  }
  /*
  def prettycuda(fname: String) {

    //        val extlib:String = scala.io.Source.fromFile("c:/install7/eclipse/scala-SDK-3.0.1-vfinal-2.10-win32.win32.x86_64/eclipse/workspace/Multigrid/src/mylibcpp.scala").getLines.reduceLeft(_+"\n"+_)
    val extlib: String = scala.io.Source.fromFile("c:/Harald/workspace/ExaGrid/src/mglib.cpp").getLines.reduceLeft(_ + "\n" + _)
    val writer = new PrintWriter(new File(fname))

    // header	    
    writer.write("#include <iostream>\n")
    writer.write("#include <vector>\n")
    writer.write("#include <complex>\n")
    writer.write("\n")

    // library classes
    writer.write(extlib)
    writer.write("\n")

    writer.write(treel2.extclasses.get("Array").get.toString_cpp)
    writer.write("\n")
    writer.write(treel2.extclasses.get("ArrayCuda").get.toString_cpp)
    writer.write("\n")
    writer.write(treel2.extclasses.get("Stencil").get.toString_cpp)
    writer.write("\n")
    writer.write(treel2.extclasses.get("StencilVar").get.toString_cpp)
    writer.write("\n")

    val arrayname: String = treel2.extclasses.get("Array").get.name
    val stencilname: String = treel2.extclasses.get("Stencil").get.name
    val nlevels: Int = DomainKnowledge.nlevels_L3.getOrElse(1)
    val pnlevels: String = nlevels.toString()

    // Data as global variables
    for (c <- treel2.Fields)
      writer.write(s"${arrayname}<${c.datatype}>* ${c.name};\n")

    writer.write("\n")
    for (c <- treel2.Stencils)
      writer.write(s"${stencilname}<${c.datatype}>* ${c.name};\n")

    writer.write("\n")

    for (g <- DomainKnowledge.global_variables) {
      writer.write(s"${g.dtype} ${g.name} = ${g.value}; \n")
      if (g.name.equals("xsize"))
        g.v = (g.v.toFloat + 5).toInt
    }
    println(DomainKnowledge.global_variables)

    for (g <- DomainKnowledge.global_fields) {
      if (g.name.equals("f[0]"))
        g.sx += 5
    }

    println(DomainKnowledge.global_fields.toString)
    println(DomainKnowledge.global_stencils.toString)

    writer.write("\n")
 

    // main
    writer.write("\n")
    writer.write(treel2.extfunctions.get("Main").get.toString_cpp)

    writer.close()
  }
*/
}


