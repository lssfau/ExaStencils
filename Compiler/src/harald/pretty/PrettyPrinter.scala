package harald.pretty

import java.io._
import harald.ast.TreeL2
import harald.dsl.DomainKnowledge

class PrettyPrinter(treel2 : TreeL2) {

  // DSL level 3
/*
  def prettyscala(fname: String) {

    val extlib: String = scala.io.Source.fromFile("c:/install7/eclipse/scala-SDK-3.0.1-vfinal-2.10-win32.win32.x86_64/eclipse/workspace/Multigrid/src/mylib.scala").getLines.reduceLeft(_ + "\n" + _)

    val writer = new PrintWriter(new File(fname))

    // header	    
    writer.write("import Array._\n")
    writer.write("import scala.math\n")
    writer.write("import java.io._\n")
    writer.write("\n")

    // library classes
    writer.write(extlib)
    writer.write("\n")

    writer.write("object mg { \n")
    writer.write("\n")

    val arrayname: String = "MyArray" //TransformL4.extclasses.get("Array").get.name
    val nlevels: Int = DomainKnowledge.nlevels_L3.getOrElse(1)
    val pnlevels: String = nlevels.toString()

    // Data as global variables
    for (c <- treel2.Fields)
      writer.write(s"var ${c.name}:Array[${arrayname}] = new Array[${arrayname}](${pnlevels})\n")

    writer.write("\n")
    for (c <- treel2.Stencils)
      if (c.weakform.equals(""))
        writer.write(s"var ${c.name}:${treel2.extclasses.get("Stencil").get.name} = new ${treel2.extclasses.get("Stencil").get.name}(${c.entries.length})\n")
      else
        writer.write(s"var ${c.name}:${treel2.extclasses.get("StencilVar").get.name} = new ${treel2.extclasses.get("StencilVar").get.name}(${c.entries.length})\n")

    writer.write("\n")

    // main
    writer.write("\n")
    writer.write("def main(args: Array[String]): Unit = {\n")

    writer.write("\n")
    val setfuncarrname: String = "set" //TransformL4.extclasses.get("Array").get.memberfunc("set").name 
    val setrandfuncname: String = "setrandom" // TransformL4.extfunctions.get("setRandom").get.name 
    for (c <- treel2.Fields) {

      var sx: Int = c.sizex;
      var sy: Int = c.sizey;
      for (i <- 0 to (nlevels - 1)) {
        writer.write(s"${c.name}(${i}) = new ${arrayname}(${sx}+${c.addpoints},${sy}+${c.addpoints} )\n")
        sx = sx / 2;
        sy = sy / 2;
      }
      writer.write(s"${c.name}(0).${setfuncarrname}(0)\n")
      writer.write("\n")
    }
    // in transfrom init must be chosen
    // writer.write(s"${setrandfuncname}()")

    writer.write("\n")
    for (c <- treel2.Stencils) {
      writer.write(s"${c.name}.set(Array[${c.datatype}](${c.entries(0)}")
      for (i <- 1 to c.entries.length - 1)
        writer.write(s",${c.entries(i)}")
      writer.write("))\n")
    }

    writer.write("\n")

    writer.write("var res_0:Double = L2_Residual ( 0 )\n")
    writer.write("println(\"Starting residual: \" + res_0 )\n")
    writer.write("var res:Double = res_0\n")
    writer.write("var res_old:Double = 0\n")
    writer.write("\n")

    // TODO: get name somewhere?
    val itersp = if (DomainKnowledge.iters_L3.isDefined) DomainKnowledge.iters_L3.get else "iters"
    writer.write(s"for ( i <- 0 to (${itersp}-1)) {\n")
    writer.write("res_old = res\n")
    writer.write("VCycle ( 0 )\n")
    writer.write("res = L2_Residual ( 0 )\n")
    writer.write("println(\"Cycle: \" + i + \" Residual: \" + res + \" residual reduction: \" + res_0/res + \" convergence factor: \" + res/res_old)\n")
    writer.write("}\n")

    writer.write("}\n")
    writer.write("}\n")
    writer.close()
  }
*/
  def prettycpp(path : String, fname: String) {

    //        val extlib:String = scala.io.Source.fromFile("c:/install7/eclipse/scala-SDK-3.0.1-vfinal-2.10-win32.win32.x86_64/eclipse/workspace/Multigrid/src/mylibcpp.scala").getLines.reduceLeft(_+"\n"+_)
    val extlib: String = scala.io.Source.fromFile(path+"mglib.cpp").getLines.reduceLeft(_ + "\n" + _)

    var extlibcuda: String = ""
    var extlibmpi: String = ""

    if (DomainKnowledge.use_MPI)
     extlibmpi = scala.io.Source.fromFile(path+"mpilib.cpp").getLines.reduceLeft(_ + "\n" + _)

    if (DomainKnowledge.use_gpu)
      extlibcuda = scala.io.Source.fromFile(path+"mglib.cu").getLines.reduceLeft(_ + "\n" + _)

    val writer = new PrintWriter(new File(fname))

    // header	    
    writer.write("#include <iostream>\n")
    writer.write("#include <vector>\n")
    writer.write("#include <string>\n")
    writer.write("#include <fstream>\n")
    writer.write("#include <complex>\n")
    writer.write("#include <stdlib.h>\n")
    if (DomainKnowledge.use_Windows)
      writer.write("#include <Windows.h>\n")
    else { 
      writer.write("#include <sys/time.h>\n")
      writer.write("#include <string.h>\n")
    }
    
    if (DomainKnowledge.use_MPI)
      writer.write("#include \"mpi.h\"\n")
    if (DomainKnowledge.use_Openmp)
      writer.write("#include \"omp.h\"\n")

    if (DomainKnowledge.use_FE)
      writer.write("#include \"Jochen/Colsamm.h\"\n")

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
    var h: String = "cpp"
    //        for (e <- TransformL4.extclasses)
    //           writer.write(e._2.toString)
    writer.write("\n")
    writer.write(treel2.ExternalClasses.get("Array").get.toString_cpp)
    writer.write("\n")
    if (DomainKnowledge.use_gpu)
      writer.write(treel2.ExternalClasses.get("ArrayCuda").get.toString_cpp)
    writer.write("\n")
//    if (DomainKnowledge.use_FE)
      writer.write(treel2.ExternalClasses.get("Matrix").get.toString_cpp)
    
     var extlibdiscr: String = ""

     if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion")) {
       extlibdiscr = scala.io.Source.fromFile(path+"discrlib.cpp").getLines.reduceLeft(_ + "\n" + _)
       writer.write(extlibdiscr)
       writer.write("\n") 
     }

    val arrayname: String = treel2.ExternalClasses.get("Array").get.name
    val stencilname: String = treel2.ExternalClasses.get("Stencil").get.name
    val nlevels: Int = DomainKnowledge.nlevels_L2.getOrElse(1)
    val pnlevels: String = nlevels.toString()

    writer.write("\n")
    // Data as global variables
    for (c <- treel2.Fields) {
       var compstr = ""
        for (i <- 0 to c.veclength-1) {
       if (c.veclength > 1)
         compstr = s"${i}"
        writer.write(s"${c.arrname}<${c.datatype}>* ${c.name}${compstr};\n")
        }
    }
 //   if (DomainKnowledge.use_MPI)  
     for (c <- treel2.GhostFields)
      writer.write(s"${arrayname}<${c.datatype}>* ${c.name};\n")
    writer.write("\n")

    writer.write(treel2.ExternalClasses.get("Stencil").get.toString_cpp)
    writer.write("\n")
    writer.write(treel2.ExternalClasses.get("StencilVar").get.toString_cpp)
    writer.write("\n")
    
    writer.write("\n")
    for (c <- treel2.Stencils) {
      var compstr = ""
      var cnt = 0
      for (i1 <- 0 to c.matlength1-1)
       for (i2 <- 0 to c.matlength1-1) {
        if (c.matlength1*c.matlength2 != 1) 
          compstr = s"${i1}_${i2}"
          
        if (c.weakform(cnt).equals(""))
          writer.write(s"${stencilname}<${c.datatype(cnt)}>* ${c.name}${compstr};\n")
        else
          writer.write(s"${treel2.ExternalClasses.get("StencilVar").get.name}<${c.datatype(cnt)}>* ${c.name}${compstr};\n")        
        cnt+=1  
       }
      } 
    
    writer.write("\n")

    for (g <- DomainKnowledge.global_variables) {
      if (g.value == 0)
        writer.write(s"${g.dtype} ${g.name}; \n")
      else
        if (DomainKnowledge.use_gpu) {
          if (g.name.contains("["))
            writer.write(s"${g.modifier} ${g.dtype} ${g.name} = ${g.valstring}; \n")
          else if (g.dtype.equals("int")) 
            writer.write(s"#define ${g.name} ${g.value.toInt} \n")
          else
            writer.write(s"#define ${g.name} ${g.value} \n")
         } else {
          if (g.valstring == "")
            writer.write(s"${g.modifier} ${g.dtype} ${g.name} = ${g.value}; \n")
          else  
            writer.write(s"${g.modifier} ${g.dtype} ${g.name} = ${g.valstring}; \n")
        }
      if (g.name.equals("xsize"))
        g.v = (g.v.toFloat + 5).toInt
    }
    
    if (DomainKnowledge.debugmode) {
      println(DomainKnowledge.global_variables)
      println(DomainKnowledge.global_fields.toString)
      println(DomainKnowledge.global_stencils.toString)
    }
    
    writer.write("\n")
    if (!DomainKnowledge.use_Windows)
     writer.write(treel2.ExternalFunctions.get("usec").get.toString_cpp)
    writer.write("\n")     
    if (!DomainKnowledge.use_gpu) 
     writer.write(treel2.ExternalFunctions.get("BC").get.toString_cpp)
    writer.write("\n")
     if (DomainKnowledge.use_MPI) {
     writer.write(treel2.ExternalFunctions.get("copyToBuffers").get.toString_cpp)
    writer.write("\n")
     writer.write(treel2.ExternalFunctions.get("copyFromBuffers").get.toString_cpp)
    writer.write("\n")
     }
    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion")) {
    writer.write("\n")
    if (DomainKnowledge.use_gpu)
     writer.write(treel2.ExternalFunctions.get("computeStencil").get.toString_cuda)
    else 
     writer.write(treel2.ExternalFunctions.get("computeStencil").get.toString_cpp)
    writer.write("\n")      
    }
    
    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion") && DomainKnowledge.use_gpu) {
    writer.write("\n")
     writer.write(treel2.ExternalFunctions.get("multcomplex1").get.toString_cuda)
     writer.write(treel2.ExternalFunctions.get("multcomplex2").get.toString_cuda)
     writer.write(treel2.ExternalFunctions.get("multcomplex3").get.toString_cuda)
     writer.write(treel2.ExternalFunctions.get("multcomplex4").get.toString_cuda)
     writer.write(treel2.ExternalFunctions.get("divcomplex").get.toString_cuda)
     writer.write(treel2.ExternalFunctions.get("inversecomplex1").get.toString_cuda)
     writer.write(treel2.ExternalFunctions.get("inversecomplex2").get.toString_cuda)
    writer.write("\n")             
     }
    
    for (c <- treel2.Functions) {
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
    }

    writer.write("\n")
    writer.write(treel2.ExternalFunctions.get("Main").get.toString_cpp)

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


