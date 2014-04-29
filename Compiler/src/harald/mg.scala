import Array._
import scala.math
import java.io._
import harald.dsl._
import harald.Continuous._
import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import java.lang.reflect.ParameterizedType
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import harald.Parser
import harald.Parser._
import harald.Generate._
import harald.ast._
import harald.expert._
import harald.pretty._
import scala.sys.process._
import harald.Discrete._

object mg {

  def main(args: Array[String]): Unit = {

    val basicpath = "e:/SVN/flow/ExaScala/"
      
    // DOC: set paths

    //  val mainDSLpath = "e:/Harald/workspace/ExaGrid/src/testDSL/"
    /*
     val DSLpath = args(0)
     val problem = args(1)
     val libpath = args(2)
     val outputpath = args(3)
     val outputfile = args(4)
       */
    val libpath = basicpath + "otherfiles/"
    //   val libpath = "C:/SVN/flow/ExaScala/otherfiles/"
    //    val libpath = "c:/Harald/workspace/ExaGrid/src/"
    //   //    val libpath = "e:/Harald/workspace/ExaGrid/src/"
    val DSLpath = basicpath + "testDSL/"
    //   val DSLpath = "C:/SVN/flow/ExaScala/testDSL/"
    //  val DSLpath = "c:/Harald/workspace/ExaGrid/src/testDSL/"
    //     val DSLpath = "e:/Harald/workspace/ExaGrid/src/testDSL/"
    //    val problem = "testDSL"
    //     val problem = "testDSL3D"
    //     val problem = "testDSLgpu"
    //        val outputpath = "e:/Harald/Studio/testmgcuda/testcuda/testcuda/"
    //    val outputpath = "c:/install7/eclipse/scala-SDK-3.0.1-vfinal-2.10-win32.win32.x86_64/eclipse/workspace/Studio/testmg/testmg/"
    //    val outputpath = "C:/SVN/flow/ExaScala/Studio/testmg/testmg/"
    // val outputpath = "C:/SVN/flow/ExaScala/Studio/CPU/testCPU/testCPU/"

    //    val outputfile = "main.cpp"
    //       bandwidth_HW = 59
    // peak_HW = 118
    //     val outputpath = "e:/Harald/Studio/testmgcuda/testcuda/testcuda/"
    //       val outputfile = "kernel.cu"
    /*   val application = scala.io.Source.fromFile(mainDSLpath + "Applications.mg").getLines
    for (appli <- application) {
     val app = appli.split(" ").toArray 
     for (a <- app) {  // set global application parameters
      println(a)   
     }
     val DSLpath = app(0)
     val problem = app(1)
     val outputpath = app(2)
     val outputfile = app(3)
   */
    //      val DSLpath = "c:/install7/eclipse/scala-SDK-3.0.1-vfinal-2.10-win32.win32.x86_64/eclipse/workspace/Multigrid/src/testDSL/"
    //    val DSLpath = "e:/Harald/workspace/ExaGrid/src/testDSL/"

    var problem = ""
    var outputfile = ""

    var domainstr: ListBuffer[String] = ListBuffer("UnitSquare", "UnitCube")
    var problemstr: ListBuffer[String] = ListBuffer("Laplacian", "ComplexDiffusion", "Stokes", "OpticalFlow")
    var bcstr: ListBuffer[String] = ListBuffer("zero", "dn", "zero", "zero")
  //  var sizeval : ListBuffer[Int] = ListBuffer(9,8)
    var sizeval: ListBuffer[Int] = ListBuffer(4, 8)
    //     var sizeval : ListBuffer[Int] = ListBuffer(4,4)
    var threadsval: ListBuffer[Int] = ListBuffer(1, 8)

    var smootherstr: ListBuffer[String] = ListBuffer("Jacobi", "GaussSeidel")
    var omegaval: ListBuffer[Double] = ListBuffer(0.8, 1.0)
    var smoothval: ListBuffer[Int] = ListBuffer(1, 2)

    var cyclestr: ListBuffer[String] = ListBuffer("VCycle", "FMGVCycle", "FASVCycle", "FMGFASVCycle")
    var hwstr: ListBuffer[String] = ListBuffer("cpu", "gpu")
    var dtstr: ListBuffer[String] = ListBuffer("Double", "Float", "ComplexDouble", "ComplexFloat")
    var locstr: ListBuffer[String] = ListBuffer("nodes", "cells", "nodes", "nodes")
    var disstr: ListBuffer[String] = ListBuffer("FD", "FE")

    var ntry = 0

    // TODO: complex dt
    val writer = new FileWriter(new File(DomainKnowledge.outputpath + "compile.bat"), false)
    writer.close

    val writer2 = new FileWriter(new File(DomainKnowledge.outputpath + "execute.bat"), false)
    writer2.close
    DomainKnowledge.use_Windows = true

//            DomainKnowledge.outputpath = "e:/Harald/Studio/testmg/testmg/"
           DomainKnowledge.outputpath = "e:/Harald/Studio/testmgcuda/testcuda/testcuda/"      
  //  DomainKnowledge.outputpath = "////i10home/home-staff/koestler/Code/"
    //      DomainKnowledge.outputpath =  "E:/cygwin/home/harald/Code/"
//    DomainKnowledge.outputpath = "C:/SVN/flow/ExaScala/Studio/testmg/testmg/"
    //      DomainKnowledge.outputpath =  "c:/cygwin64/home/koestler/Code/"
    //       DomainKnowledge.outputpath = "C:/SVN/flow/ExaScala/Studio/CPU/testCPU/testCPU/"

    val h = 1 //4096
    val x : Array[Double] = Array(-2.0/h,-1.0/h, 0,1.0/h,2.0/h)
//    val x : Array[Double] = Array(-1.0/h, 0,1.0/h)
    val erg = DomainKnowledge.fornberg(0, x, 4)
    /*
    println(erg)
     for (i <- 0 to erg.length-1) {
         for ( j <- 0 to erg(i).length-1) {
            print(" " + erg(i)(j));
         }
         println();
      }
    */
    val no = 3
    val x2 : Array[Double]= Array.ofDim(erg(no).length-1)
    val xh : Array[Double]= Array.ofDim(erg(no).length-1)
    for (i <- 1 to erg(no).length-1)
      x2(i-1) = erg(no)(i)
    xh((erg(no).length-1)/2) = 1
    
    val xerg : Array[Array[Array[Double]]]= Array.ofDim[Double](x2.length+1,x2.length+2,x2.length)
    
    println("next" + x2.length + " " + xerg(0).size);
//    exit
    
    // tensor product for stencils
    for (i <- 0 to x2.length-1) {
       for ( j <- 0 to x2.length-1) {
       for ( k <- 0 to x2.length-1) {

          xerg(i)(j)(k) += x2(k) * xh(j) * xh(i)
          //print(" " + xerg(i)(j)(k));
         
       }
//    println()
    }
 //   println()
 //   println()
    }
    
   DomainKnowledge.debugmode = true;

    /*
    exit
    
    val x3 : Array[Array[Double]] = Array.ofDim[Double](5,5)
    
    val erg2 = DomainKnowledge.tensor_prod(erg,erg)
     for (i <- 0 to erg2.length-1) {
         for ( j <- 0 to erg2(i).length-1) {
            print(" " + erg2(i)(j));
         }
         println();
      }
    
    exit
      */
    for (hwtry <- 0 to 0)
      for (prtry <- 0 to 0)
        for (dmtry <- 0 to 0)
          for (thtry <- 0 to 0)
            for (ditry <- 0 to 0)
              for (smtry <- 0 to 0)
                for (cytry <- 0 to 0)
                  for (dttry <- 0 to 0) {

                    var cytry2 = cytry
                    var dttry2 = dttry
                    var thtry2 = thtry
                    var levsub = 1
                    if (prtry == 1) {
                      DomainKnowledge.stenciltype = "nonlinear"
                      cytry2 += 2
                      if (hwtry == 0)
                        dttry2 += 2
                    } else
                      DomainKnowledge.stenciltype = "constant"

                    if (threadsval(thtry2) > 1)
                      DomainKnowledge.use_Openmp = true

                    if (hwtry == 1) {
                      DomainKnowledge.CUDA_BLOCKSIZE = ListBuffer(16, 16)
                      DomainKnowledge.use_Openmp = false
                      DomainKnowledge.use_gpu = true
                      levsub = 2
                      thtry2 = 0
                    } else
                      DomainKnowledge.use_gpu = false

                    ntry += 1

                    var optHW = OptionsHW(hwstr(hwtry), threadsval(thtry2), 1)
                    var optL1 = OptionsL1(domainstr(dmtry), problemstr(prtry), bcstr(prtry), sizeval(dmtry), 0) //important set generate to 0!
                    var optL2 = OptionsL2(dtstr(dttry2), disstr(ditry), locstr(prtry), sizeval(dmtry) - levsub)
                    var optL3 = OptionsL3(smootherstr(smtry), omegaval(smtry), smoothval(smtry), smoothval(smtry), cyclestr(cytry2), smootherstr(smtry), 8, 5, 8)
                    DomainKnowledge.optionslist = ListBuffer(ntry.toString, optHW.platform, optHW.cores.toString, optHW.nodes.toString,
                      optL1.domain, optL1.operator, optL1.bc, optL1.generate.toString, optL1.accuracy.toString,
                      optL2.datatype, optL2.discretization, optL2.location,
                      optL3.smoother, optL3.omega.toString, optL3.nprae.toString, optL3.npost.toString, optL3.cycletype, optL3.coarsesolver, optL3.iterscoarse.toString, optL3.iters.toString, optL3.accuracy.toString, optL2.nlevels.toString)

                    var probname = "myDSL" + ntry
                    //     var optL3 = OptionsL3(smoother : String, omega : Double, nprae:Int, npost:Int, cycletype : String, coarsesolver:String, iterscoarse:Int, iters : Int, accuracy:Int, nlevels:Int)
                    val useoptimizer = true

                    if (useoptimizer) {
                      //   GenerateL1.generateall(DSLpath, "genDSL")
                      problem = probname
                      GenerateHW.transformhardwaretoHW(DSLpath + problem + "levHW.mg", optHW)
                      GenerateL1.transformproblemtoL1(DSLpath + problem + "lev1.mg", optL1)
                    }

                    if (!useoptimizer)
                      problem = probname // "testDSL"
                    //       outputfile = "main.cpp"        
                    if (hwtry == 0)
                      outputfile = s"main${ntry}.cpp"
                    else
                      outputfile = s"main${ntry}.cu"
                    outputfile = "kernel.cu"

                    DomainKnowledge.clearData
                    TreeManager.tree = new TreeL2

                    if (DomainKnowledge.debugmode)
                      println("read HW")

                    if (!new java.io.File(DSLpath + problem + "levHW.mg").exists) {
                      println("HW specification is missing!")
                      exit(0)
                    }

                    val DSLHW: String = scala.io.Source.fromFile(DSLpath + problem + "levHW.mg").getLines.reduceLeft(_ + '\n' + _)
                    if (DomainKnowledge.debugmode)
                      println(DSLHW)

                    val parserHW = new ParserHW
                    parserHW.parseAll(parserHW.exastencilsHW, DSLHW)

                    HardwareKnowledge.initHWFeatures

                    if (!new java.io.File(DSLpath + problem + "lev1.mg").exists) {
                      println("Problem specification (DSL level 1) is missing!")
                      exit(0)
                    }

                    if (DomainKnowledge.debugmode)
                      println("read PDE")
                    val DSLl1: String = scala.io.Source.fromFile(DSLpath + problem + "lev1.mg").getLines.reduceLeft(_ + '\n' + _)
                    if (DomainKnowledge.debugmode)
                      println(DSLl1)

                    //val parserl1 = new ParserL1
                    //parserl1.parseAll(parserl1.exastencilsL1, DSLl1)
                    //ContDescription.setup

//                    val DSLl1a: String = scala.io.Source.fromFile(DSLpath + problem + "lev1.mg").getLines.reduceLeft(_ + '\n' + _)
                   // if (DomainKnowledge.debugmode)
                   val parserl1a = new Parser.ParserL1a()
                   parserl1a.parse(DSLl1)
                   DomainKnowledge.setglobalobjects_L1

                   DomainKnowledge.initfragments

          for (i <- DomainKnowledge.cont_functions)
    println(s"${i}")
    for (i <- DomainKnowledge.cont_operators)
    println(s"${i}")
    for (i <- DomainKnowledge.cont_equations)
      println(s"${i}")
    for (i <- DomainKnowledge.cont_consts)
      println(s"${i}")
    for (i <- DomainKnowledge.cont_domains)
      println(s"${i}")
      
                    if (useoptimizer)
                      GenerateL2.transformL1toL2opt(DSLpath + problem + "lev2.mg", optL2)

                    if (!new java.io.File(DSLpath + problem + "lev2.mg").exists || DomainKnowledge.generate_L1.getOrElse(1) == 1)
                      GenerateL2.transformL1toL2(DSLpath + problem + "lev2.mg")
            
                    if (DomainKnowledge.debugmode)
                      println("read discretization")
                    val DSLl2: String = scala.io.Source.fromFile(DSLpath + problem + "lev2.mg").getLines.reduceLeft(_ + _)
                    if (DomainKnowledge.debugmode)
                      println(DSLl2)

                    val parserl2 = new ParserL2(TreeManager.tree)
                    parserl2.parseAll(parserl2.exastencilsL2, DSLl2)
                    //TreeManager.tree.exaFields.foreach(println)
                    //TreeManager.tree.exaOperators.foreach(println)

    for (i <- DomainKnowledge.discr_functions)
    println(s"${i}")
    for (i <- DomainKnowledge.discr_operators)
    println(s"${i}")
    for (i <- DomainKnowledge.discr_consts)
    println(s"${i}")
    for (i <- DomainKnowledge.discr_equations)
    println(s"${i}")

                    DomainKnowledge.initarraysizes
            
                    val genL3 = new GenerateL3(TreeManager.tree)

                    if (useoptimizer) {
                      genL3.transformL2toL3opt(DSLpath + problem + "lev3.mg", optL3)
                      genL3.transformL2toL3aopt(DSLpath + problem + "lev3a.mg", optL3)
                    }

                    if (!new java.io.File(DSLpath + problem + "lev3.mg").exists || DomainKnowledge.generate_L1.getOrElse(1) == 1)
                      genL3.transformL2toL3(DSLpath + problem + "lev3.mg")

                    val DSLl3: String = scala.io.Source.fromFile(DSLpath + problem + "lev3.mg").getLines.reduceLeft(_ + _)
                    if (DomainKnowledge.debugmode)
                      println(DSLl3)

                    val parserl3 = new ParserL3
                    parserl3.parseAll(parserl3.exastencilsL3, DSLl3)
                    
                    val DSLl3a: String = scala.io.Source.fromFile(DSLpath + problem + "lev3a.mg").getLines.reduceLeft(_ + _)
                    if (DomainKnowledge.debugmode)
                      println(DSLl3a)
                    val parserl3a = new ParserL3a(TreeManager.tree)
                    parserl3a.parse(DSLl3a)

    for (i <- DomainKnowledge.discr_functions) {
      println(s"${i}")
      TreeManager.tree.exaFields += i.transform
    }
    for (i <- DomainKnowledge.discr_operators)
    println(s"${i}")
    for (i <- DomainKnowledge.discr_consts)
    println(s"${i}")
    for (i <- DomainKnowledge.discr_equations)
    println(s"${i}")
    for (i <- DomainKnowledge.discr_sets)
    println(s"${i}")
    for (i <- DomainKnowledge.discr_iterations) {
     println(s"${i}")
     println(i.printtoDSL4)
    }
    

                    val genL4 = new GenerateL4(TreeManager.tree)

                    if (useoptimizer)
                      genL4.transformL3toL4(DSLpath + problem + "lev4.mg")

                    if (!(new java.io.File(DSLpath + problem + "lev4.mg").exists) || (DomainKnowledge.generate_L1.getOrElse(1) == 1)) {
                      genL4.transformL3toL4(DSLpath + problem + "lev4.mg")
                      //println("generate L4" + DomainKnowledge.generate_L1.getOrElse(1))
                    }
                    val DSLl4: String = scala.io.Source.fromFile(DSLpath + problem + "lev4.mg").getLines.reduceLeft(_ + _)
                    if (DomainKnowledge.debugmode)
                      println(DSLl4)

                    val parserl4 = new Parser.ParserL4(TreeManager.tree)
                    parserl4.parse(DSLl4)
                    //TransformL4.cppfunctions.foreach(println)

println("finished parsing")

                    TreeManager.tree.transformFields
                    TreeManager.tree.transformStencils

                    val discr = new Discretization(TreeManager.tree)
                    discr.generatediscretization

                    DomainKnowledge.setglobalvariables(DomainKnowledge.optionslist)
                    val tfl2 = new TransformL2(TreeManager.tree)
                    tfl2.setglobalobjects

                    var dataclass = new DataClasses(TreeManager.tree)
                    dataclass.initextClasses

                    TreeManager.tree.transformFunctions

                    val ownfunc = new OwnFunctions(TreeManager.tree)
                    ownfunc.initextFunctions

                    //		ExaDSL.pretty("c:/install7/eclipse/scala-SDK-3.0.1-vfinal-2.10-win32.win32.x86_64/eclipse/workspace/Multigrid/src/test.pretty")
                    //        val cpppath = "c:/install7/eclipse/scala-SDK-3.0.1-vfinal-2.10-win32.win32.x86_64/eclipse/workspace/Studio/testmg/testmg/"

                    //val cufile = "main.cu" 
                    //    ExaDSL.prettycpp(sourcepath + sourcefile)
                    val exadsl = new PrettyPrinter(TreeManager.tree)
                    exadsl.prettycpp(libpath, DomainKnowledge.outputpath + outputfile)

                    val writer = new FileWriter(new File(DomainKnowledge.outputpath + "compile.bat"), true)
                    if (hwtry == 0)
                      writer.write(s"g++ -O3 -fopenmp ${outputfile} -o main${ntry} \n")
                    else
                      writer.write(s"nvcc -O3 ${outputfile} -arch compute_20 -o main${ntry} \n")
                    writer.write("sleep 1 \n")
                    writer.close

                    val writer2 = new FileWriter(new File(DomainKnowledge.outputpath + "execute.bat"), true)
                    if (DomainKnowledge.use_Windows)
                      writer2.write(s"main${ntry}.exe \n")
                    else
                      writer2.write(s"./main${ntry} \n")

                    writer2.write("sleep 2 \n")
                    writer2.close

                    //    set CUDAFE_FLAGS=--sdk_dir "C:\Program Files (x86)\Windows Kits\8.0\"
                    //"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v5.5\bin\nvcc.exe" --use-local-env --cl-version 2012 -ccbin "C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\bin\x86_amd64"        --keep-dir x64\Release -maxrregcount=0  --machine 64 --compile -cudart static    -use_fast_math  -Xcompiler "/EHsc  /nologo /Ox /Zi    " -o x64\Release\%(Filename)%(Extension).obj "%(FullPath)"
                    //    def fileExists(name: String) = Seq("test", "-f", name).! == 0

                    // This uses !! to get the whole result as a string
                    //    val dirContents = "ls".!!

                    //    val str = "\"C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v5.5\\bin\\nvcc.exe\" -ccbin \"C:\\Program Files (x86)\\Microsoft Visual Studio 11.0\\VC\\bin\\x86_amd64\" \"C:\\temp\\main.cu\" -o C:\\temp\\output\\aso.exe"

                    //    val result = Seq("C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\VC\\bin\\cl.exe", outputpath + outputfile).!!
                    //    val result = Seq("C:\\cygwin64\\bin\\g++", outputpath + outputfile).!!
                    //val result = "cmd.exe" #| "dir.exe"  // C:\\cygwin64\\home\\koestler\\start.bat

                  }

    val writer3 = new FileWriter(new File(DomainKnowledge.outputpath + "compile.bat"), true)
    writer3.write("./execute.bat \n")
    writer3.close

    println("finished")

    /*
    //ExaDSL.prettycuda(cpppath + cufile)
    var perfmodel = new PerformanceModel(TreeManager.tree)

    var esttimeGS = perfmodel.estimatePerformance(DomainKnowledge.smoother_L3.get)
    println(s"GaussSeidel: Estimated time: IO = ${esttimeGS._1} ms FLOP = ${esttimeGS._2} ms")
    var esttimeRes = perfmodel.estimatePerformance("Residual")
    println(s"Residual: Estimated time: IO = ${esttimeRes._1} ms FLOP = ${esttimeRes._2} ms")
    var esttimeRestr = perfmodel.estimatePerformance("Restrict")
    println(s"Restrict: Estimated time: IO = ${esttimeRestr._1} ms FLOP = ${esttimeRestr._2} ms")
    var esttimeIntCorr = perfmodel.estimatePerformance("interpolatecorr")
    println(s"InterpolateCorr: Estimated time: IO = ${esttimeIntCorr._1} ms FLOP = ${esttimeIntCorr._2} ms")
    var IOcycle = esttimeGS._1 + esttimeRes._1 + esttimeRestr._1 + esttimeIntCorr._1
    var FLOPcycle = esttimeGS._2 + esttimeRes._2 + esttimeRestr._2 + esttimeIntCorr._2
    println(s"VCycle: Estimated time: IO = ${IOcycle} ms FLOP = ${FLOPcycle} ms")
*/
    
   
//     TODO: code for read source write scala
/*        val DSLHW2 = scala.io.Source.fromFile(DSLpath + "myDSL1lev3a.mg").getLines
        val writer4 = new FileWriter(new File(DSLpath + "test.mg"))
       for (s <- DSLHW2) {
         writer4.write("writer.write(\"" + s"${s}" + "\");\n" )
         println("writer.write(\""+s"${s}"+"\"\n);")
       }
        writer4.close
*/         
        
  }
}