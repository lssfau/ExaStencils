package harald

import Array._
import scala.math
import java.io._
import dsl._
import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import java.lang.reflect.ParameterizedType
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import Parser._
import Generate._
import ast.TreeManager
import expert._
import pretty.PrettyPrinter

object mg {

  def main(args: Array[String]): Unit = {

    //  val mainDSLpath = "e:/Harald/workspace/ExaGrid/src/testDSL/"
    /*
     val DSLpath = args(0)
     val problem = args(1)
     val libpath = args(2)
     val outputpath = args(3)
     val outputfile = args(4)
       */
    val libpath = "e:/SVN/flow/ExaScala/otherfiles/"
//    val libpath = "C:/SVN/flow/ExaScala/otherfiles/"
//    val libpath = "c:/Harald/workspace/ExaGrid/src/"
    //    val libpath = "e:/Harald/workspace/ExaGrid/src/"
    val DSLpath = "e:/SVN/flow/ExaScala/testDSL/"
//    val DSLpath = "C:/SVN/flow/ExaScala/testDSL/"
  //  val DSLpath = "c:/Harald/workspace/ExaGrid/src/testDSL/"
    //     val DSLpath = "e:/Harald/workspace/ExaGrid/src/testDSL/"
    val problem = "testDSL"
    //     val problem = "testDSL3D"
    //     val problem = "testDSLgpu"
        val outputpath = "e:/Harald/Studio/testmg/testmg/"
//    val outputpath = "c:/install7/eclipse/scala-SDK-3.0.1-vfinal-2.10-win32.win32.x86_64/eclipse/workspace/Studio/testmg/testmg/"
//    val outputpath = "C:/SVN/flow/ExaScala/Studio/testmg/testmg/"

   // val outputfile = "main.cpp"
    //     val outputpath = "e:/Harald/Studio/testmgcuda/testcuda/testcuda/"
         val outputfile = "kernel.cu"

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
    println("read HW")

    if (!new java.io.File(DSLpath + problem + "levHW.mg").exists) {
      println("HW specification is missing!")
      exit(0)
    }

    val DSLHW: String = scala.io.Source.fromFile(DSLpath + problem + "levHW.mg").getLines.reduceLeft(_ + '\n' + _)
    println(DSLHW)

    val parserHW = new ParserHW
    parserHW.parseAll(parserHW.exastencilsHW, DSLHW)

    Hardware.initHWFeatures

    if (!new java.io.File(DSLpath + problem + "lev1.mg").exists) {
      println("Problem specification (DSL level 1) is missing!")
      exit(0)
    }

    println("read PDE")
    val DSLl1: String = scala.io.Source.fromFile(DSLpath + problem + "lev1.mg").getLines.reduceLeft(_ + '\n' + _)
    println(DSLl1)

    val parserl1 = new ParserL1
    parserl1.parseAll(parserl1.exastencilsL1, DSLl1)

    if (!new java.io.File(DSLpath + problem + "lev2.mg").exists || DomainKnowledge.generate_L1.getOrElse(1) == 1)
      GenerateL2.transformL1toL2(DSLpath + problem + "lev2.mg")

    println("read discretization")
    val DSLl2: String = scala.io.Source.fromFile(DSLpath + problem + "lev2.mg").getLines.reduceLeft(_ + _)
    println(DSLl2)

    val parserl2 = new ParserL2(TreeManager.tree)
    parserl2.parseAll(parserl2.exastencilsL2, DSLl2)
    TreeManager.tree.exaFields.foreach(println)
    TreeManager.tree.exaOperators.foreach(println)
    DomainKnowledge.initfragments

    val genL3 = new GenerateL3(TreeManager.tree)
    if (!new java.io.File(DSLpath + problem + "lev3.mg").exists || DomainKnowledge.generate_L1.getOrElse(1) == 1)
      genL3.transformL2toL3(DSLpath + problem + "lev3.mg")

    val DSLl3: String = scala.io.Source.fromFile(DSLpath + problem + "lev3.mg").getLines.reduceLeft(_ + _)
    println(DSLl3)

    val parserl3 = new ParserL3
    parserl3.parseAll(parserl3.exastencilsL3, DSLl3)
    DomainKnowledge.initarraysizes

    val genL4 = new GenerateL4(TreeManager.tree)
    if (!(new java.io.File(DSLpath + problem + "lev4.mg").exists) || (DomainKnowledge.generate_L1.getOrElse(1) == 1)) {
      genL4.transformL3toL4(DSLpath + problem + "lev4.mg")
      println("generate L4" + DomainKnowledge.generate_L1.getOrElse(1))
    }
    val DSLl4: String = scala.io.Source.fromFile(DSLpath + problem + "lev4.mg").getLines.reduceLeft(_ + _)
    println(DSLl4)

    val parserl4 = new Parser.ParserL4(TreeManager.tree)
    parserl4.parse(DSLl4)
    //TransformL4.cppfunctions.foreach(println)

    TreeManager.tree.transformFields
    TreeManager.tree.transformStencils

    val discr = new Discretization(TreeManager.tree)
    discr.generatediscretization

    DomainKnowledge.setglobalvariables
    val tfl2 = new TransformL2(TreeManager.tree)
    tfl2.setglobalobjects

    
    var dataclass = new DataClasses(TreeManager.tree)
    dataclass.initextClasses
    
    TreeManager.tree.transformFunctions
    
    //val ownfunc = new OwnFunctions(TreeManager.tree)
    //ownfunc.initextFunctions


    //		ExaDSL.pretty("c:/install7/eclipse/scala-SDK-3.0.1-vfinal-2.10-win32.win32.x86_64/eclipse/workspace/Multigrid/src/test.pretty")
    //        val cpppath = "c:/install7/eclipse/scala-SDK-3.0.1-vfinal-2.10-win32.win32.x86_64/eclipse/workspace/Studio/testmg/testmg/"

    //val cufile = "main.cu" 
    //    ExaDSL.prettycpp(sourcepath + sourcefile)
    val exadsl = new PrettyPrinter(TreeManager.tree)
    exadsl.prettycpp(libpath, outputpath + outputfile)

    //ExaDSL.prettycuda(cpppath + cufile)
    var perfmodel = new PerformanceModel(TreeManager.tree)

    var esttimeGS = perfmodel.estimatePerformance("GaussSeidel")
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

    // }

    // TODO:
    // field, stencil immer gleich initialisieren (schon in DSL erzwingen)
    // die parameter in die verschiedenen levels/teile aufteilen: Multigrid/Domain/PDE
    // syntax highlighting, debugging des DSL codes?
    // Grid aufteilen inner + boundary, BC?
    // MG parameter + components

    // Baustellen:
    // FE -> Colsamm
    // var. coefficients
    // systems
    // cuda
    // openmp
    // MPI
    // einheitl. parser
    // perf. model
    // lfa generieren
    // BC
    // fragments
    // generate L4?
    // parse stencil?

    //  print ( "Residual: " , res , " residual reduction: " , res_0/res , "convergence factor:", res/res_old )
    
  }
}