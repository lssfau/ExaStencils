package harald.Generate

import java.io._
import harald.dsl.DomainKnowledge
import harald.Discrete._

case class OptionsL2(datatype : String, discretization : String, location : String, nlevels:Int)

object GenerateL2 {

  def generateall(path:String, fname:String) {
    
    var num = 0;
    val dt = List("Double","Float")
    val dis = List("FD","FE")
    val loc = List("nodes","cells")
    val nlev = List(1,2,3,4,5,6,7,8)
    
    for (d <- dt)
    for (di <- dis)
    for (le <- nlev)
    for (l <- loc) {
      num = num + 1
      transformL1toL2opt(path+fname+d+di+l+"lev2.mg",OptionsL2(d,di,l,le))
    }

  }

  def getoptionsdefault() : OptionsL2 = OptionsL2("Double","FD","nodes",DomainKnowledge.getmaxlevel)
  
  def transformL1toL2(fname: String) {
    val opt = getoptionsdefault
    transformL1toL2opt(fname,opt)    
  }
  
  // TODO: introduce no. of times (time steps?) for each field = no. of copies
   
  def transformL1toL2opt(fname: String, opt : OptionsL2) {

    val writer = new PrintWriter(new File(fname))

/*    val fragname = DomainKnowledge.domain_L1.get._2 match {
      case "UnitSquare" => "Regular_Square"
      case "UnitCube" => "Regular_Cube"
    }
*/
    val fragname = DomainKnowledge.rule_dim() match {
      case 2 => "Regular_Square"
      case 3 => "Regular_Cube"
      case _ => "Regular"  
    } 
    
    writer.write(s"Fragments f1 = ${fragname}\n")
    writer.write("\n")

    var size = 1
    var nlevels = opt.nlevels
    
    //   println(Math.pow(10, DomainKnowledge.accuracy_L1.get))
    while (size < Math.pow(2, DomainKnowledge.accuracy_L1.get)) {
      size *= 2
    }

    writer.write(s"Discrete_Domain ${DomainKnowledge.cont_domains(0).name} levels ${nlevels-1} { \n")
    writer.write(s"xsize_L2 = ${size}  \n")
    writer.write(s"xcoarsefac_L2 = 2  \n")
    writer.write(s"ysize_L2 = ${size}  \n")
    writer.write(s"ycoarsefac_L2 = 2  \n")

    if (DomainKnowledge.rule_dim() == 3) {
      writer.write(s"zsize_L2 = ${size}  \n")
      writer.write(s"zcoarsefac_L2 = 2  \n")
    }
    writer.write("} \n")

    writer.write("\n")


    DomainKnowledge.globaldatatype_L2 = opt.datatype
    DomainKnowledge.unknownlocation_L2 = opt.location
                       
    DiscrDescription.setup(DomainKnowledge.globaldatatype_L2,DomainKnowledge.unknownlocation_L2)

    //DomainKnowledge.globaldatatype_L2 = opt.datatype     
    var datatypefields = DomainKnowledge.globaldatatype_L2
    var datatypestencils = DomainKnowledge.globaldatatype_L2
    var unknownlocation = DomainKnowledge.unknownlocation_L2
    var discretization = opt.discretization
    
    DomainKnowledge.vectorentries = DomainKnowledge.system_L1.getOrElse(1)
//    val matrixentries = s"${DomainKnowledge.system_L1.getOrElse(1)},${DomainKnowledge.system_L1.getOrElse(1)}"
    
    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion")) {
      //unknownlocation = "cells"
      if (DomainKnowledge.use_gpu)
        DomainKnowledge.vectorentries = 2
    }
        
    for (f <- DomainKnowledge.cont_functions) {
      writer.write(s"DiscreteFunction<${datatypefields},${f.functype.codomainvectorlength}>@${unknownlocation} ${f.nam} \n") 
  /*    if (f.nam.equals(DomainKnowledge.unknown_L1(0)._1)) {
        writer.write(s"DiscreteFunction<${f.functype.codomain},${f.functype.codomainvectorlength}>@${f.evalpoints} ${f.nam+"_old"}  \n") 
        writer.write(s"DiscreteFunction<${f.functype.codomain},${f.functype.codomainvectorlength}>@${f.evalpoints} Res  \n") 
      }
   */ }
      
/*    for (i <- 0 to DomainKnowledge.function_L1.length - 1) 
      writer.write(s"field<${datatypefields},${DomainKnowledge.vectorentries}>@${DomainKnowledge.unknownlocation_L2} ${DomainKnowledge.function_L1(i)._1}  \n")
    for (i <- 0 to DomainKnowledge.unknown_L1.length - 1) {
      writer.write(s"field<${datatypefields},${DomainKnowledge.vectorentries}>@${DomainKnowledge.unknownlocation_L2} ${DomainKnowledge.unknown_L1(i)._1}  \n")
      writer.write(s"field<${datatypefields},${DomainKnowledge.vectorentries}>@${DomainKnowledge.unknownlocation_L2} ${DomainKnowledge.unknown_L1(i)._1}_old  \n")
    }
  */  
    // TODO: move to L1!
    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion"))
      writer.write(s"DiscreteFunction<${datatypestencils},${DomainKnowledge.vectorentries}>@${DomainKnowledge.unknownlocation_L2} coeff \n")
      
//    for (f <- DomainKnowledge.discr_functions) 
//      if (f.nam.equals(DomainKnowledge.unknown_L1(0)._1))
    for (o <- DomainKnowledge.cont_operators) {
 //    if (o.name.equals(DomainKnowledge.operator_L1(0)._1)) {
      writer.write(s"DiscreteOperator<${datatypestencils},${o.optype.inputfunctype.codomainvectorlength},${o.optype.outputfunctype.codomainvectorlength}, ${discretization}, 2>@${unknownlocation} ${o.nam}  \n")          
//      writer.write(s"DiscreteOperator<${f.functype.codomain},1,1, FD, 2>@${f.evalpoints} RestrictionStencil  \n")
 //    }
    }
    
//    for (i <- 0 to DomainKnowledge.operator_L1.length - 1)
//      writer.write(s"stencil<${datatypestencils},${matrixentries}, ${discretization}, 2>@${DomainKnowledge.unknownlocation_L2} ${DomainKnowledge.operator_L1(i)._1}  \n")
//    writer.write(s"stencil<${datatypestencils},1,1, FD, 2>@${DomainKnowledge.unknownlocation_L2} RestrictionStencil  \n")

    writer.close()
    if (DomainKnowledge.debugmode)
      println("DSL level 2 was generated in " + fname)
  }
}
