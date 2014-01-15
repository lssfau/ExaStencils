package harald.expert

import scala.collection.mutable.ListBuffer
import harald.Impl._
import harald.ast.TreeL2
import harald.dsl._

class OwnFunctions(treel2 : TreeL2) {


  def initMain() {
    
    val nlevels: Int = DomainKnowledge.nlevels_L3.getOrElse(1)
    var body: ListBuffer[ImplStatement] = ListBuffer()


    if (DomainKnowledge.use_MPI) {
  body += new ImplExternalStatement(s"int Pperiods[${DomainKnowledge.rule_dim}];\n")
  body += new ImplExternalStatement("MPI_Init(&argc, &argv); \n")
  body += new ImplExternalStatement("MPI_Comm_size(MPI_COMM_WORLD, &noprocesses);\n")
  body += new ImplExternalStatement("MPI_Comm_rank(MPI_COMM_WORLD, &rank);\n")
  body += new ImplExternalStatement("std::cout << \"MPI rank: \" << rank << \" of \" << noprocesses << std::endl;\n")
  body += new ImplExternalStatement(s"for (int i=0; i<${DomainKnowledge.rule_dim}; i++ )  {\n")
  body += new ImplExternalStatement("  Pdims[i] = 0;\n")
  body += new ImplExternalStatement("  Pcoords[i] = 0;\n")
  body += new ImplExternalStatement("  Pperiods[i] = 0; }\n")
  body += new ImplExternalStatement(s"MPI_Dims_create(noprocesses,${DomainKnowledge.rule_dim}, Pdims);\n")
  body += new ImplExternalStatement(s"MPI_Cart_create(MPI_COMM_WORLD, ${DomainKnowledge.rule_dim}, Pdims, Pperiods, 0, &COMM_CART);\n")
  body += new ImplExternalStatement(s"MPI_Cart_coords(COMM_CART, rank, ${DomainKnowledge.rule_dim}, Pcoords);\n")
  body += new ImplExternalStatement("if (rank == 0)\n")
  body += new ImplExternalStatement("std::cout << \" Pdims \" << Pdims[0] << \" \" << Pdims[1] ")
  if (DomainKnowledge.rule_dim == 3)
  body += new ImplExternalStatement(" << Pdims[2]")
  body += new ImplExternalStatement("<< std::endl;\n")  
  body += new ImplExternalStatement("std::cout << rank << \" Pcoords \"  << Pcoords[0] << \" \" << Pcoords[1]")
  if (DomainKnowledge.rule_dim == 3)
  body += new ImplExternalStatement(" << Pcoords[2]")
  body += new ImplExternalStatement("<< std::endl;\n")  
  body += new ImplExternalStatement("int l,r;\n")

  for(i <- 0 to DomainKnowledge.rule_dim-1) {
   body += new ImplExternalStatement(s"MPI_Cart_shift(COMM_CART, ${i}, 1, &l, &r);\n")
  body += new ImplExternalStatement(s"Pnb[2*${i}]=l;\n")
  body += new ImplExternalStatement(s"Pnb[2*${i}+1]=r;\n")
  body += new ImplExternalStatement("std::cout << \" l \" << " + s"Pnb[2*${i}]" + "<< \" r \" << " + s"Pnb[2*${i}+1]" + " << std::endl;\n")
  }
  body += new ImplExternalStatement("MPI_Barrier(MPI_COMM_WORLD);\n")
    }
    
    if (DomainKnowledge.use_Openmp) 
    body += new ImplExternalStatement(s"omp_set_num_threads(${DomainKnowledge.cores_HW.get});\n")

    
    if (DomainKnowledge.use_gpu) {
      body += new ImplExternalStatement("cudaSetDevice(0);\n")
      body += new ImplExternalStatement("cudaFree(0);\n")
    }
    
    for (c <- treel2.Fields) 
      body += new ImplExternalStatement(s"${c.name} = new ${c.arrname}<${c.datatype}>[${nlevels}];\n")
    if (DomainKnowledge.use_MPI) 
    for (c <- treel2.GhostFields) 
      body += new ImplExternalStatement(s"${c.name} = new ${c.arrname}<${c.datatype}>[${nlevels}];\n")
    
    for (c <- treel2.Stencils) {
      if (c.weakform.equals(""))
        body += new ImplExternalStatement(s"${c.name} = new ${treel2.ExternalClasses.get("Stencil").get.name}<${c.datatype}>[1];\n")
      else
        body += new ImplExternalStatement(s"${c.name} = new ${treel2.ExternalClasses.get("StencilVar").get.name}<${c.datatype}>[${nlevels}];\n")

      if ((c.sizex == 1) && (c.entries.length > 0))
        body += new ImplExternalStatement(s"${c.name}[0].resize(${c.entries.length});\n")

    }

    val setfuncarrname: String = "set" //extclasses.get("Array").get.memberfunc("set").name 
    val setrandfuncname: String = "setrandom" //extfunctions.get("setRandom").get.name 
    /*	for (c <- TransformL2.Fields) {
			var sx:Int = c.sizex;
			var sy:Int = c.sizey;
			var sz:Int = c.sizez;
			for ( i <- 0 to (nlevels - 1) ) {
			  var setst = s"${c.name}[${i}].resize(${sx}+${c.addpoints},${sy}+${c.addpoints}"
			  sx = sx / 2;
			  sy = sy / 2;
			  if (DomainKnowledge.rule_dim() == 3) {
			    setst = setst + s",${sz}+${c.addpoints}"
			    sz = sz / 2;
			  }
			  setst = setst + ");"
			  body += new ExternalStatement(setst+"\n")
			}
			body += new ExternalStatement(s"${setfuncarrname}(0,${c.name}[0],0);\n")
		}
 */ for (c <- DomainKnowledge.global_fields) {
       var pdims : ListBuffer[String] = ListBuffer()
       if (DomainKnowledge.use_MPI) {
         for (i <- 0 to 2)
          pdims += s"/Pdims[${i}]"
       } else {
         for (i <- 0 to 2)
          pdims += ""
       }

      var setst = s"${c.name}.resize(${c.sizex}${pdims(0)}+${c.addpoints},${c.sizey}${pdims(1)}+${c.addpoints}"
      if (DomainKnowledge.rule_dim() == 3)
        setst = setst + s",${c.sizez}${pdims(2)}+${c.addpoints}"
      setst = setst + ");"
      body += new ImplExternalStatement(setst + "\n")

    }

    if (DomainKnowledge.use_MPI) 
  for (c <- DomainKnowledge.global_ghost_fields) {
      var setst = s"${c.name}.resize(${c.sizex}+${c.addpoints}"
      if (c.sizey > 1)
        setst += s",${c.sizey}+${c.addpoints}"
      else  
        setst += ",1"
      if (DomainKnowledge.rule_dim() == 3)
      if (c.sizez > 1)
        setst += s",${c.sizez}+${c.addpoints}"
      else  
        setst += ",1"
      setst = setst + ");"
      body += new ImplExternalStatement(setst + "\n")

    }
//    for (c <- DomainKnowledge.global_ghost_fields)
//      body += new ExternalStatement(s"${c.name} = new ${DataClasses.extclasses.get("Array").get.name}<${c.datatype}>[${nlevels}];\n")
    for (c <- treel2.Fields)
      if (c.arrname.equals("MyArray"))
        body += new ImplExternalStatement(s"${setfuncarrname}(0,${c.name}[0],0);\n")
      else
        body += new ImplExternalStatement(s"${setfuncarrname+"cuda"}(0,${c.name}[0],0);\n")

    for (i <- 0 to DomainKnowledge.function_L1.length - 1)
      if (DomainKnowledge.use_gpu) {
       body += new ImplExternalStatement(s"${setrandfuncname}(${DomainKnowledge.unknown_L1(i)._1+"_host"}[0],${DomainKnowledge.unknown_L1(i)._2});\n")
       body += new ImplExternalStatement(s"pushDataToDevice (${DomainKnowledge.unknown_L1(i)._1+"_host"}[0].begin(),${DomainKnowledge.unknown_L1(i)._1}[0].begin(), ${DomainKnowledge.unknown_L1(i)._1}[0].x1_*${DomainKnowledge.unknown_L1(i)._1}[0].x2_);\n")
      } else
       body += new ImplExternalStatement(s"${setrandfuncname}(${DomainKnowledge.unknown_L1(i)._1}[0],${DomainKnowledge.unknown_L1(i)._2});\n")

    for (c <- DomainKnowledge.global_stencils) {
      if (c.sizex != 1) {
        var setst = s"${c.name}.resize(${c.sizex}+${c.addpoints},${c.sizey}+${c.addpoints}"
        if (DomainKnowledge.rule_dim() == 3)
          setst = setst + s",${c.sizez}+${c.addpoints}"
        setst = setst + s",${c.length});"
        body += new ImplExternalStatement(setst + "\n")
      }
    }

    if (DomainKnowledge.use_FE)
      body += new ImplExternalStatement("setdiscretizationFE(0);\n")

    for (c <- treel2.Stencils) {
      for (i <- 0 to c.entries.length - 1)
        body += new ImplExternalStatement(s"${c.name}[0].set(${i},${c.entries(i)});\n")
    }
    
    for (c <- treel2.Functions) {
      println(c._2.name)
      if (c._2.name.equals("Application")) 
        body += new ImplExternalStatement(c._2.toString_cpp_body)
    }
    /*
    body += new ImplExternalStatement(s"${DomainKnowledge.datatype_L2.getOrElse("double")} res_0 = L2Residual ( 0 );\n")
    body += new ImplExternalStatement("std::cout << \"Starting residual: \" << res_0 << std::endl;\n")
    body += new ImplExternalStatement(s"${DomainKnowledge.datatype_L2.getOrElse("double")} res = res_0;\n")
    body += new ImplExternalStatement(s"${DomainKnowledge.datatype_L2.getOrElse("double")} res_old = 0;\n")

    var loopbody: ListBuffer[ImplStatement] = ListBuffer()

    // val itersp = if (DomainKnowledge.iters_L3.isDefined) DomainKnowledge.iters_L3.get else "iters"  
    //body += new ExternalStatement(s"for (int  i = 0; i <= (${itersp}-1); ++i) {\n")
    loopbody += new ImplExternalStatement("res_old = res;\n")
    loopbody += new ImplPcall("", "VCycle", ListBuffer(new ImplValueExpr[Int](0))) // ExternalStatement("VCycle ( 0 );\n") 
    loopbody += new ImplExternalStatement("res = L2Residual ( 0 );\n")
    loopbody += new ImplExternalStatement("std::cout << \"Cycle: \" << i0 << \" Residual: \" << res << \" residual reduction: \" << res_0/res << \" convergence factor: \" << res/res_old << std::endl;\n")
    //body += new ExternalStatement("}\n")
    body += new Implforloop(ListBuffer(new ParameterInfo("i","int")),ListBuffer(new ImplValueExpr[Int](0)), ListBuffer(new ImplValueExpr[Int](DomainKnowledge.iters_L3.getOrElse(1))), ListBuffer(1, 1, 1), "lex", 1, loopbody)
*/
    if (DomainKnowledge.use_MPI) 
    body += new ImplExternalStatement("MPI_Finalize();\n")

   // if ((!DomainKnowledge.use_gpu)) // && (!DomainKnowledge.use_MPI)) 
   // body += new ImplExternalStatement("int ch; std::cin >> ch;")

    treel2.extfunctions += "Main" -> new ImplFunction("main", "int", ListBuffer(new ParameterInfo("argc", "int"),new ParameterInfo("argv", "char**")), body,Map(), "cpu")

  }

  def initBC() {
    
        var bcloops: ListBuffer[ImplStatement] = ListBuffer()
    var lev = 0

    for (v <- DomainKnowledge.fragments(0).vertices) {

      var ghosts = ""
      var s = ""

      for (i <- 0 to DomainKnowledge.rule_dim() - 1) {
        var locals = ""

        if (IdxKnowledge.mapcoordToidxInt(v.coords, lev)(i) == 0)
          locals = "0"
        else
          locals = s"${DomainKnowledge.unknown_L1(0)._1}[lev].x${i + 1}_-1"

        if (IdxKnowledge.mapcoordToidxInt(v.coords, lev)(i) == 0)
          ghosts = ghosts + locals + "+1"
        else
          ghosts = ghosts + locals + "-1"

        s = s + locals

        if (i < DomainKnowledge.rule_dim() - 1) {
          ghosts = ghosts + ","
          s = s + ","
        }
      }

      if (DomainKnowledge.pdebc_L1.get._2.equals("dn")) {
        bcloops += new ImplExternalStatement(s"${DomainKnowledge.pdebc_L1.get._1}[lev](${s}) = ${DomainKnowledge.pdebc_L1.get._1}[lev](${ghosts});\n")
      } else if (DomainKnowledge.pdebc_L1.get._2.equals("zero"))
        bcloops += new ImplExternalStatement(s"${DomainKnowledge.pdebc_L1.get._1}[lev](${s}) = 0;\n")
    }

    for (e <- DomainKnowledge.fragments(0).edges) {

      val vertex1 = e.vertex1.coords
      val vertex2 = e.vertex2.coords

      if (DomainKnowledge.pdebc_L1.get._2.equals("dn"))
      bcloops += generateBCidxloop(vertex1, vertex2, DomainKnowledge.pdebc_L1.get._1+"[lev]", DomainKnowledge.pdebc_L1.get._1+"[lev]", false,  lev)
      else
      bcloops += generateBCidxloop(vertex1, vertex2, DomainKnowledge.pdebc_L1.get._1+"[lev]", "0", true,  lev)
        
    }

    for (f <- DomainKnowledge.fragments(0).faces) {

      val vertex1 = f.vertices(0).coords
      val vertex2 = f.vertices(f.vertices.length - 1).coords // ASSUME vertices are ordered !!!

      if (DomainKnowledge.pdebc_L1.get._2.equals("dn"))
      bcloops += generateBCidxloop(vertex1, vertex2, DomainKnowledge.pdebc_L1.get._1+"[lev]", DomainKnowledge.pdebc_L1.get._1+"[lev]", false,  lev, "BC")
      else
      bcloops += generateBCidxloop(vertex1, vertex2, DomainKnowledge.pdebc_L1.get._1+"[lev]", "0", true,  lev, "BC")
    }

    treel2.extfunctions += "BC" -> new ImplFunction("treatBoundary", "void", ListBuffer(new ParameterInfo("lev", "int")),
      bcloops,Map(), "cpu")

  }
  
  
  def initcopyToBuf() {
    
    var bcloops: ListBuffer[ImplStatement] = ListBuffer()
    var lev = 0
    var i = 0
    
    for (e <- DomainKnowledge.fragments(0).edges) {

      val vertex1 = e.vertex1.coords
      val vertex2 = e.vertex2.coords
      
      bcloops += new ImplExternalStatement(s"if (Pnb[${i}] >= 0)")
      bcloops += generateBCidxloop(vertex1, vertex2,DomainKnowledge.pdebc_L1.get._1+s"_ghost_edge${i}_send[0]",  DomainKnowledge.pdebc_L1.get._1+"[lev]", false,lev,"Buffer")
      i += 1
    }

    treel2.extfunctions += "copyToBuffers" -> new ImplFunction("copyToBuffers", "void", ListBuffer(new ParameterInfo("lev", "int")),
      bcloops,Map(), "cpu")
  }
  
  def initcopyFromBuf() {
    
    var bcloops: ListBuffer[ImplStatement] = ListBuffer()
    var lev = 0
    var i = 0

    for (e <- DomainKnowledge.fragments(0).edges) {

      val vertex1 = e.vertex1.coords
      val vertex2 = e.vertex2.coords

      bcloops += new ImplExternalStatement(s"if (Pnb[${i}] >= 0)")
      bcloops += generateBCidxloop(vertex1, vertex2, DomainKnowledge.pdebc_L1.get._1+"[lev]", DomainKnowledge.pdebc_L1.get._1+s"_ghost_edge${i}_recv[0]", false,lev,"Buffer")
      i += 1

    }

    treel2.extfunctions += "copyFromBuffers" -> new ImplFunction("copyFromBuffers", "void", ListBuffer(new ParameterInfo("lev", "int")),
      bcloops,Map(), "cpu")
  }
  
  def initextFunctions() {

    initMain()

    initBC()
    
    if (DomainKnowledge.use_MPI) {
      initcopyToBuf()
      initcopyFromBuf()
    }
    


    var rottempl: ListBuffer[ListBuffer[Double]] = ListBuffer()
    var rotdeg: ListBuffer[Int] = ListBuffer()

    for (i <- 0 to 7) {
      rottempl += ListBuffer(Math.cos(i * Math.PI / 4.0), Math.sin(i * Math.PI / 4.0), -Math.sin(i * Math.PI / 4.0), Math.cos(i * Math.PI / 4.0))
      rotdeg += i * 45
    }
    println("rottemplate " + rottempl)

    for (es <- DomainKnowledge.fragments(0).edges) {
      for (ed <- DomainKnowledge.fragments(0).edges) {

        var trans: ListBuffer[Double] = ListBuffer()
        for (i <- 0 to es.vertex1.coords.length - 1)
          trans += ed.vertex1.coords(i) - es.vertex1.coords(i)

        var shiftesv2 = new Vertex(ListBuffer(es.vertex2.coords(0) - es.vertex1.coords(0), es.vertex2.coords(1) - es.vertex1.coords(1)))
        var shiftedv2 = new Vertex(ListBuffer(ed.vertex2.coords(0) - trans(0) - es.vertex1.coords(0), ed.vertex2.coords(1) - trans(1) - es.vertex1.coords(1)))

        // println("give: " + shiftesv2 + " " + shiftedv2)
        var rotidx = 0

        for (i <- 0 to 7)
          if (checkifrotated(shiftesv2, shiftedv2, rottempl(i))) {
            // println("rotated: " + rottempl(i))
            rotidx = i
          }
        // println("map " + es + " to " + ed + " tr: " + trans + " rot: " + rotdeg(rotidx))
      }

    }
  }

  def checkifrotated(p1: Vertex, p2: Vertex, rot: ListBuffer[Double]): Boolean = {
    for (i <- 0 to p1.coords.length - 1)
      if (Math.abs(p2.coords(i) - (p1.coords(0) * rot(2 * i) + p1.coords(1) * rot(2 * i + 1))) > 0.0001)
        return false
    return true
  }


  def generateBCidxloopstatement(ghostshifts : ListBuffer[Int],loopshifts: ListBuffer[Int], To: String, From:String, FromValue : Boolean, mode : String = "BC"): ImplStatement = {

     if (mode.equals("BC")) {
//    if (DomainKnowledge.pdebc_L1.get._2.equals("dn")) {
      var idxshift = s"i0+${ghostshifts(0)},i1+${ghostshifts(1)}"
      if (DomainKnowledge.rule_dim() == 3)
        idxshift = idxshift + s",i2+${ghostshifts(2)}"

      if (FromValue)
        return new ImplExternalStatement(s"${To}${DomainKnowledge.rule_idxArray_cpp()} = ${From};\n") // ${DomainKnowledge.pdebc_L1.get._2}
      else  
      return new ImplExternalStatement(s"${To}${DomainKnowledge.rule_idxArray_cpp()} = ${From}(${idxshift});\n")
//      return new ExternalStatement(s"${DomainKnowledge.pdebc_L1.get._1}[lev]${DomainKnowledge.rule_idxArray_cpp()} = ${DomainKnowledge.pdebc_L1.get._1}[lev](${idxshift});\n")
//    } else if (DomainKnowledge.pdebc_L1.get._2.equals("zero"))
//      return new ExternalStatement(s"${DomainKnowledge.pdebc_L1.get._1}[lev]${DomainKnowledge.rule_idxArray_cpp()} = 0;\n") // ${DomainKnowledge.pdebc_L1.get._2}
     } else if (mode.equals("Buffer")) {
       
      var idxshift = s"i0+${ghostshifts(0)},i1+${ghostshifts(1)}"
      if (DomainKnowledge.rule_dim() == 3)
        idxshift = idxshift + s",i2+${ghostshifts(2)}"

      var ghostidxstr = ""
      if (loopshifts(0) == 0)
        ghostidxstr = ".a[i1]"
      else    
        ghostidxstr = ".a[i0]"
          
      var lhs = ""    
      var rhs = ""
      if (To.contains("ghost"))
        lhs = s"${To}${ghostidxstr}"
      else
        lhs = s"${To}${DomainKnowledge.rule_idxArray_cpp()}"
      if (From.contains("ghost"))
        rhs = s"${From}${ghostidxstr}"
      else
        rhs = s"${From}(${idxshift})"
      
      return new ImplExternalStatement(s"${lhs} = ${rhs};\n")
      
     }
     return new ImplStatement
  }

  def generateBCidxloop(vertex1: ListBuffer[Double], vertex2: ListBuffer[Double], ToArray:String, FromArray: String, FromValue : Boolean = false, lev: Int = 0, mode : String = "BC"): ImplStatement = {

    val loopshifts = IdxKnowledge.mapcoordToidxLoop(vertex1, vertex2, lev)
    var ghostshifts: ListBuffer[Int] = ListBuffer()

    var start: ListBuffer[ImplExpression] = ListBuffer()
    var stop: ListBuffer[ImplExpression] = ListBuffer()

    for (i <- 0 to DomainKnowledge.rule_dim() - 1) {
      start += new ImplValueExpr[Int](IdxKnowledge.mapcoordToidxInt(vertex1, lev)(i))
      if (IdxKnowledge.mapcoordToidxInt(vertex2, lev)(i) == 0)
        stop += new ImplValueExpr[Int](0)
      else
        stop += new ImplValueExpr[String](s"${DomainKnowledge.pdebc_L1.get._1+"[lev]"}.x${i + 1}_-1")

      if (loopshifts(i) == 0) {
        if (IdxKnowledge.mapcoordToidxInt(vertex2, lev)(i) == 0)
          ghostshifts += 1
        else
          ghostshifts += -1
      } else
        ghostshifts += 0

    }


    return new Implforloop(ListBuffer(new ParameterInfo("i","int")),start, stop, loopshifts, "lex", 1, ListBuffer(generateBCidxloopstatement(ghostshifts,loopshifts, ToArray, FromArray,  FromValue, mode)))
  }

}