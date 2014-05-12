package harald.expert

import scala.collection.mutable.ListBuffer
import harald.Impl._
import harald.dsl._
import harald.ast._

class DataClasses(treel2 : TreeL2) {

  def initextClasses() = {

    initArrayClass()
    initStencilClass()
  }

  def initArrayClass() : Unit = {
    // Array Class on CPU

    var ArrayClassName = DomainKnowledge.ArrayClassName

    var memfunc : ListBuffer[ImplFunction] = ListBuffer()
    var cmemlist : ListBuffer[ParameterInfo] = ListBuffer()
    var idxdimmemlist : ListBuffer[String] = ListBuffer()
    var idxdimparlist : ListBuffer[String] = ListBuffer()

    cmemlist += new ParameterInfo("a", "T*")
    for (i <- 1 to DomainKnowledge.rule_dim()) {
      idxdimmemlist += "x" + i.toString + "_"
      idxdimparlist += "x" + i.toString
      cmemlist += new ParameterInfo(idxdimmemlist(i - 1), "int")
    }

    var sizeidx : String = idxdimmemlist(0)
    for (i <- 1 to DomainKnowledge.rule_dim() - 1)
      sizeidx = sizeidx + "*" + idxdimmemlist(i)

    memfunc += new ImplFunction(ArrayClassName, "", ListBuffer(new ParameterInfo("", "")), ListBuffer(new ImplStatement()), Map(), "cpu")

    var memlist : ListBuffer[ParameterInfo] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      memlist += new ParameterInfo(idxdimparlist(i - 1), "int")
    var bodylist : ListBuffer[ImplStatement] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      bodylist += new ImplExternalStatement(s"${idxdimmemlist(i - 1)} = ${idxdimparlist(i - 1)};")
    bodylist += new ImplExternalStatement(s"a = new T[${sizeidx}];")

    memfunc += new ImplFunction(ArrayClassName, "", memlist, bodylist, Map(), "cpu")
    memfunc += new ImplFunction("resize", "void", memlist, bodylist, Map(), "cpu")
    memfunc += new ImplFunction("begin", "T*", new ListBuffer(), ListBuffer(new ImplExternalStatement("return &a[0];")), Map(), "cpu")
    for (i <- 1 to DomainKnowledge.rule_dim())
      memfunc += new ImplFunction("s" + i.toString, "int", ListBuffer(new ParameterInfo("", "")), ListBuffer(new ImplExternalStatement(s"return ${idxdimmemlist(i - 1)};")), Map(), "cpu")

    var idxlin : String = IdxKnowledge.mapidxToLinear(idxdimparlist, idxdimmemlist)
    memfunc += new ImplFunction("operator()", "T&", memlist, ListBuffer(new ImplExternalStatement("return a[" + idxlin + "]; ")), Map(), "cpu")

    if (DomainKnowledge.rule_dim == 2)
      memfunc += new ImplFunction("plot", "void", ListBuffer(new ParameterInfo("fname", "string")),
        ListBuffer(new ImplExternalStatement("ofstream os(fname.c_str());\n for (int i=0;i < x1_; i++) \n for (int j=0;j < x2_; j++) \n  os << i << \" \" << j << \" \" << a[i*x2_ + j] << endl;")), Map(), "cpu")
    else if (DomainKnowledge.rule_dim == 3)
      memfunc += new ImplFunction("plot", "void", ListBuffer(new ParameterInfo("fname", "string")),
        ListBuffer(new ImplExternalStatement("ofstream os(fname.c_str());\n for (int i=0;i < x1_; i++) \n for (int j=0;j < x2_; j++) \n for (int k=0;k < x3_; k++) \n  os << i << \" \" << j << \" \" << k << \" \" << a[i*x2_*x3_ + j*x3_ + k] << endl;")), Map(), "cpu")

    treel2.ExternalClasses += "Array" -> new ImplClass(ArrayClassName, "T",
      cmemlist,
      memfunc)

    // Array Class on GPU
    ArrayClassName = DomainKnowledge.ArrayClassNameGPU

    var memfunccuda : ListBuffer[ImplFunction] = ListBuffer()

    memfunccuda += new ImplFunction(ArrayClassName, "", ListBuffer(new ParameterInfo("", "")), ListBuffer(new ImplStatement()), Map(), "cpu")

    var memlistcuda : ListBuffer[ParameterInfo] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      memlistcuda += new ParameterInfo(idxdimparlist(i - 1), "int")
    var bodylistcuda : ListBuffer[ImplStatement] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      bodylistcuda += new ImplExternalStatement(s"${idxdimmemlist(i - 1)} = ${idxdimparlist(i - 1)};")
    bodylistcuda += new ImplExternalStatement(s"cudaMalloc ( ( void ** ) &a, sizeof (T) * ${sizeidx});")

    memfunccuda += new ImplFunction(ArrayClassName, "", memlist, bodylistcuda, Map(), "cpu")
    memfunccuda += new ImplFunction("begin", "T*", new ListBuffer(), ListBuffer(new ImplExternalStatement("return &a[0];")), Map(), "cpu")
    memfunccuda += new ImplFunction("resize", "void", memlist, bodylistcuda, Map(), "cpu")

    treel2.ExternalClasses += "ArrayCuda" -> new ImplClass(ArrayClassName, "T",
      cmemlist,
      memfunccuda)

    // Matrix Class 
    ArrayClassName = DomainKnowledge.MatrixClassName

    var Mmemfunc : ListBuffer[ImplFunction] = ListBuffer()
    var Mcmemlist : ListBuffer[ParameterInfo] = ListBuffer()
    var Midxdimmemlist : ListBuffer[String] = ListBuffer()
    var Midxdimparlist : ListBuffer[String] = ListBuffer()

    Mcmemlist += new ParameterInfo("a", "T*")
    for (i <- 1 to 2) {
      Midxdimmemlist += "x" + i.toString + "_"
      Midxdimparlist += "x" + i.toString
      Mcmemlist += new ParameterInfo(Midxdimmemlist(i - 1), "int")
    }

    var Msizeidx : String = Midxdimmemlist(0)
    Msizeidx = Msizeidx + "*" + Midxdimmemlist(1)

    Mmemfunc += new ImplFunction(ArrayClassName, "", ListBuffer(new ParameterInfo("", "")), ListBuffer(new ImplStatement()), Map(), "cpu")

    var Mmemlist : ListBuffer[ParameterInfo] = ListBuffer()
    for (i <- 1 to 2)
      Mmemlist += new ParameterInfo(Midxdimparlist(i - 1), "int")
    var Mbodylist : ListBuffer[ImplStatement] = ListBuffer()
    for (i <- 1 to 2)
      Mbodylist += new ImplExternalStatement(s"${Midxdimmemlist(i - 1)} = ${Midxdimparlist(i - 1)};")
    Mbodylist += new ImplExternalStatement(s"a = new T[${Msizeidx}];")

    Mmemfunc += new ImplFunction(ArrayClassName, "", Mmemlist, Mbodylist, Map(), "cpu")
    Mmemfunc += new ImplFunction("resize", "void", Mmemlist, Mbodylist, Map(), "cpu")
    Mmemfunc += new ImplFunction("begin", "T*", new ListBuffer(), ListBuffer(new ImplExternalStatement("return &a[0];")), Map(), "cpu")
    for (i <- 1 to 2)
      Mmemfunc += new ImplFunction("s" + i.toString, "int", ListBuffer(new ParameterInfo("", "")), ListBuffer(new ImplExternalStatement(s"return ${Midxdimmemlist(i - 1)};")), Map(), "cpu")

    var Midxlin : String = IdxKnowledge.mapidxToLinear(Midxdimparlist, Midxdimmemlist)
    Mmemfunc += new ImplFunction("operator()", "T&", Mmemlist, ListBuffer(new ImplExternalStatement("return a[" + Midxlin + "]; ")), Map(), "cpu")

    treel2.ExternalClasses += "Matrix" -> new ImplClass(ArrayClassName, "T",
      Mcmemlist,
      Mmemfunc)

  }

  def initStencilClass() = {

    // Const Stencil Class on CPU
    var StencilClassName = DomainKnowledge.StencilClassName

    var cmemlist : ListBuffer[ParameterInfo] = ListBuffer()
    var idxdimmemlist : ListBuffer[String] = ListBuffer()
    var idxdimparlist : ListBuffer[String] = ListBuffer()

    cmemlist += new ParameterInfo("a", "T*")
    for (i <- 1 to DomainKnowledge.rule_dim()) {
      idxdimmemlist += "x" + i.toString + "_"
      idxdimparlist += "x" + i.toString
      cmemlist += new ParameterInfo(idxdimmemlist(i - 1), "int")
    }

    var sizeidx : String = idxdimmemlist(0)
    for (i <- 1 to DomainKnowledge.rule_dim() - 1)
      sizeidx = sizeidx + "*" + idxdimmemlist(i)

    var memlist : ListBuffer[ParameterInfo] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      memlist += new ParameterInfo(idxdimparlist(i - 1), "int")

    var idxlin : String = IdxKnowledge.mapidxToLinear(idxdimparlist, idxdimmemlist)
    var memfuncS : ListBuffer[ImplFunction] = ListBuffer()

    memfuncS += new ImplFunction(StencilClassName, "", ListBuffer(new ParameterInfo("", "")), ListBuffer(new ImplStatement()), Map(), "cpu")
    memfuncS += new ImplFunction(StencilClassName, "", ListBuffer(new ParameterInfo("s", "int")), ListBuffer(new ImplExternalStatement("size = s;entries.resize(s); ")), Map(), "cpu")
    memfuncS += new ImplFunction("resize", "void", ListBuffer(new ParameterInfo("s", "int")), ListBuffer(new ImplExternalStatement("size = s;entries.resize(s); ")), Map(), "cpu")
    memfuncS += new ImplFunction("set", "void", ListBuffer(new ParameterInfo("idx", "int"), new ParameterInfo("v", DomainKnowledge.transform_datatype_cpp(DomainKnowledge.globaldatatype_L2))), ListBuffer(new ImplExternalStatement("entries[idx] = v; ")), Map(), "cpu")
    memfuncS += new ImplFunction("begin", "T*", new ListBuffer(), ListBuffer(new ImplExternalStatement("return &entries[0];")), Map(), "cpu")
    if (DomainKnowledge.stenciltype.equals("nonlinear")) {
      memfuncS += new ImplFunction("setlevel", "void", ListBuffer(new ParameterInfo("l", "int")), ListBuffer(new ImplExternalStatement(s"lev = l; fac = 0.5/ ( pow ( 4.0,lev)); ")), Map(), "cpu")

      var d3dstr = ""
      if (DomainKnowledge.rule_dim == 3)
        d3dstr = ",k"
      /* 
     var body : ListBuffer[ImplStatement] = ListBuffer()
     if (DomainKnowledge.globaldatatype_L2.equals("ComplexDouble"))
       body += new ImplExternalStatement(s"${DomainKnowledge.transform_datatype_cpp_cuda(DomainKnowledge.globaldatatype_L2)} coeff;\n  const double sigma = 1.0;\n  const int kdiff = 2;\n  const double sigmakdiffinv = 1.0/(kdiff*sigma*kdiff*sigma); \n  double denominv = 1.0/( 1.0 + Sol[lev] ( i,j${d3dstr} ).imag()*Sol[lev] ( i,j${d3dstr} ).imag() * sigmakdiffinv);\n  coeff.real( cos ( sigma ) * denominv); \n  coeff.imag( sin ( sigma ) * denominv);\n  return coeff;")
     else
       body += new ImplExternalStatement(s"${DomainKnowledge.transform_datatype_cpp_cuda(DomainKnowledge.globaldatatype_L2)} coeff;\n  const double sigma = 1.0;\n  const int kdiff = 2;\n  const double sigmakdiffinv = 1.0/(kdiff*sigma*kdiff*sigma); \n  double denominv = 1.0/( 1.0 + Sol[lev] ( i,j${d3dstr} )*Sol[lev] ( i,j${d3dstr} ) * sigmakdiffinv);\n  coeff = sigma*denominv; \n return coeff;")
       
     if (DomainKnowledge.rule_dim==2) 
       memfuncS += new ImplFunction("getcoeff", s"${DomainKnowledge.transform_datatype_cpp_cuda(DomainKnowledge.globaldatatype_L2)}", 
         ListBuffer(new ParameterInfo("Sol", s"${DomainKnowledge.ArrayClassName}<${DomainKnowledge.transform_datatype_cpp_cuda(DomainKnowledge.globaldatatype_L2)}>*"), new ParameterInfo("i", "int"), new ParameterInfo("j", "int")), 
         body, Map(), "cpu")      
     else if (DomainKnowledge.rule_dim==3) 
       memfuncS += new ImplFunction("getcoeff", s"${DomainKnowledge.transform_datatype_cpp_cuda(DomainKnowledge.globaldatatype_L2)}", 
         ListBuffer(new ParameterInfo("Sol", s"${DomainKnowledge.ArrayClassName}<${DomainKnowledge.transform_datatype_cpp_cuda(DomainKnowledge.globaldatatype_L2)}>*"), new ParameterInfo("i", "int"), new ParameterInfo("j", "int"), new ParameterInfo("k", "int")), 
         body, Map(), "cpu")      
     */
      /*       
     if (DomainKnowledge.rule_dim==2) 
       memfuncS += new ImplFunction("compst", "void", 
         ListBuffer(new ParameterInfo("Sol", s"${DomainKnowledge.ArrayClassName}<${DomainKnowledge.transform_datatype_cpp_cuda(DomainKnowledge.globaldatatype_L2)}>&"), new ParameterInfo("lev", "int"), new ParameterInfo("i", "int"), new ParameterInfo("j", "int")), 
         ListBuffer(new ImplExternalStatement(s"float fac = 0.5/ ( pow ( 4.0,lev)); \n set(1, -0.5*fac* (coeff[lev]( i,j-1 ) +coeff[lev]( i,j ) )); \n	set(2, -0.5*fac* (coeff[lev]( i,j+1 ) +coeff[lev]( i,j ) ));\n	set(3, -0.5*fac* (coeff[lev]( i-1,j ) +coeff[lev]( i,j ) ));\n	set(4, -0.5*fac* (coeff[lev]( i+1,j ) +coeff[lev]( i,j ) )); \n	set(0, (coeff[lev]( i,j-1 ) +coeff[lev]( i,j+1 ) +coeff[lev]( i-1,j ) +coeff[lev]( i+1,j ) + 4.0*coeff[lev](  i,j ) ) *fac + 0.01);")), 
         Map(), "cpu")      
     else if (DomainKnowledge.rule_dim==3) 
       memfuncS += new ImplFunction("compst", "void", 
         ListBuffer(new ParameterInfo("Sol", s"${DomainKnowledge.ArrayClassName}<${DomainKnowledge.transform_datatype_cpp_cuda(DomainKnowledge.globaldatatype_L2)}>&"), new ParameterInfo("lev", "int"), new ParameterInfo("i", "int"), new ParameterInfo("j", "int"), new ParameterInfo("k", "int")), 
         ListBuffer(new ImplExternalStatement(s"float fac = 0.5/ ( pow ( 4.0,lev)); \n set(1, -0.5*fac* (coeff[lev]( i,j-1,k ) +coeff[lev]( i,j,k ) )); \n	set(2, -0.5*fac* (coeff[lev]( i,j+1,k ) +coeff[lev]( i,j,k ) ));\n	set(3, -0.5*fac* (coeff[lev]( i-1,j,k ) +coeff[lev]( i,j,k ) ));\n	set(4, -0.5*fac* (coeff[lev]( i+1,j,k ) +coeff[lev]( i,j,k ) )); \n set(5, -0.5*fac* (coeff[lev]( i,j,k-1 ) +coeff[lev]( i,j,k ) )); \n set(6, -0.5*fac* (coeff[lev]( i,j,k+1 ) +coeff[lev]( i,j,k ) )); \n set(0, (coeff[lev]( i,j-1,k ) +coeff[lev]( i,j+1,k ) +coeff[lev]( i-1,j,k ) +coeff[lev]( i+1,j,k ) +coeff[lev]( i,j,k-1 ) +coeff[lev]( i,j,k+1 )+ 6.0*coeff[lev](  i,j,k ) ) *fac + 0.01);")), 
         Map(), "cpu")      
*/
    }

    var statdiag = new ImplExternalStatement("return entries[0]; ")
    memfuncS += new ImplFunction("diag", "T", memlist, ListBuffer(statdiag), Map(), "cpu")

    val stsizes : List[Int] = DomainKnowledge.rule_dim() match {
      case 2 => List(1, 5, 9)
      case 3 => List(1, 7, 27)
    }

    var memlistS : ListBuffer[ParameterInfo] = ListBuffer()
    memlistS += new ParameterInfo("arr", DomainKnowledge.ArrayClassName + "<T>&")
    for (i <- 1 to DomainKnowledge.rule_dim())
      memlistS += new ParameterInfo(idxdimparlist(i - 1), "int")

    for (i <- 0 to stsizes.length - 1) {

      var stat : ListBuffer[ImplStatement] = ListBuffer()
      /*
      if ( DomainKnowledge.stenciltype.equals("nonlinear")) {

        var d3dstr = ""
       if (DomainKnowledge.rule_dim==3) 
         d3dstr = ",x3"
         
        if (!DomainKnowledge.use_gpu)
          stat += new ImplExternalStatement(s"compst(${DomainKnowledge.unknown_L1(0)._1},x1,x2${d3dstr},lev);") //ImplPcall("", "compst", paramlist)
      }
      */
      stat += new ImplReturnStatement(StencilGenerator.generateStencilConvolution_plain("", stsizes(i), memlistS, ""))

      /*      val idxmap = IdxKnowledge.StencilToidx(DomainKnowledge.rule_dim(), stsizes(i))
      var exts: String = s"return entries[0]*" + memlistS(0).name + "(" + memlistS(1).name + "+" + idxmap(0)(0).toString
      for (j <- 1 to DomainKnowledge.rule_dim() - 1)
        exts = exts + "," + memlistS(j + 1).name + "+" + idxmap(0)(j).toString
      exts = exts + ")"
      for (k <- 1 to idxmap.length - 1) {
        exts = exts + " + entries[" + k + "]*" + memlistS(0).name + "(" + memlistS(1).name + "+" + idxmap(k)(0).toString
        for (j <- 1 to DomainKnowledge.rule_dim() - 1)
          exts = exts + "," + memlistS(j + 1).name + "+" + idxmap(k)(j).toString
        exts = exts + ")"
      }
      exts = exts + ";"

      var stat = new ExternalStatement(exts)
      stat.CostInfo += "*" -> idxmap.length
      stat.CostInfo += "+" -> (idxmap.length - 1)
      stat.CostInfo += "Load" -> idxmap.length
      //stat.costs("Load")
*/
      memfuncS += new ImplFunction("convolve" + stsizes(i).toString + "P", "T", memlistS, stat, Map(), "cpu")

      //      new BinaryExpr(left:ExpressionInfo, oper:OperatorInfo, right:ExpressionInfo) 
      //      new returnStatement(expr.transform(scopeparas, None, "return"))
    }

    /*    val fac = Math.pow(0.25, DomainKnowledge.rule_dim() - 1)
    var ints = DomainKnowledge.rule_dim() match {
      case 2 => "return " + fac.toString + "*(arr(" + DomainKnowledge.rule_mapfineTocoarse("x1") + "," + DomainKnowledge.rule_mapfineTocoarse("x2") + ") + arr((x1+1)/2,(x2+1)/2) + arr((x1+1)/2,x2/2) + arr(x1/2,(x2+1)/2))" + ";"
      case 3 => "return " + fac.toString + "*(arr(" + DomainKnowledge.rule_mapfineTocoarse("x1") + "," + DomainKnowledge.rule_mapfineTocoarse("x2") + "," + DomainKnowledge.rule_mapfineTocoarse("x3") + ") + arr((x1+1)/2,(x2+1)/2,x3/2) + arr((x1+1)/2,x2/2,x3/2) + arr(x1/2,(x2+1)/2,x3/2) + arr((x1+1)/2,(x2+1)/2,(x3+1)/2) + arr((x1+1)/2,x2/2,(x3+1)/2) + arr(x1/2,(x2+1)/2,(x3+1)/2) + arr(x1/2,x2/2,(x3+1)/2))" + ";"
    }
  */
    /*  var ints = generateStencilInterpolation("arr", "x")

    // val parserl4 = new ParserL4
    //  parserl4.parse(DSLl4)
    var statint = new ExternalStatement(ints)
    statint.CostInfo += "*" -> 1
    statint.CostInfo += "+" -> (Math.pow(2, DomainKnowledge.rule_dim()).toInt - 1)
    statint.CostInfo += "Load" -> (Math.pow(2, DomainKnowledge.rule_dim()).toInt)
    //statint.costs(ListBuffer(new ParameterInfo("","")))
*/
    var statint = new ImplReturnStatement(StencilGenerator.generateStencilInterpolation("", "arr", "x"))

    memfuncS += new ImplFunction("interpolate", "T", memlistS, ListBuffer(statint), Map(), "cpu")

    var stencilmemvariables : ListBuffer[ParameterInfo] = ListBuffer()
    stencilmemvariables += new ParameterInfo("entries", "std::vector<T>")
    stencilmemvariables += new ParameterInfo("size", "int")
    if (DomainKnowledge.stenciltype.equals("nonlinear")) {
      stencilmemvariables += new ParameterInfo("lev", "int")
      stencilmemvariables += new ParameterInfo("fac", s"${DomainKnowledge.plaintype}")
    }

    treel2.ExternalClasses += "Stencil" -> new ImplClass(StencilClassName, "T", stencilmemvariables, memfuncS)

    StencilClassName = DomainKnowledge.StencilClassNameVar

    var memfuncSV : ListBuffer[ImplFunction] = ListBuffer()

    var memlistSinit : ListBuffer[ParameterInfo] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      memlistSinit += new ParameterInfo(idxdimparlist(i - 1), "int")
    memlistSinit += new ParameterInfo("s", "int")

    memfuncSV += new ImplFunction(StencilClassName, "", ListBuffer(new ParameterInfo("", "")), ListBuffer(new ImplStatement()), Map(), "cpu")

    var bodylistSinit : ListBuffer[ImplStatement] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      bodylistSinit += new ImplExternalStatement(s"${idxdimmemlist(i - 1)} = ${idxdimparlist(i - 1)};")
    bodylistSinit += new ImplExternalStatement(s"size = s;entries.resize(${sizeidx},s); ")

    memfuncSV += new ImplFunction(StencilClassName, "", memlistSinit, bodylistSinit, Map(), "cpu")
    memfuncSV += new ImplFunction("resize", "void", memlistSinit, bodylistSinit, Map(), "cpu")

    var memlistSset : ListBuffer[ParameterInfo] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      memlistSset += new ParameterInfo(idxdimparlist(i - 1), "int")
    memlistSset += new ParameterInfo("idx", "int")
    memlistSset += new ParameterInfo("v", DomainKnowledge.transform_datatype_cpp(DomainKnowledge.globaldatatype_L2))

    var hstr = s"if ((${idxdimparlist(0)} > 0) && (${idxdimparlist(0)} < ${idxdimmemlist(0)})"
    for (i <- 1 to DomainKnowledge.rule_dim() - 1)
      hstr += s"&& (${idxdimparlist(i)} > 0) && (${idxdimparlist(i)} < ${idxdimmemlist(i)})"
    hstr += ")\n"

    memfuncSV += new ImplFunction("set", "void", memlistSset, ListBuffer(new ImplExternalStatement(s"entries(${idxlin},idx) = v; ")), Map(), "cpu")
    memfuncSV += new ImplFunction("add", "void", memlistSset, ListBuffer(new ImplExternalStatement(s"${hstr} entries(${idxlin},idx) += v; ")), Map(), "cpu")

    var statdiagV = new ImplExternalStatement(s"return entries(${idxlin},0); ")
    memfuncSV += new ImplFunction("diag", "T", memlist, ListBuffer(statdiagV), Map(), "cpu")

    if (DomainKnowledge.rule_dim == 2)
      memfuncSV += new ImplFunction("plot", "void", ListBuffer(new ParameterInfo("fname", "string")),
        ListBuffer(new ImplExternalStatement("ofstream os(fname.c_str());\n for (int i=0;i < x1_; i++) \n for (int j=0;j < x2_; j++) { \n  os << i << \" \" << j << \" \"; \n for (int s=0;s < size; s++) \n os << entries(i*x2_ + j,s) << \" \"; \n os << endl;}")), Map(), "cpu")
    else if (DomainKnowledge.rule_dim == 3)
      memfuncSV += new ImplFunction("plot", "void", ListBuffer(new ParameterInfo("fname", "string")),
        ListBuffer(new ImplExternalStatement("ofstream os(fname.c_str());\n for (int i=0;i < x1_; i++) \n for (int j=0;j < x2_; j++) \n for (int k=0;k < x3_; k++) { \n  os << i << \" \" << j << \" \" << k << \" \"; \n for (int s=0;s < size; s++) \n os << entries(i*x2_*x3_ + j*x3_ + k,s) << \" \"; \n os << endl;}")), Map(), "cpu")

    for (i <- 0 to stsizes.length - 1) {

      var stat = new ImplReturnStatement(StencilGenerator.generateStencilConvolution_plain("", stsizes(i), memlistS, idxlin))

      /*      val idxmap = IdxKnowledge.StencilToidx(DomainKnowledge.rule_dim(), stsizes(i))
      var exts: String = s"return entries(${idxlin},0)*" + memlistS(0).name + "(" + memlistS(1).name + "+" + idxmap(0)(0).toString
      for (j <- 1 to DomainKnowledge.rule_dim() - 1)
        exts = exts + "," + memlistS(j + 1).name + "+" + idxmap(0)(j).toString
      exts = exts + ")"
      for (k <- 1 to idxmap.length - 1) {
        exts = exts + s" + entries(${idxlin},${k})*${memlistS(0).name}(${memlistS(1).name}+${idxmap(k)(0).toString}"
        for (j <- 1 to DomainKnowledge.rule_dim() - 1)
          exts = exts + "," + memlistS(j + 1).name + "+" + idxmap(k)(j).toString
        exts = exts + ")"
      }
      exts = exts + ";"

      var stat = new ExternalStatement(exts)
      stat.CostInfo += "*" -> idxmap.length
      stat.CostInfo += "+" -> (idxmap.length - 1)
      stat.CostInfo += "Load" -> idxmap.length
      //    stat.costs(ListBuffer(new ParameterInfo("","")))
*/
      memfuncSV += new ImplFunction("convolve" + stsizes(i).toString + "P", "T", memlistS, ListBuffer(stat), Map(), "cpu")
    }

    var cmemlistS : ListBuffer[ParameterInfo] = ListBuffer()

    for (i <- 1 to DomainKnowledge.rule_dim()) {
      cmemlistS += new ParameterInfo(idxdimmemlist(i - 1), "int")
    }
    cmemlistS += new ParameterInfo("entries", s"${DomainKnowledge.MatrixClassName}" + "<T>")
    cmemlistS += new ParameterInfo("size", "int")

    treel2.ExternalClasses += "StencilVar" -> new ImplClass(StencilClassName, "T",
      cmemlistS, memfuncSV)

  }
}

object StencilGenerator {

  def generateStencilInterpolation(classname : String, arrname : String, idxname : String, intorder : Int = 2) : ImplExpression = {

    val idxmap = IdxKnowledge.IntStencilToidx(DomainKnowledge.rule_dim(), Math.pow(2, DomainKnowledge.rule_dim()).toInt)
    var fac = Math.pow(0.25, DomainKnowledge.rule_dim() - 1)
    var cstr = ""
    if (!classname.equals(""))
      cstr = classname + "."

    if (intorder == 1) {
      fac = 1
    }

    var exts : String = ""
    if (fac == 1)
      exts = s"(${cstr}${arrname}(" + DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(0)(0).toString, 0)
    else
      exts = fac.toString + s"*(${cstr}${arrname}(" + DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(0)(0).toString, 0)

    for (j <- 1 to DomainKnowledge.rule_dim() - 1)
      exts += "," + DomainKnowledge.rule_mapfineTocoarse(s"${idxname}${j + 1}+" + idxmap(0)(j).toString, j)
    exts += ")"

    if (intorder == 2) {

      for (k <- 1 to idxmap.length - 1) {
        exts += s"+ ${arrname}(" + DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(k)(0).toString, 0)
        for (j <- 1 to DomainKnowledge.rule_dim() - 1)
          exts += "," + DomainKnowledge.rule_mapfineTocoarse(s"${idxname}${j + 1}+" + idxmap(k)(j).toString, j)
        exts += ")"
      }
    }
    exts += ")"

    var statint = new ImplValueExpr[String](exts)

    if (intorder == 2) {
      statint.CostInfo += "*" -> Map(1 -> "")
      statint.CostInfo += "+" -> Map((Math.pow(2, DomainKnowledge.rule_dim()).toInt - 1) -> "")
      statint.CostInfo += "Load" -> Map((Math.pow(2, DomainKnowledge.rule_dim()).toInt) -> s"${arrname}")
    } else {
      statint.CostInfo += "Load" -> Map(1 -> s"${arrname}")
    }
    return statint
  }

  def generateStencilInterpolationcuda(arrname : String, idxname : String) : ImplExpression = {

    val idxmap = IdxKnowledge.IntStencilToidx(DomainKnowledge.rule_dim(), Math.pow(2, DomainKnowledge.rule_dim()).toInt)
    val fac = Math.pow(0.5, DomainKnowledge.rule_dim())

    var exts : String = ""

    if (DomainKnowledge.stenciltype.equals("nonlinear")) {

      exts = s"${arrname}["
      if (DomainKnowledge.rule_dim == 2)
        exts += IdxKnowledge.mapidxToLinear(ListBuffer(DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(0)(0).toString, 0),
          DomainKnowledge.rule_mapfineTocoarse(s"${idxname}2+" + idxmap(0)(1).toString, 1)), ListBuffer("s1_1", "s2_1"))
      else if (DomainKnowledge.rule_dim == 3)
        exts += IdxKnowledge.mapidxToLinear(ListBuffer(DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(0)(0).toString, 0),
          DomainKnowledge.rule_mapfineTocoarse(s"${idxname}2+" + idxmap(0)(1).toString, 1), DomainKnowledge.rule_mapfineTocoarse(s"${idxname}3+" + idxmap(0)(2).toString, 2)),
          ListBuffer("s1_1", "s2_1", "s3_1"))
      exts += "]"

    } else {

      exts = fac.toString + s"*(${arrname}["
      if (DomainKnowledge.rule_dim == 2)
        exts += IdxKnowledge.mapidxToLinear(ListBuffer(DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(0)(0).toString, 0),
          DomainKnowledge.rule_mapfineTocoarse(s"${idxname}2+" + idxmap(0)(1).toString, 1)), ListBuffer("s1_1", "s2_1"))
      else if (DomainKnowledge.rule_dim == 3)
        exts += IdxKnowledge.mapidxToLinear(ListBuffer(DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(0)(0).toString, 0),
          DomainKnowledge.rule_mapfineTocoarse(s"${idxname}2+" + idxmap(0)(1).toString, 1), DomainKnowledge.rule_mapfineTocoarse(s"${idxname}3+" + idxmap(0)(2).toString, 2)),
          ListBuffer("s1_1", "s2_1", "s3_1"))
      exts += "]"

      for (k <- 1 to idxmap.length - 1) {
        if (DomainKnowledge.rule_dim == 2)
          exts += s"+ ${arrname}[" + IdxKnowledge.mapidxToLinear(ListBuffer(DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(k)(0).toString, 0),
            DomainKnowledge.rule_mapfineTocoarse(s"${idxname}2+" + idxmap(k)(1).toString, 1)), ListBuffer("s1_1", "s2_1")) + "]"
        else if (DomainKnowledge.rule_dim == 3)
          exts += s"+ ${arrname}[" + IdxKnowledge.mapidxToLinear(ListBuffer(DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(k)(0).toString, 0),
            DomainKnowledge.rule_mapfineTocoarse(s"${idxname}2+" + idxmap(k)(1).toString, 1), DomainKnowledge.rule_mapfineTocoarse(s"${idxname}3+" + idxmap(k)(2).toString, 2)),
            ListBuffer("s1_1", "s2_1", "s3_1")) + "]"
      }
      exts += ")"
    }

    var statint = new ImplValueExpr[String](exts)
    statint.CostInfo += "*" -> Map(1 -> "")
    statint.CostInfo += "+" -> Map((Math.pow(2, DomainKnowledge.rule_dim()).toInt - 1) -> "")
    statint.CostInfo += "Load" -> Map((Math.pow(2, DomainKnowledge.rule_dim()).toInt) -> "")

    return statint
  }

  def generateStencilRestrictioncuda(arrname : String, idxname : String) : ImplExpression = {

    val idxmap = IdxKnowledge.IntStencilToidx(DomainKnowledge.rule_dim(), Math.pow(2, DomainKnowledge.rule_dim()).toInt)
    val fac = Math.pow(0.5, DomainKnowledge.rule_dim())

    var exts : String = fac.toString + s"*(${arrname}["
    if (DomainKnowledge.rule_dim == 2)
      exts += IdxKnowledge.mapidxToLinear(ListBuffer("(" + DomainKnowledge.rule_mapcoarseTofine(s"${idxname}1", 0) + s"+${idxmap(0)(0)})",
        "(" + DomainKnowledge.rule_mapcoarseTofine(s"${idxname}2", 1) + s"+${idxmap(0)(1)})"), ListBuffer("s1_1", "s2_1"))
    else if (DomainKnowledge.rule_dim == 3)
      exts += IdxKnowledge.mapidxToLinear(ListBuffer("(" + DomainKnowledge.rule_mapcoarseTofine(s"${idxname}1", 0) + s"+${idxmap(0)(0)})",
        "(" + DomainKnowledge.rule_mapcoarseTofine(s"${idxname}2", 1) + s"+${idxmap(0)(1)})", "(" + DomainKnowledge.rule_mapcoarseTofine(s"${idxname}3", 2) + s"+${idxmap(0)(2)})"),
        ListBuffer("s1_1", "s2_1", "s3_1"))
    exts += "]"

    for (k <- 1 to idxmap.length - 1) {
      if (DomainKnowledge.rule_dim == 2)
        exts += s"+ ${arrname}[" + IdxKnowledge.mapidxToLinear(ListBuffer("(" + DomainKnowledge.rule_mapcoarseTofine(s"${idxname}1", 0) + s"+${idxmap(k)(0)})",
          "(" + DomainKnowledge.rule_mapcoarseTofine(s"${idxname}2", 1) + s"+${idxmap(k)(1)})"), ListBuffer("s1_1", "s2_1")) + "]"
      else if (DomainKnowledge.rule_dim == 3)
        exts += s"+ ${arrname}[" + IdxKnowledge.mapidxToLinear(ListBuffer("(" + DomainKnowledge.rule_mapcoarseTofine(s"${idxname}1", 0) + s"+${idxmap(k)(0)})",
          "(" + DomainKnowledge.rule_mapcoarseTofine(s"${idxname}2", 1) + s"+${idxmap(k)(1)})", "(" + DomainKnowledge.rule_mapcoarseTofine(s"${idxname}3", 2) + s"+${idxmap(k)(2)})"),
          ListBuffer("s1_1", "s2_1", "s3_1")) + "]"
    }
    exts += ")"

    var statint = new ImplValueExpr[String](exts)
    statint.CostInfo += "*" -> Map(1 -> "")
    statint.CostInfo += "+" -> Map((Math.pow(2, DomainKnowledge.rule_dim()).toInt - 1) -> "")
    statint.CostInfo += "Load" -> Map((Math.pow(2, DomainKnowledge.rule_dim()).toInt) -> "")

    return statint
  }

  def generateStencilConvolution(classname : String, stencil : ImplStencil, memlistS : ListBuffer[ParameterInfo], idxlin : String) : ImplExpression = {

    val stsize = stencil.length(0)
    val idxmap = IdxKnowledge.StencilToidx(DomainKnowledge.rule_dim(), stsize)
    var exts : String = ""
    var cstr = ""
    if (!classname.equals(""))
      cstr = classname + "."

    if (idxlin.equals(""))
      exts = s"(${cstr}entries[0]*" + memlistS(0).name + "(" + memlistS(1).name + "+" + idxmap(0)(0).toString
    else
      exts = s"(${cstr}entries(${idxlin},0)*" + memlistS(0).name + "(" + memlistS(1).name + "+" + idxmap(0)(0).toString

    for (j <- 1 to DomainKnowledge.rule_dim() - 1)
      exts = exts + "," + memlistS(j + 1).name + "+" + idxmap(0)(j).toString
    exts = exts + ")"
    for (k <- 1 to idxmap.length - 1) {
      if (idxlin.equals(""))
        exts = exts + s" + ${cstr}entries[" + k + "]*" + memlistS(0).name + "(" + memlistS(1).name + "+" + idxmap(k)(0).toString
      else
        exts = exts + s" + ${cstr}entries(${idxlin},${k})*${memlistS(0).name}(${memlistS(1).name}+${idxmap(k)(0).toString}"

      for (j <- 1 to DomainKnowledge.rule_dim() - 1)
        exts = exts + "," + memlistS(j + 1).name + "+" + idxmap(k)(j).toString
      exts = exts + ")"
    }
    exts = exts + ")"

    var stat = new ImplValueExpr[String](exts)
    stat.CostInfo += "*" -> Map(idxmap.length -> "")
    stat.CostInfo += "+" -> Map((idxmap.length - 1) -> "")
    stat.CostInfo += "Load" -> Map(idxmap.length -> s"conv_${memlistS(0).name}")

    return stat
  }

  def generateStencilConvolution_plain(classname : String, stsize : Int, memlistS : ListBuffer[ParameterInfo], idxlin : String) : ImplExpression = {

    val idxmap = IdxKnowledge.StencilToidx(DomainKnowledge.rule_dim(), stsize)
    var exts : String = ""
    var cstr = ""
    if (!classname.equals(""))
      cstr = classname + "."

    if (idxlin.equals(""))
      exts = s"(${cstr}entries[0]*" + memlistS(0).name + "(" + memlistS(1).name + "+" + idxmap(0)(0).toString
    else
      exts = s"(${cstr}entries(${idxlin},0)*" + memlistS(0).name + "(" + memlistS(1).name + "+" + idxmap(0)(0).toString

    for (j <- 1 to DomainKnowledge.rule_dim() - 1)
      exts = exts + "," + memlistS(j + 1).name + "+" + idxmap(0)(j).toString
    exts = exts + ")"
    for (k <- 1 to idxmap.length - 1) {
      if (idxlin.equals(""))
        exts = exts + s" + ${cstr}entries[" + k + "]*" + memlistS(0).name + "(" + memlistS(1).name + "+" + idxmap(k)(0).toString
      else
        exts = exts + s" + ${cstr}entries(${idxlin},${k})*${memlistS(0).name}(${memlistS(1).name}+${idxmap(k)(0).toString}"

      for (j <- 1 to DomainKnowledge.rule_dim() - 1)
        exts = exts + "," + memlistS(j + 1).name + "+" + idxmap(k)(j).toString
      exts = exts + ")"
    }
    exts = exts + ")"

    var stat = new ImplValueExpr[String](exts)
    stat.CostInfo += "*" -> Map(idxmap.length -> "")
    stat.CostInfo += "+" -> Map((idxmap.length - 1) -> "")
    stat.CostInfo += "Load" -> Map(idxmap.length -> s"conv_${memlistS(0).name}")

    return stat
  }

  def generateStencilConvolutioncuda(stsize : Int, curStencil : ImplStencil, arrname : String, arrsizeidx : Int, idxlin : String, gidx : String, compstr : String, factor : String = "") : ImplExpression = {

    val idxmap = IdxKnowledge.StencilToidx(DomainKnowledge.rule_dim(), stsize)
    var exts : String = ""
    var facstr = ""
    if (!factor.equals(""))
      facstr = factor + "*"

    // println("now in" +curStencil + " " + curStencil.sizex)

    if (DomainKnowledge.stenciltype.equals("nonlinear")) {

      if (curStencil.sizex == 1)
        exts = s"${facstr}(mult${compstr}(localst[0],${arrname}0[${gidx}],${arrname}1[${gidx}])"

      for (k <- 1 to idxmap.length - 1) {

        exts += s" + mult${compstr}(localst[${k}],"

        if (DomainKnowledge.rule_dim == 2) {
          exts += s"${arrname}0[${gidx}+s2*(${idxmap(k)(0).toString})"
          for (j <- 1 to DomainKnowledge.rule_dim() - 1)
            exts += "+" + idxmap(k)(j).toString
          exts += "],"
          exts += s"${arrname}1[${gidx}+s2*(${idxmap(k)(0).toString})"
          for (j <- 1 to DomainKnowledge.rule_dim() - 1)
            exts += "+" + idxmap(k)(j).toString
          exts += "])"
        } else if (DomainKnowledge.rule_dim == 3) {
          exts += s"${arrname}0[${gidx}+s2*s3*(${idxmap(k)(0).toString})"
          exts += s"+s3*(${idxmap(k)(1).toString})"
          exts += "+" + idxmap(k)(2).toString
          exts += "],"
          exts += s"${arrname}1[${gidx}+s2*s3*(${idxmap(k)(0).toString})"
          exts += s"+s3*(${idxmap(k)(1).toString})"
          exts += "+" + idxmap(k)(2).toString
          exts += "])"
        }

      }
      exts += ")"
    } else {

      if (curStencil.sizex == 1)
        exts = s"${facstr}((${curStencil.entries(0)(0)})*" + arrname + s"[${gidx}]"
      //       else 
      //        exts = s"${stname}[0]*" + arrname + s"[${gidx}]"

      for (k <- 1 to idxmap.length - 1) {
        if (DomainKnowledge.rule_dim == 2) {
          if (arrsizeidx == 0)
            exts = exts + s" + (${curStencil.entries(0)(k)})*" + arrname + s"[${gidx}+s2*(" + idxmap(k)(0).toString + ")"
          else
            exts = exts + s" + (${curStencil.entries(0)(k)})*" + arrname + s"[${gidx}+s2_${arrsizeidx}*(" + idxmap(k)(0).toString + ")"
        } else if (DomainKnowledge.rule_dim == 3) {
          if (arrsizeidx == 0)
            exts = exts + s" + (${curStencil.entries(0)(k)})*" + arrname + s"[${gidx}+s2*s3*(" + idxmap(k)(0).toString + ")"
          else
            exts = exts + s" + (${curStencil.entries(0)(k)})*" + arrname + s"[${gidx}+s2_${arrsizeidx}*s3_${arrsizeidx}*(" + idxmap(k)(0).toString + ")"
        }

        if (DomainKnowledge.rule_dim == 2) {
          for (j <- 1 to DomainKnowledge.rule_dim() - 1)
            exts = exts + "+" + idxmap(k)(j).toString
          exts = exts + "]"
        } else if (DomainKnowledge.rule_dim == 3) {
          exts = exts + "+ s3*(" + idxmap(k)(1).toString + ")"
          exts += "+" + idxmap(k)(2).toString
          exts = exts + "]"
        }

      }
      exts = exts + ")"
    }

    var reqstr = ""
    if (DomainKnowledge.stenciltype.equals("nonlinear"))
      reqstr = "compst"

    var stat = new ImplValueExpr[String]("(" + exts + ")", reqstr)
    stat.CostInfo += "*" -> Map(idxmap.length -> "")
    stat.CostInfo += "+" -> Map((idxmap.length - 1) -> "")
    stat.CostInfo += "Load" -> Map(idxmap.length -> "")

    return stat
  }

}