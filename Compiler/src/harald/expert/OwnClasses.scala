package harald.expert

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import harald.Impl._
import harald.dsl._
import harald.ast._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

class DataClasses(treel2 : TreeL2) {

  def initextClasses() {

    initArrayClass()
    initStencilClass()
  }

  def initArrayClass() {
    // Array Class on CPU

    var ArrayClassName = "MyArray"

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

    memfunc += new ImplFunction(ArrayClassName, "", ListBuffer(new ParameterInfo("", "")), new ListBuffer, Map(), "cpu")

    var memlist : ListBuffer[ParameterInfo] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      memlist += new ParameterInfo(idxdimparlist(i - 1), "int")
    var bodylist : ListBuffer[Statement] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      bodylist += new StringLiteral(s"${idxdimmemlist(i - 1)} = ${idxdimparlist(i - 1)};")
    bodylist += new StringLiteral(s"a = new T[${sizeidx}];")

    memfunc += new ImplFunction(ArrayClassName, "", memlist, bodylist, Map(), "cpu")
    memfunc += new ImplFunction("resize", "void", memlist, bodylist, Map(), "cpu")
    memfunc += new ImplFunction("begin", "T*", new ListBuffer(), ListBuffer(new StringLiteral("return &a[0];")), Map(), "cpu")
    for (i <- 1 to DomainKnowledge.rule_dim())
      memfunc += new ImplFunction("s" + i.toString, "int", ListBuffer(new ParameterInfo("", "")), ListBuffer(new StringLiteral(s"return ${idxdimmemlist(i - 1)};")), Map(), "cpu")

    var idxlin : String = IdxKnowledge.mapidxToLinear(idxdimparlist, idxdimmemlist)
    memfunc += new ImplFunction("operator()", "T&", memlist, ListBuffer(new StringLiteral("return a[" + idxlin + "]; ")), Map(), "cpu")

    treel2.ExternalClasses += "Array" -> new ImplClass(ArrayClassName, "T",
      cmemlist,
      memfunc)

    // Array Class on GPU
    ArrayClassName = "MyArrayCuda"

    var memfunccuda : ListBuffer[ImplFunction] = ListBuffer()

    memfunccuda += new ImplFunction(ArrayClassName, "", ListBuffer(new ParameterInfo("", "")), new ListBuffer, Map(), "cpu")

    var memlistcuda : ListBuffer[ParameterInfo] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      memlistcuda += new ParameterInfo(idxdimparlist(i - 1), "int")
    var bodylistcuda : ListBuffer[Statement] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      bodylistcuda += new StringLiteral(s"${idxdimmemlist(i - 1)} = ${idxdimparlist(i - 1)};")
    bodylistcuda += new StringLiteral(s"cudaMalloc ( ( void ** ) &a, sizeof (T) * ${sizeidx});")

    memfunccuda += new ImplFunction(ArrayClassName, "", memlist, bodylistcuda, Map(), "cpu")
    memfunccuda += new ImplFunction("begin", "T*", new ListBuffer(), ListBuffer(new StringLiteral("return &a[0];")), Map(), "cpu")
    memfunccuda += new ImplFunction("resize", "void", memlist, bodylistcuda, Map(), "cpu")

    if (DomainKnowledge.use_gpu) {
      treel2.ExternalClasses += "ArrayCuda" -> new ImplClass(ArrayClassName, "T",
        cmemlist,
        memfunccuda)
    }
  }

  def initStencilClass() {

    // Const Stencil Class on CPU
    var StencilClassName = "MyStencil"

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

    memfuncS += new ImplFunction(StencilClassName, "", ListBuffer(new ParameterInfo("", "")), new ListBuffer, Map(), "cpu")
    memfuncS += new ImplFunction(StencilClassName, "", ListBuffer(new ParameterInfo("s", "int")), ListBuffer(new StringLiteral("size = s;entries.resize(s); ")), Map(), "cpu")
    memfuncS += new ImplFunction("resize", "void", ListBuffer(new ParameterInfo("s", "int")), ListBuffer(new StringLiteral("size = s;entries.resize(s); ")), Map(), "cpu")
    memfuncS += new ImplFunction("set", "void", ListBuffer(new ParameterInfo("idx", "int"), new ParameterInfo("v", DomainKnowledge.datatype_L2.getOrElse("double"))), ListBuffer(new StringLiteral("entries[idx] = v; ")), Map(), "cpu")
    memfuncS += new ImplFunction("begin", "T*", new ListBuffer(), ListBuffer(new StringLiteral("return &entries[0];")), Map(), "cpu")

    var statdiag = new StringLiteral("return entries[0]; ")
    memfuncS += new ImplFunction("diag", "T", memlist, ListBuffer(statdiag), Map(), "cpu")

    val stsizes : List[Int] = DomainKnowledge.rule_dim() match {
      case 2 => List(1, 5, 9)
      case 3 => List(1, 7, 27)
    }

    var memlistS : ListBuffer[ParameterInfo] = ListBuffer()
    // COMM_HACK
    memlistS += new ParameterInfo("arr", "Container&")
    //memlistS += new ParameterInfo("arr", treel2.ExternalClasses.get("Array").get.name + "<T>&")
    for (i <- 1 to DomainKnowledge.rule_dim())
      memlistS += new ParameterInfo(idxdimparlist(i - 1), "int")

    for (i <- 0 to stsizes.length - 1) {

      var stat = new ReturnStatement(StencilGenerator.generateStencilConvolution("", stsizes(i), memlistS, ""))

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
      memfuncS += new ImplFunction("convolve" + stsizes(i).toString + "P", "T", memlistS, ListBuffer(stat), Map(), "cpu")

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
    var statint = new ReturnStatement(StencilGenerator.generateStencilInterpolation("", "arr", "x"))

    memfuncS += new ImplFunction("interpolate", "T", memlistS, ListBuffer(statint), Map(), "cpu")

    treel2.ExternalClasses += "Stencil" -> new ImplClass(StencilClassName, "T",
      ListBuffer(new ParameterInfo("entries", "std::vector<T>"),
        new ParameterInfo("size", "int")),
      memfuncS)

    StencilClassName = "MyStencilVar"
    var memfuncSV : ListBuffer[ImplFunction] = ListBuffer()

    var memlistSinit : ListBuffer[ParameterInfo] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      memlistSinit += new ParameterInfo(idxdimparlist(i - 1), "int")
    memlistSinit += new ParameterInfo("s", "int")

    memfuncSV += new ImplFunction(StencilClassName, "", ListBuffer(new ParameterInfo("", "")), new ListBuffer, Map(), "cpu")

    var bodylistSinit : ListBuffer[Statement] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      bodylistSinit += new StringLiteral(s"${idxdimmemlist(i - 1)} = ${idxdimparlist(i - 1)};")
    bodylistSinit += new StringLiteral(s"size = s;entries.resize(${sizeidx},s); ")

    memfuncSV += new ImplFunction(StencilClassName, "", memlistSinit, bodylistSinit, Map(), "cpu")
    memfuncSV += new ImplFunction("resize", "void", memlistSinit, bodylistSinit, Map(), "cpu")

    var memlistSset : ListBuffer[ParameterInfo] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      memlistSset += new ParameterInfo(idxdimparlist(i - 1), "int")
    memlistSset += new ParameterInfo("idx", "int")
    memlistSset += new ParameterInfo("v", DomainKnowledge.datatype_L2.getOrElse("double"))

    memfuncSV += new ImplFunction("set", "void", memlistSset, ListBuffer(new StringLiteral(s"entries(${idxlin},idx) = v; ")), Map(), "cpu")
    memfuncSV += new ImplFunction("add", "void", memlistSset, ListBuffer(new StringLiteral(s"entries(${idxlin},idx) += v; ")), Map(), "cpu")

    var statdiagV = new StringLiteral(s"return entries(${idxlin},0); ")
    memfuncSV += new ImplFunction("diag", "T", memlist, ListBuffer(statdiagV), Map(), "cpu")

    for (i <- 0 to stsizes.length - 1) {

      var stat = new ReturnStatement(StencilGenerator.generateStencilConvolution("", stsizes(i), memlistS, idxlin))

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
    cmemlistS += new ParameterInfo("entries", s"${treel2.ExternalClasses.get("Array").get.name}" + "<T>")
    cmemlistS += new ParameterInfo("size", "int")

    treel2.ExternalClasses += "StencilVar" -> new ImplClass(StencilClassName, "T",
      cmemlistS, memfuncSV)

  }
}

object StencilGenerator {

  def generateStencilInterpolation(classname : String, arrname : String, idxname : String) : Expression = {

    val idxmap = IdxKnowledge.IntStencilToidx(DomainKnowledge.rule_dim(), Math.pow(2, DomainKnowledge.rule_dim()).toInt)
    val fac = Math.pow(0.25, DomainKnowledge.rule_dim() - 1)
    var cstr = ""
    if (!classname.equals(""))
      cstr = classname + "."

    //var exts : String = fac.toString + s"*(${cstr}${arrname}(" + DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(0)(0).toString)
    var exts : String = fac.toString + s"*(${cstr}${arrname}(" + DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(0)(0).toString + " + 1")
    for (j <- 1 to DomainKnowledge.rule_dim() - 1)
      //exts += "," + DomainKnowledge.rule_mapfineTocoarse(s"${idxname}${j + 1}+" + idxmap(0)(j).toString)
      exts += "," + DomainKnowledge.rule_mapfineTocoarse(s"${idxname}${j + 1}+" + idxmap(0)(j).toString + " + 1")
    exts += ")"

    for (k <- 1 to idxmap.length - 1) {
      //exts += s"+ ${arrname}(" + DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(k)(0).toString)
      exts += s"+ ${arrname}(" + DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(k)(0).toString + " + 1")
      for (j <- 1 to DomainKnowledge.rule_dim() - 1)
        //exts += "," + DomainKnowledge.rule_mapfineTocoarse(s"${idxname}${j + 1}+" + idxmap(k)(j).toString)
        exts += "," + DomainKnowledge.rule_mapfineTocoarse(s"${idxname}${j + 1}+" + idxmap(k)(j).toString + " + 1")
      exts += ")"
    }
    exts += ")"

    var statint = new StringLiteral(exts)

    return statint
  }

  def generateStencilInterpolationcuda(arrname : String, idxname : String) : Expression = {

    val idxmap = IdxKnowledge.IntStencilToidx(DomainKnowledge.rule_dim(), Math.pow(2, DomainKnowledge.rule_dim()).toInt)
    val fac = Math.pow(0.25, DomainKnowledge.rule_dim() - 1)

    var exts : String = fac.toString + s"*(${arrname}["
    exts += IdxKnowledge.mapidxToLinear(ListBuffer(DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(0)(0).toString),
      DomainKnowledge.rule_mapfineTocoarse(s"${idxname}2+" + idxmap(0)(1).toString)), ListBuffer("s1_1", "s2_1"))
    exts += "]"

    for (k <- 1 to idxmap.length - 1) {
      exts += s"+ ${arrname}[" + IdxKnowledge.mapidxToLinear(ListBuffer(DomainKnowledge.rule_mapfineTocoarse(s"${idxname}1+" + idxmap(k)(0).toString),
        DomainKnowledge.rule_mapfineTocoarse(s"${idxname}2+" + idxmap(k)(1).toString)), ListBuffer("s1_1", "s2_1")) + "]"
    }
    exts += ")"

    var statint = new StringLiteral(exts)

    return statint
  }

  def generateStencilConvolution(classname : String, stsize : Int, memlistS : ListBuffer[ParameterInfo], idxlin : String) : Expression = {

    val idxmap = IdxKnowledge.StencilToidx(DomainKnowledge.rule_dim(), stsize)
    var exts : String = ""
    var cstr = ""
    if (!classname.equals(""))
      cstr = classname + "."

    var fieldName = memlistS(0).name;
    // COMM_HACK
    fieldName = fieldName match {
      case "solution[lev]" => "curFragment.solData[0][lev]->getDataRef"
      case "Res[lev]"      => "curFragment.resData[0][lev]->getDataRef"
      case "f[lev]"        => "curFragment.rhsData[0][lev]->getDataRef"
      case s : String      => s
    }

    if (idxlin.equals(""))
      exts = s"(${cstr}entries[0]*" + fieldName + "(" + memlistS(1).name + "+" + idxmap(0)(0).toString
    else
      exts = s"(${cstr}entries(${idxlin},0)*" + fieldName + "(" + memlistS(1).name + "+" + idxmap(0)(0).toString

    for (j <- 1 to DomainKnowledge.rule_dim() - 1)
      exts = exts + "," + memlistS(j + 1).name + "+" + idxmap(0)(j).toString
    exts = exts + ")"
    for (k <- 1 to idxmap.length - 1) {
      if (idxlin.equals(""))
        exts = exts + s" + ${cstr}entries[" + k + "]*" + fieldName + "(" + memlistS(1).name + "+" + idxmap(k)(0).toString
      else
        exts = exts + s" + ${cstr}entries(${idxlin},${k})*${fieldName}(${memlistS(1).name}+${idxmap(k)(0).toString}"

      for (j <- 1 to DomainKnowledge.rule_dim() - 1)
        exts = exts + "," + memlistS(j + 1).name + "+" + idxmap(k)(j).toString
      exts = exts + ")"
    }
    exts = exts + ")"

    var stat = new StringLiteral(exts)

    return stat
  }

  def generateStencilConvolutioncuda(stsize : Int, curStencil : ImplStencil, arrname : String, arrsizeidx : Int, idxlin : String, gidx : String) : Expression = {

    val idxmap = IdxKnowledge.StencilToidx(DomainKnowledge.rule_dim(), stsize)
    var exts : String = ""

    // println("now in" +curStencil + " " + curStencil.sizex)

    if (curStencil.sizex == 1)
      exts = s"(${curStencil.entries(0)})*" + arrname + s"[${gidx}]"
    //       else 
    //        exts = s"${stname}[0]*" + arrname + s"[${gidx}]"

    for (k <- 1 to idxmap.length - 1) {
      if (arrsizeidx == 0)
        exts = exts + s" + (${curStencil.entries(k)})*" + arrname + s"[${gidx}+s2*(" + idxmap(k)(0).toString + ")"
      else
        exts = exts + s" + (${curStencil.entries(k)})*" + arrname + s"[${gidx}+s2_${arrsizeidx}*(" + idxmap(k)(0).toString + ")"

      for (j <- 1 to DomainKnowledge.rule_dim() - 1)
        exts = exts + "+" + idxmap(k)(j).toString
      exts = exts + "]"
    }

    var stat = new StringLiteral("(" + exts + ")")

    return stat
  }

}