package harald.Impl

import scala.collection.mutable.ListBuffer
import harald.dsl.ParameterInfo

class ImplFunction(fname : String, rettype : String, paramlist : ListBuffer[ParameterInfo], bodylist : ListBuffer[ImplStatement], bodyvar : Map[String, Int], loc : String) {
  var name : String = fname
  var returntype : String = rettype
  var params : ListBuffer[ParameterInfo] = paramlist
  var body : ListBuffer[ImplStatement] = bodylist
  var location = loc
  var bodyvariables = bodyvar

  def toString_cpp_signature : String = {

    var sp : String = ""
    for (b <- params)
      if (b.equals(params.head))
        sp += b.toString_cpp
      else
        sp += "," + b.toString_cpp

    if (location.equals("cpu"))
      return rettype + " " + fname + " ( " + sp + " );\n"
    else
      return "__global__ " + rettype + " " + fname + " ( " + sp + " );\n"
  }

  def toString_cpp : String = {
    var s : String = ""

    //  if (fname.equals("GaussSeidelcuda"))
    //     println(body)

    if (location.equals("cpu"))
      for (b <- body)
        s += b.toString_cpp
    else
      for (b <- body)
        s += b.toString_cuda

    var sp : String = ""
    for (b <- params)
      if (b.equals(params.head))
        sp += b.toString_cpp
      else
        sp += "," + b.toString_cpp

    if (location.equals("cpu"))
      return rettype + " " + fname + " ( " + sp + " ) " + " { \n" + s + " } \n"
    else
      return "__global__ " + rettype + " " + fname + " ( " + sp + " ) " + " { \n" + s + " } \n"
    //        return "def " + fname + " ( " + sp + " ) : " + rettype + " = { \n" + s + " } \n"
  }

  def toString_cuda : String = {
    var s : String = ""

    for (b <- body)
      s += b.toString_cuda

    var sp : String = ""
    for (b <- params)
      if (b.equals(params.head))
        sp += b.toString_cpp
      else
        sp += "," + b.toString_cpp

    return "__device__ " + rettype + " " + fname + " ( " + sp + " ) " + " { \n" + s + " } \n"
  }

  def calls(lev : Int) = {
    //  println("call " + fname + " " + lev)
    for (b <- bodylist)
      b.calls(lev)
  }

  def costs(para : String) : Map[Int, String] = {
    //  println("costs: " + fname + "\n")
    //   for (p <- para)
    //     println(p)

    var m : Map[Int, String] = Map()
    for (b <- bodylist) {
      m ++= b.costs(para)
      // println(b.CostInfo)      
    }
    return m
  }

  def toString_cpp_body : String = {
    var s : String = ""

    if (location.equals("cpu"))
      for (b <- body)
        s += b.toString_cpp
    else
      for (b <- body)
        s += b.toString_cuda

    return " \n" + s + " \n"
  }

}
