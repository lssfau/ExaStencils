package exastencils.deprecated.harald.Impl

import scala.collection.mutable.ListBuffer
import exastencils.deprecated.harald.dsl.ParameterInfo

  class ImplClass(cname: String, templ: String, memlist: ListBuffer[ParameterInfo], memfunc: ListBuffer[ImplFunction]) {
    var name: String = cname
    var memberfunc = memfunc
    var membervar = memlist

    def toString_cpp: String = {
      var s: String = ""
      if (templ.equals("")) {} else {
        s = "template<class " + templ + ">"
      }
      var sm: String = ""
      for (e <- memlist)
        sm = sm + e.toString_cpp + ";\n"
      var smf: String = ""
      for (e <- memfunc)
        smf = smf + " \n" + e.toString_cpp

      return s + " class " + cname + "{ \n public: \n" + sm + "\n " + smf + "};"
    }
    def costs(para: String): Map[Int,String] = {
      var m : Map[Int,String] = Map()
      for (b <- memfunc) {
        m ++= b.costs(para)
        // println(b.costs)      
      }
      return m
    }
  }
