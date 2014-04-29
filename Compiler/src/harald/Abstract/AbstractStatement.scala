package harald.Abstract

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.Impl._
import harald.ast._

sealed abstract class AbstractStatement {
  def transform(scopeparas: ListBuffer[ParameterInfo]): ListBuffer[ImplStatement]
  def getvariables(): ListBuffer[String] = ListBuffer()
}

case class AbstractCommunication(fname: String, loc: String) extends AbstractStatement {
  override def transform(scopeparas: ListBuffer[ParameterInfo]): ListBuffer[ImplStatement] = {

    var ret: ListBuffer[ImplStatement] = ListBuffer()
    ret += new ImplCommunication(fname, loc)
    return ret
  }
}

case class AbstractPCall(fname: String, arglist: List[AbstractExpression]) extends AbstractStatement {
  override def transform(scopeparas: ListBuffer[ParameterInfo]): ListBuffer[ImplStatement] = {
    var args: ListBuffer[ImplExpression] = ListBuffer()
    for (a <- arglist) {

//      println("aha " + a + " " + a.getClass())
      a match {
        case AbstractVariable("location",_,_) => {
          var lb : ListBuffer[String] = ListBuffer()
          lb += "i0"
          for (i <- 1 to DomainKnowledge.rule_dim() - 1) 
            lb += s"i${i}"
          
          val phcoords = IdxKnowledge.mapidxTocoordcast(lb,"lev")
          for (i <- 0 to DomainKnowledge.rule_dim() - 1) {
            var loclist : AbstractExpression = new AbstractVariable(phcoords(i),"",new AbstractLiteral(""))
            args += loclist.transform(scopeparas, "", None,  "argument")
          }
        }
        case AbstractVariable("index",_,_) => {
          var lb : ListBuffer[String] = ListBuffer()
          lb += "i0"
          for (i <- 1 to DomainKnowledge.rule_dim() - 1) 
            lb += s"i${i}"
          
          for (i <- 0 to DomainKnowledge.rule_dim() - 1) {
            var loclist : AbstractExpression = new AbstractVariable(lb(i),"",new AbstractLiteral(""))
            args += loclist.transform(scopeparas, "", None,  "argument")
          }
        }
        case _ => args += a.transform(scopeparas, "", None,  "argument")
      }

    }

    var ret: ListBuffer[ImplStatement] = ListBuffer()
    ret += new ImplPcall("", fname, args)
    return ret
  }

}
case class AbstractLoop(where: String, lev: String, order: String, blocksize: String, blocksteps: String, stmts: List[AbstractStatement]) extends AbstractStatement {

  override def transform(scopeparas: ListBuffer[ParameterInfo]): ListBuffer[ImplStatement] = {
    val lpkn = new DomainKnowledge.LoopKnowledge(DomainKnowledge.domain_L1.get._2, where, "1")


    var parall = true
    var no = 0
    for (scpara <- scopeparas) {
      if (scpara.name.equals("notparallelizable")) {
        scopeparas.remove(no)
        parall = false
      }
      no = no +1
    }
    
    var comp = 1
    var compstr = ""
    for (f <- TreeManager.tree.Fields)
      if (f.name.equals(DomainKnowledge.unknown_L1(0)._1))
        comp = f.veclength
    if (comp > 1)
        compstr = "0"
      
    val lpendvariable: String = lev match {
      case "lev" => DomainKnowledge.unknown_L1(0)._1 + s"${compstr}[lev]" // + ".s"
      case _ => lev // + ".s"
    }

    var startidx: Int =
      lpkn match {

        case DomainKnowledge.LoopKnowledge(_, "innerpoints", "1") => 1
        case DomainKnowledge.LoopKnowledge(_, "allpoints", "1") => 0
      }
    var start: ListBuffer[ImplExpression] = ListBuffer()
    for (i <- 1 to DomainKnowledge.rule_dim())
      start += new ImplValueExpr[Int](startidx)

    var stop: ListBuffer[ImplExpression] = ListBuffer()

    lpkn match {
      case DomainKnowledge.LoopKnowledge("UnitSquare" | "UnitCube", "innerpoints", "1") => {
        for (i <- 1 to DomainKnowledge.rule_dim())
          stop += new ImplBinaryExpr(new ImplVariable(lpendvariable, "x" + i.toString + "_", "", new TypeInfo(lpendvariable + "x" + i.toString, 0), new ImplExpression(), "expression"), new OperatorInfo("-"), new ImplValueExpr[Int](1))
        //	                  stop += new BinaryExpr(new functioncall(lpendvariable + i.toString,ListBuffer()), new OperatorInfo("-"), new ValueExpr[Int](1)) 
      }
      case DomainKnowledge.LoopKnowledge("UnitSquare" | "UnitCube", "allpoints", "1") => {
        for (i <- 1 to DomainKnowledge.rule_dim())
          stop += new ImplVariable(lpendvariable, "x" + i.toString + "_", "",new TypeInfo(lpendvariable + "x" + i.toString, 0), new ImplExpression(), "expression") //functioncall(lpendvariable + i.toString,ListBuffer())

      }
    }

    var body: ListBuffer[ImplStatement] = ListBuffer()
    for (st <- stmts) {
      var stt = st.transform(scopeparas)
      for (s <- stt)
        body += s
    }

    var retl: ListBuffer[ImplStatement] = ListBuffer()
    retl += new Implforloop(ListBuffer(new ParameterInfo("i", "int")), start, stop, ListBuffer(blocksteps.toInt, blocksteps.toInt, blocksteps.toInt), order, parall, blocksize.toInt, body)
    return retl
  }

  override def getvariables(): ListBuffer[String] = {
    var s: ListBuffer[String] = ListBuffer()
    for (st <- stmts)
      s ++= st.getvariables
    return s
  }
}

case class AbstractWhile(val cond: AbstractExpression, val stmt: List[AbstractStatement]) extends AbstractStatement {
   override def transform(scopeparas: ListBuffer[ParameterInfo]): ListBuffer[ImplStatement] = {
     var ret: ListBuffer[ImplStatement] = ListBuffer()
     

     var st2: ListBuffer[ImplStatement] = ListBuffer()
    for (a <- stmt) {
      var stt = a.transform(scopeparas)
      for (s <- stt)
        st2 += s
    }

     ret += new Implwhileloop(cond.transform(scopeparas, "", None, "condition"), st2)
      
     return ret
   }
}

case class AbstractRepeat(val expr: AbstractExpression, val stmt: List[AbstractStatement], val direction: String, val variablename : String) extends AbstractStatement {
  override def transform(scopeparas: ListBuffer[ParameterInfo]): ListBuffer[ImplStatement] = {

    var ret: ListBuffer[ImplStatement] = ListBuffer()
    
    var st2: ListBuffer[ImplStatement] = ListBuffer()
    for (a <- stmt) {
      var stt = a.transform(scopeparas)
      for (s <- stt)
        st2 += s
    }

    if (direction.equals("up"))
      ret += new Implforloop(ListBuffer(new ParameterInfo(variablename, "int")), ListBuffer(new ImplValueExpr[Int](0)), ListBuffer(expr.transform(scopeparas, "",None, "condition")), ListBuffer(1, 1, 1), "lex", false, 1, st2)
    else
      ret += new Implforloop(ListBuffer(new ParameterInfo(variablename, "int")), ListBuffer(new ImplValueExpr[Int](0)), ListBuffer(expr.transform(scopeparas, "",None, "condition")), ListBuffer(-1, -1, -1), "lex", false, 1, st2)

    return ret
  }

}
case class AbstractIfElse(val cond: AbstractExpression, ifstmts: List[AbstractStatement], elseifstmts: List[AbstractStatement]) extends AbstractStatement {
  override def transform(scopeparas: ListBuffer[ParameterInfo]): ListBuffer[ImplStatement] = {
    var ret: ListBuffer[ImplStatement] = ListBuffer()
    var st1: ListBuffer[ImplStatement] = ListBuffer()
    for (a <- ifstmts) {
      var stt = a.transform(scopeparas)
      for (s <- stt)
        st1 += s
    }

    var st2: ListBuffer[ImplStatement] = ListBuffer()
    for (a <- elseifstmts) {
      var stt = a.transform(scopeparas)
      for (s <- stt)
        st2 += s
    }

    if (cond.toString.startsWith("coarsestlevel"))
      ret += new ImplIfelseStatement(new ImplBinaryExpr(new ImplVariable("", "lev", "",new TypeInfo("lev", 0), new ImplExpression(), "expression"),
        new OperatorInfo("=="),
        new ImplBinaryExpr(new ImplVariable("", "nlevels", "",new TypeInfo("nlevels", 0), new ImplExpression(), "expression"),
          new OperatorInfo("-"),
          new ImplValueExpr[Int](1))), st1, st2)
    else
      ret += new ImplIfelseStatement(cond.transform(scopeparas, "",None, "condition"), st1, st2)

    return ret
  }

}
case class AbstractLet(val lhs: AbstractExpression, val expr: AbstractExpression, modifier: Option[String]) extends AbstractStatement {
    
  var id = lhs match { case AbstractVariable(a,comp,lev) => a case _ => ""}
    
  override def transform(scopeparas: ListBuffer[ParameterInfo]): ListBuffer[ImplStatement] = {

    var ret: ListBuffer[ImplStatement] = ListBuffer()
    var ti: TypeInfo = new TypeInfo(id, 0)
    var levstr: ImplExpression = new ImplExpression()

    var comp = 1
    var compstr = ""
    var complexstr = ""
    for (e <- TreeManager.tree.Fields)
      if (e.name.equals(id)) {
        ti = new TypeInfo(id, 1, e.datatype)
        comp = e.veclength
        levstr = new ImplValueExpr[String]("lev")
      }

    for (e <- TreeManager.tree.Stencils)
      if (e.name.equals(id)) {
        ti = new TypeInfo(id, 2)
        levstr = new ImplExpression()
      }

    for (e <- scopeparas)
      if (e.name.equals(id))
        if (e.dtype.startsWith(DomainKnowledge.ArrayClassName)) {
          ti = new TypeInfo(id, 1)
          levstr = new ImplExpression()
        }

    for (i <- 0 to comp-1) {
      if (comp > 1)
        compstr = s"${i}"
      
      if (comp > 1) {
       if (modifier.getOrElse("").equals("Re")) {
        if (i == 0)
         ret += new ImplAssigmentStatement(new ImplVariable("", id, compstr,ti, levstr, "statement", modifier.getOrElse("")), new OperatorInfo("="), expr.transform(scopeparas, compstr, modifier, "expression"), modifier.getOrElse(""))
       } else if (modifier.getOrElse("").equals("Im")) {
        if (i == 1)
        ret += new ImplAssigmentStatement(new ImplVariable("", id, compstr,ti, levstr, "statement", modifier.getOrElse("")), new OperatorInfo("="), expr.transform(scopeparas, compstr, modifier, "expression"), modifier.getOrElse(""))
       } else
        ret += new ImplAssigmentStatement(new ImplVariable("", id, compstr,ti, levstr, "statement", modifier.getOrElse("")), new OperatorInfo("="), expr.transform(scopeparas, compstr, modifier, "expression"), modifier.getOrElse(""))
      } else
        ret += new ImplAssigmentStatement(new ImplVariable("", id, compstr,ti, levstr, "statement", modifier.getOrElse("")), new OperatorInfo("="), expr.transform(scopeparas, compstr, modifier, "expression"), modifier.getOrElse(""))
    }
    
    return ret
  }

  override def getvariables: ListBuffer[String] = {
    var s2 = expr.getvariables
    var s1: ListBuffer[String] = ListBuffer(id)
    for (s <- s2)
      s1 += s
    return s1
  }
}

case class AbstractPLet(val id: String, val expr: AbstractExpression, modifier: Option[String]) extends AbstractStatement {
  override def transform(scopeparas: ListBuffer[ParameterInfo]): ListBuffer[ImplStatement] = {
    var ti: TypeInfo = new TypeInfo(id, 0)
    for (e <- TreeManager.tree.Fields)
      if (e.name.equals(id))
        ti = new TypeInfo(id, 1)
    for (e <- TreeManager.tree.Stencils)
      if (e.name.equals(id))
        ti = new TypeInfo(id, 2)

    for (e <- scopeparas)
      if (e.name.equals(id))
        if (e.dtype.startsWith(DomainKnowledge.ArrayClassName)) 
          ti = new TypeInfo(id, 1)

    var ret: ListBuffer[ImplStatement] = ListBuffer()

    if (modifier.getOrElse("").equals("Vec2")) {
    ret += new ImplAssigmentStatement(new ImplVariable("", id,"", ti, new ImplExpression(), "statement"), new OperatorInfo("+="), expr.transform(scopeparas, "0",modifier, "expression"), modifier.getOrElse(""))
    ret += new ImplAssigmentStatement(new ImplVariable("", id,"", ti, new ImplExpression(), "statement"), new OperatorInfo("+="), expr.transform(scopeparas, "1",modifier, "expression"), modifier.getOrElse(""))
    } else
    ret += new ImplAssigmentStatement(new ImplVariable("", id,"", ti, new ImplExpression(), "statement"), new OperatorInfo("+="), expr.transform(scopeparas, "",modifier, "expression"), modifier.getOrElse(""))
    
    return ret
  }
  override def getvariables: ListBuffer[String] = {
    var s2 = expr.getvariables
    var s1: ListBuffer[String] = ListBuffer(id)
    for (s <- s2)
      s1 += s
    return s1
  }
}

case class AbstractReturn(val expr: AbstractExpression) extends AbstractStatement {
  override def transform(scopeparas: ListBuffer[ParameterInfo]): ListBuffer[ImplStatement] = {
    var ret: ListBuffer[ImplStatement] = ListBuffer()
    ret += new ImplReturnStatement(expr.transform(scopeparas, "",None, "return"))
    return ret
  }

}
case class AbstractReduction(stmt: AbstractStatement) extends AbstractStatement {
  override def transform(scopeparas: ListBuffer[ParameterInfo]): ListBuffer[ImplStatement] = {
    var ret: ListBuffer[ImplStatement] = ListBuffer()
    scopeparas += new ParameterInfo("notparallelizable","int")
    ret += new ImplReductionStatement(stmt.transform(scopeparas)(0))
    return ret
  }
  override def getvariables(): ListBuffer[String] = {
    return stmt.getvariables
  }

}

  case class AbstractDefinition(val p : Param, val value: AbstractExpression) extends AbstractStatement  {

    override def toString = p.toString + " " + value.toString()
    
    override def transform(scopeparas: ListBuffer[ParameterInfo]): ListBuffer[ImplStatement] = { 
      var ret: ListBuffer[ImplStatement] = ListBuffer()
      ret += new ImplDefinitionStatement(p.name, DomainKnowledge.transform_datatype_cpp(p.dtype),value.transform(scopeparas, "",None, ""))
      return ret
    }
  }

