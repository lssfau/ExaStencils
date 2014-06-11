package harald_dep.Abstract

import scala.collection.mutable.ListBuffer
import harald_dep.dsl._
import harald_dep.Impl._
import harald_dep.ast.TreeManager
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.primitives._
import exastencils.core._
import exastencils.core.collectors._

sealed abstract class AbstractStatement {
  def transform(scopeparas : ListBuffer[ParameterInfo]) : ListBuffer[Statement]
  def getvariables() : ListBuffer[String] = ListBuffer()
}

case class AbstractPCall(fname : String, arglist : List[AbstractExpression]) extends AbstractStatement {
  override def transform(scopeparas : ListBuffer[ParameterInfo]) : ListBuffer[Statement] = {
    var args : ListBuffer[Expression] = ListBuffer()
    for (a <- arglist)
      args += a.transform(scopeparas, None, "argument")

    var ret : ListBuffer[Statement] = ListBuffer()
    ret += new ImplPcall("", fname, args)
    return ret
  }

}
case class AbstractLoop(where : String, lev : String, order : String, blocksize : String, blocksteps : String, stmts : List[AbstractStatement]) extends AbstractStatement {

  override def transform(scopeparas : ListBuffer[ParameterInfo]) : ListBuffer[Statement] = {
    val lpkn = new DomainKnowledge.LoopKnowledge(DomainKnowledge.domain_L1.get._2, where, "1")

    val levIsInt = lev.forall(c => c.isDigit)

    val lpendvariable : String = lev match {
      case "lev"         => DomainKnowledge.unknown_L1(0)._1 + "[lev]" // + ".s"
      case _ if levIsInt => DomainKnowledge.unknown_L1(0)._1 + s"[$lev]"
      case _             => lev // + ".s"
    }

    val fieldCollection = FieldCollection
    val solField : Field = fieldCollection.getFieldByIdentifier("Solution", lev.toInt).get

    var start : ListBuffer[Expression] = ListBuffer()
    for (i <- 0 until Knowledge.dimensionality)
      start += (lpkn match {
        case DomainKnowledge.LoopKnowledge(_, "innerpoints", "1") => solField.layout(i).idxDupLeftBegin - solField.referenceOffset(i)
        case DomainKnowledge.LoopKnowledge(_, "allpoints", "1")   => solField.layout(i).idxGhostLeftBegin - solField.referenceOffset(i)
      })

    var stop : ListBuffer[Expression] = ListBuffer()

    lpkn match {
      case DomainKnowledge.LoopKnowledge("UnitSquare" | "UnitCube", "innerpoints", "1") => {
        for (i <- 0 until Knowledge.dimensionality)
          stop += solField.layout(i).idxDupRightEnd - solField.referenceOffset(i)
      }
      case DomainKnowledge.LoopKnowledge("UnitSquare" | "UnitCube", "allpoints", "1") => {
        for (i <- 0 until Knowledge.dimensionality)
          stop += solField.layout(i).idxGhostRightEnd - solField.referenceOffset(i)
      }
    }

    var body : ListBuffer[Statement] = ListBuffer()
    for (st <- stmts) {
      var stt = st.transform(scopeparas)
      for (s <- stt)
        body += s
    }

    var retl : ListBuffer[Statement] = ListBuffer()
    retl += new Implforloop(ListBuffer(new ParameterInfo("i", "int")), start, stop, ListBuffer(blocksteps.toInt, blocksteps.toInt, blocksteps.toInt), order, blocksize.toInt, body)
    return retl
  }

  override def getvariables() : ListBuffer[String] = {
    var s : ListBuffer[String] = ListBuffer()
    for (st <- stmts)
      s ++= st.getvariables
    return s
  }
}

case class AbstractRepeat(val expr : AbstractExpression, val stmt : List[AbstractStatement], val direction : String) extends AbstractStatement {
  override def transform(scopeparas : ListBuffer[ParameterInfo]) : ListBuffer[Statement] = {

    var ret : ListBuffer[Statement] = ListBuffer()

    var st2 : ListBuffer[Statement] = ListBuffer()
    for (a <- stmt) {
      var stt = a.transform(scopeparas)
      for (s <- stt)
        st2 += s
    }

    if (direction.equals("up"))
      ret += new Implforloop(ListBuffer(new ParameterInfo("i", "int")), ListBuffer(0), ListBuffer(expr.transform(scopeparas, None, "condition")), ListBuffer(1, 1, 1), "lex", 1, st2)
    else
      ret += new Implforloop(ListBuffer(new ParameterInfo("i", "int")), ListBuffer(0), ListBuffer(expr.transform(scopeparas, None, "condition")), ListBuffer(-1, -1, -1), "lex", 1, st2)

    return ret
  }

}
case class AbstractIfElse(val cond : AbstractExpression, ifstmts : List[AbstractStatement], elseifstmts : List[AbstractStatement]) extends AbstractStatement {
  override def transform(scopeparas : ListBuffer[ParameterInfo]) : ListBuffer[Statement] = {
    var ret : ListBuffer[Statement] = ListBuffer()
    var st1 : ListBuffer[Statement] = ListBuffer()
    for (a <- ifstmts) {
      var stt = a.transform(scopeparas)
      for (s <- stt)
        st1 += s
    }

    var st2 : ListBuffer[Statement] = ListBuffer()
    for (a <- elseifstmts) {
      var stt = a.transform(scopeparas)
      for (s <- stt)
        st2 += s
    }

    if (cond.toString.startsWith("coarsestlevel"))
      ret += new ConditionStatement(new EqEqExpression("lev",
        // COMM_HACK
        //        new BinaryExpression("-", "nlevels",
        //          1)),
        0),
        st1, st2)
    else
      ret += new ConditionStatement(cond.transform(scopeparas, None, "condition"), st1, st2)

    return ret
  }

}
case class AbstractLet(var id : String, var expr : AbstractExpression, var modifier : Option[String], var level : Option[Integer]) extends AbstractStatement {
  override def transform(scopeparas : ListBuffer[ParameterInfo]) : ListBuffer[Statement] = {

    var ret : ListBuffer[Statement] = ListBuffer()
    var ti : TypeInfo = new TypeInfo(id, 0)
    var levstr : Expression = new NullExpression

    for (e <- TreeManager.tree.exaFields)
      if (e.name.equals(id)) {
        ti = new TypeInfo(id, 1)
        levstr = if (level.isDefined) (new StringConstant(level.get.toString)) else "lev"
        val fieldCollection = FieldCollection

        // COMM_HACK
        id match {
          case "solution" => //return ListBuffer[Statement](AssignmentStatement("curFragment.solData[0][" ~ levstr ~ "]->getDataRef" ~ DomainKnowledge.rule_idxArray_cpp(), expr.transform(scopeparas, modifier, "expression")))
            val field : Field = fieldCollection.getFieldByIdentifier("Solution", level.get).get
            return ListBuffer[Statement](AssignmentStatement(new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex()), expr.transform(scopeparas, modifier, "expression")))
          case "Res" => //return ListBuffer[Statement](AssignmentStatement("curFragment.resData[0][" ~ levstr ~ "]->getDataRef" ~ DomainKnowledge.rule_idxArray_cpp(), expr.transform(scopeparas, modifier, "expression")))
            val field : Field = fieldCollection.getFieldByIdentifier("Residual", level.get).get
            return ListBuffer[Statement](AssignmentStatement(new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex()), expr.transform(scopeparas, modifier, "expression")))
          case "f" => //return ListBuffer[Statement](AssignmentStatement("curFragment.rhsData[0][" ~ levstr ~ "]->getDataRef" ~ DomainKnowledge.rule_idxArray_cpp(), expr.transform(scopeparas, modifier, "expression")))
            val field : Field = fieldCollection.getFieldByIdentifier("RHS", level.get).get
            return ListBuffer[Statement](AssignmentStatement(new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex()), expr.transform(scopeparas, modifier, "expression")))
          case _ =>
        }
      }

    for (e <- TreeManager.tree.exaOperators)
      if (e.name.equals(id)) {
        ti = new TypeInfo(id, 2)
        levstr = new NullExpression
      }

    for (e <- scopeparas)
      if (e.name.equals(id))
        if (e.dtype.startsWith("Container")) {
          ti = new TypeInfo(id, 1)
          levstr = new NullExpression
        }

    val idAndLvl : Expression = (if ("" == levstr.cpp) id else id ~ "[" ~ levstr ~ "]")
    ret += new AssignmentStatement(
      (ti.d match {
        case 0 => idAndLvl
        case 1 => idAndLvl ~ DomainKnowledge.rule_idxArray_cpp()
        case 2 => idAndLvl ~ "(Matrix (i0,i1))"
      }), expr.transform(scopeparas, modifier, "expression"))

    return ret
  }

  override def getvariables : ListBuffer[String] = {
    var s2 = expr.getvariables
    var s1 : ListBuffer[String] = ListBuffer(id)
    for (s <- s2)
      s1 += s
    return s1
  }
}

case class AbstractPLet(var id : String, val expr : AbstractExpression, modifier : Option[String], level : Option[Int]) extends AbstractStatement {
  override def transform(scopeparas : ListBuffer[ParameterInfo]) : ListBuffer[Statement] = {
    var ti : TypeInfo = new TypeInfo(id, 0)
    for (e <- TreeManager.tree.exaFields)
      if (e.name.equals(id)) {
        ti = new TypeInfo(id, 1)

        val levstr : Expression = if (level.isDefined) (new StringConstant(level.get.toString)) else "lev"
        val fieldCollection = FieldCollection

        // COMM_HACK
        id = id match {
          case "solution" => // ("curFragment.solData[0][" ~ levstr ~ "]->getDataRef").cpp
            val field : Field = fieldCollection.getFieldByIdentifier("Solution", level.get).get
            return ListBuffer[Statement](AssignmentStatement(new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex()), expr.transform(scopeparas, modifier, "expression"), "+="))
          case "Res" => //("curFragment.resData[0][" ~ levstr ~ "]->getDataRef").cpp
            val field : Field = fieldCollection.getFieldByIdentifier("Residual", level.get).get
            return ListBuffer[Statement](AssignmentStatement(new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex()), expr.transform(scopeparas, modifier, "expression"), "+="))
          case "f" => //("curFragment.rhsData[0][" ~ levstr ~ "]->getDataRef").cpp
            val field : Field = fieldCollection.getFieldByIdentifier("RHS", level.get).get
            return ListBuffer[Statement](AssignmentStatement(new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex()), expr.transform(scopeparas, modifier, "expression"), "+="))
          case _ => //id
            return ListBuffer[Statement](new AssignmentStatement(id ~ DomainKnowledge.rule_idxArray_cpp(), expr.transform(scopeparas, modifier, "expression"), "+="))
        }
      }
    for (e <- TreeManager.tree.exaOperators)
      if (e.name.equals(id))
        ti = new TypeInfo(id, 2)

    for (e <- scopeparas)
      if (e.name.equals(id))
        if (e.dtype.startsWith("Container"))
          ti = new TypeInfo(id, 1)

    var ret : ListBuffer[Statement] = ListBuffer()

    ti.d match {
      case 0 => ret += new AssignmentStatement(id, expr.transform(scopeparas, modifier, "expression"), "+=")
      case 1 => ret += new AssignmentStatement(id ~ DomainKnowledge.rule_idxArray_cpp(), expr.transform(scopeparas, modifier, "expression"), "+=")
      case 2 => ret += new AssignmentStatement(id ~ "(Matrix (i0,i1))", expr.transform(scopeparas, modifier, "expression"), "+=")
    }

    return ret
  }
  override def getvariables : ListBuffer[String] = {
    var s2 = expr.getvariables
    var s1 : ListBuffer[String] = ListBuffer(id)
    for (s <- s2)
      s1 += s
    return s1
  }
}

case class AbstractReturn(val expr : AbstractExpression) extends AbstractStatement {
  override def transform(scopeparas : ListBuffer[ParameterInfo]) : ListBuffer[Statement] = {
    var ret : ListBuffer[Statement] = ListBuffer()
    ret += new ReturnStatement(expr.transform(scopeparas, None, "return"))
    return ret
  }

}
case class AbstractReduction(stmt : AbstractStatement) extends AbstractStatement {
  override def transform(scopeparas : ListBuffer[ParameterInfo]) : ListBuffer[Statement] = {
    var ret : ListBuffer[Statement] = ListBuffer()
    ret += new ImplReductionStatement(stmt.transform(scopeparas)(0))
    return ret
  }
  override def getvariables() : ListBuffer[String] = {
    return stmt.getvariables
  }

}

case class AbstractDefinition(val p : Param, val value : AbstractExpression) extends AbstractStatement {

  override def toString = p.toString + " " + value.toString()

  override def transform(scopeparas : ListBuffer[ParameterInfo]) : ListBuffer[Statement] = {
    var ret : ListBuffer[Statement] = ListBuffer()
    ret += new VariableDeclarationStatement(DomainKnowledge.transform_datatype_cpp(p.dtype), p.name, Some(value.transform(scopeparas, None, "")))
    return ret
  }
}

