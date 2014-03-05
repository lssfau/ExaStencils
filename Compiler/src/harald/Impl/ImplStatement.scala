package harald.Impl

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.ast.TreeManager
import harald.expert.StencilGenerator
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives.LoopOverFragments
import exastencils.core.collectors.StackCollector

case class ImplCommunication(fname : String, loc : String) extends Statement {
  override def cpp : String = {
    return s"transfer(${fname},${loc});\n"
  }
}

case class ImplReductionStatement(s : Statement) extends Statement {
  override def cpp : String = {
    // COMM_HACK
    //s"${DomainKnowledge.datatype_L2.getOrElse("double")} s = 0; \n " + s.cpp + "return s;"
    s"${DomainKnowledge.datatype_L2.getOrElse("double")} s = 0;\n" +
      s.cpp + "\n" +
      "double sTotal;\n" +
      "MPI_Allreduce(&s, &sTotal, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);\n" +
      "return sTotal;\n"
  }
}

case class Implforloop(var loopvar : ListBuffer[ParameterInfo], var start : ListBuffer[Expression], var stop : ListBuffer[Expression], var stepsize : ListBuffer[Int], var runningorder : String, var blocksize : Int, var body : ListBuffer[Statement]) extends Statement {
  def stepToUpdate(step : Int, dim : Int, loopVarName : String) : String = {
    if (0 == step)
      return ""
    else if (1 == step)
      return s"++$loopVarName$dim"
    else if (-1 == step)
      return s"--$loopVarName$dim"
    else if (step > 0)
      return s"$loopVarName$dim += $step"
    else
      return s"$loopVarName$dim -= $step"
  }

  override def cpp : String = {
    // FIXME: make this node expandable and move mapping to expand function

    // COMM_HACK
    for (i <- 0 to stop.length - 1) {
      stop(i) = stop(i).cpp match {
        case "solution[lev].x1_"    => "curFragment.solData[0][lev]->x1_"
        case "solution[lev].x2_"    => "curFragment.solData[0][lev]->x2_"
        case "solution[lev].x3_"    => "curFragment.solData[0][lev]->x3_"
        case "(solution[lev].x1_ - 1)"    => "(curFragment.solData[0][lev]->x1_ - 1)"
        case "(solution[lev].x2_ - 1)"    => "(curFragment.solData[0][lev]->x2_ - 1)"
        case "(solution[lev].x3_ - 1)"    => "(curFragment.solData[0][lev]->x3_ - 1)"
        case "(solutionMinusOne.x1_ - 1)" => "(curFragment.solData[0][lev - 1]->x1_ - 1)"
        case "(solutionMinusOne.x2_ - 1)" => "(curFragment.solData[0][lev - 1]->x2_ - 1)"
        case "(solutionMinusOne.x3_ - 1)" => "(curFragment.solData[0][lev - 1]->x3_ - 1)"
        case _                            => stop(i)
      }
    }

    var s : String = ""
    for (b <- body)
      s += b.cpp
    var sloops : String = ""

    if (runningorder.equals("rb")) {
      // multicolor: int offset = ( i0 % 2 == 0 ? 1 : 2 ); fuer alle += 2 erster index 1,2 dann offset2 = ( i % 2 == offs2 ? 1 : 2 ); offset3 = ( j % 2 == offs3 ? 2 : 1 );

      for (off <- 0 to 1) {
        var wrappedBody : ListBuffer[Statement] = body; // TODO: clone?

        for (i <- start.length - 1 to 0 by -1) /* FIXME: this loop seems to be inverted */ {
          stepsize(i) = (if (start.length - 1 == i) 2 else 1)
          start(start.length - 1) = "offset"

          wrappedBody = ListBuffer[Statement](
            (if (start.length - 1 == i)
              s"int offset = 1 + (${(2 to DomainKnowledge.rule_dim()).map(i => loopvar(0).name + (i - 2)).mkString(" + ")} + $off) % 2;"
            else
              new NullStatement),
            new ForLoopStatement(
              loopvar(0).dtype ~ " " ~ s"${loopvar(0).name}$i" ~ " = " ~ start(i),
              s"${loopvar(0).name}$i < " ~ stop(i),
              stepToUpdate(stepsize(i), i, loopvar(0).name),
              wrappedBody))
        }
        sloops += StatementBlock(wrappedBody).cpp;
      }
    } else { // lex
      var wrappedBody : ListBuffer[Statement] = body; // TODO: clone?

      for (i <- 0 to start.length - 1) {
        if (stepsize(i) >= 0) {
          wrappedBody = ListBuffer[Statement](new ForLoopStatement(
            loopvar(0).dtype ~ " " ~ s"${loopvar(0).name}$i" ~ " = " ~ start(i),
            s"${loopvar(0).name}$i < " ~ stop(i),
            stepToUpdate(stepsize(i), i, loopvar(0).name),
            wrappedBody))
        } else {
          wrappedBody = ListBuffer[Statement](new ForLoopStatement(
            loopvar(0).dtype ~ " " ~ s"${loopvar(0).name}$i" ~ " = (" ~ stop(i) ~ "- 1)",
            s"${loopvar(0).name}$i >= " ~ start(i),
            stepToUpdate(stepsize(i), i, loopvar(0).name),
            wrappedBody))
        }
      }
      sloops += StatementBlock(wrappedBody).cpp;
    }

    // COMM_HACK
    if (start.length > 1)
      return (new LoopOverFragments(sloops)).expand(new StackCollector).cpp;
    else
      return sloops;
  }

  /* FIXME: reintegrate:
	override def toString_cuda : String = {
    var s : String = ""

    if (DomainKnowledge.rule_dim() == 2) {
      s += "unsigned int i1 = blockIdx.x*blockDim.x + threadIdx.x;\n"
      s += "unsigned int i2 = blockIdx.y*blockDim.y + threadIdx.y;\n"

      s += s"unsigned int global_idx = ${IdxKnowledge.mapidxToLinear(ListBuffer("i1", "i2"), ListBuffer("s1", "s2"))};\n"
      for (b <- body) {
        if (b.contains_modifier("ToCoarse")) {
          var lb : ListBuffer[String] = ListBuffer()
          for (i <- 1 to DomainKnowledge.rule_dim())
            lb += DomainKnowledge.rule_mapcoarseTofine("i" + (i).toString)

          s += s"unsigned int global_idx_2 = ${IdxKnowledge.mapidxToLinear(lb, ListBuffer("s1_1", "s2_1"))};\n"
          //          println("in cuda loop" + b.cpp)

          var curStencil = TreeManager.tree.Stencils(0)
          for (st <- TreeManager.tree.Stencils)
            if (st.name.equals("RestrictionStencil"))
              curStencil = st

          val exprloop : Expression = StencilGenerator.generateStencilConvolutioncuda(9, curStencil, "fine", 1, "", "global_idx_2")
          s += s"if (i1 >= ${start(0).toString_cpp} && i2 >= ${start(1).toString_cpp} && i1 < s1 - ${start(0).toString_cpp} && i2 < s2 - ${start(1).toString_cpp}) { \n"
          val statloop = b match {
            case ImplAssigmentStatement(variable, op, expr, mod) => new ImplAssigmentStatement(variable, op, exprloop, mod)
          }
          s += statloop.toString_cuda
          s += "}\n"

          return s
        }
        if (b.contains_modifier("ToFine")) {
          var lb : ListBuffer[String] = ListBuffer()
          for (i <- 1 to DomainKnowledge.rule_dim())
            lb += DomainKnowledge.rule_mapcoarseTofine("i" + (i).toString)

          s += s"unsigned int global_idx_2 = ${IdxKnowledge.mapidxToLinear(lb, ListBuffer("s1_1", "s2_1"))};\n"
          //          println("in cuda loop" + b.cpp)
          val exprloop : Expression = StencilGenerator.generateStencilInterpolationcuda("uc", "i")
          s += s"if (i1 >= ${start(0).toString_cpp} && i2 >= ${start(1).toString_cpp} && i1 < s1 - ${start(0).toString_cpp} && i2 < s2 - ${start(1).toString_cpp}) { \n"
          val statloop = b match {
            case ImplAssigmentStatement(variable, op, expr, mod) => new ImplAssigmentStatement(variable, op, exprloop, mod)
          }
          s += statloop.toString_cuda
          s += "}\n"

          return s
        }
      }

      s += s"if (i1 >= ${start(0).toString_cpp} && i2 >= ${start(1).toString_cpp} && i1 < s1 - ${start(0).toString_cpp} && i2 < s2 - ${start(1).toString_cpp}) { \n"
      if (runningorder.equals("rb"))
        s += s"if (((i1+i2)%2) == red_black) \n"

      for (b <- body)
        s += b.cpp // FIXME: inteded was toString_cuda
      s += "}\n"
      return s
    } else
      return ""
  }*/
}

case class ImplPcall(obj : String, name : String, paramlist : ListBuffer[Expression]) extends Statement {
  override def cpp : String =
    {
      var objs = obj
      if (!objs.equals(""))
        objs = objs + "."

      if (name.equals("print")) {
        return (new Scope(ListBuffer[Statement](
          "int rank;",
          "MPI_Comm_rank(MPI_COMM_WORLD, &rank);",
          "if (0 == rank) {",
          (if (TreeManager.tree.isinFields(paramlist(0).cpp)) {
            var s : String = DomainKnowledge.rule_idxArray_cpp()
            "std::cout << " + paramlist(0).cpp + s + " << \" \" ;"
          } else {
            var pstr = "std::cout << "
            for (p <- paramlist)
              pstr += p.cpp + " << \" \" << "
            pstr += " std::endl; "
            pstr
          }),
          "}"))).cpp
      }
      //    return name + " ( " + paramlist.mkString(",") + ");\n"

      var location = ""
      for (f <- TreeManager.tree.Functions)
        if (f._2.name.equals(name))
          location = f._2.location

      // COMM_HACK
      if ("" == location) location = "cpu"

      var s = ""

      if (location.equals("cpu")) {
        s = objs + name + " ( "
        if (paramlist.length > 0)
          s = s + paramlist(0).cpp
        for (i <- 1 to paramlist.length - 1)
          s = s + "," + paramlist(i).cpp
        return s + ");\n"
      } else {
        s = objs + name + "<<<dimgrid,dimblock>>> ( "
        if (paramlist.length > 0)
          s = s + paramlist(0).cpp
        for (i <- 1 to paramlist.length - 1)
          s = s + "," + paramlist(i).cpp
        return s + ");\n"
      }
    }

  def evaluate {}
}
