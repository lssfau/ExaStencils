package harald.Impl

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.ast.TreeManager
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.core.collectors._
import exastencils.primitives._
import exastencils.omp._

case class ImplReductionStatement(s : Statement) extends Statement with Expandable {
  def cpp = "NOT VALID ; CLASS = ImplReductionStatement\n";

  def expand(collector : StackCollector) : StatementBlock = {
    var statements : ListBuffer[Statement] = new ListBuffer
    statements += s"${DomainKnowledge.datatype_L2.getOrElse("double")} s = 0;"
    statements += s
    statements += "double sTotal;"
    statements += "MPI_Allreduce(&s, &sTotal, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);"
    statements += "return sTotal;"
    StatementBlock(statements)
  }
}

case class Implforloop(var loopvar : ListBuffer[ParameterInfo], var start : ListBuffer[Expression], var stop : ListBuffer[Expression], var stepsize : ListBuffer[Int], var runningorder : String, var blocksize : Int, var body : ListBuffer[Statement]) extends Statement with Expandable {
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

  def cpp = "NOT VALID ; CLASS = Implforloop\n";

  def expand(collector : StackCollector) : Statement = {
    // FIXME: support for OMP_PotentiallyParallel is currently missing; this is partly due to the shallow expands

    // COMM_HACK
    for (i <- 0 to stop.length - 1) {
      if (stop(i).cpp.length > "solution".length) {
        if ("solution" == stop(i).cpp.substring(0, "solution".length)) {
          stop(i) = "curFragment.solData[0]" + stop(i).cpp.substring("solution".length).replace(".", "->")
        } else if ("(solution" == stop(i).cpp.substring(0, "(solution".length)) {
          stop(i) = "(curFragment.solData[0]" + stop(i).cpp.substring("(solution".length).replace(".", "->")
        }
      }
    }

    if (runningorder.equals("rb")) {
      // multicolor: int offset = ( i0 % 2 == 0 ? 1 : 2 ); fuer alle += 2 erster index 1,2 dann offset2 = ( i % 2 == offs2 ? 1 : 2 ); offset3 = ( j % 2 == offs3 ? 2 : 1 );

      var loops : ListBuffer[Statement] = new ListBuffer

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
              wrappedBody) /* FIXME: add OMP support */ )
        }

        if (start.length > 1)
          loops += new LoopOverFragments(wrappedBody) with OMP_PotentiallyParallel
        else
          loops ++= wrappedBody
      }

      return StatementBlock(loops)
    } else { // lex
      if (start.length > 1) {
        return new LoopOverFragments(
          // TODO: add sth like new ConditionStatement(s"curFragment.isValidForSubdomain[${field.domain}]",
          new LoopOverDimensions(IndexRange(new MultiIndex(start.toArray), new MultiIndex(stop.toArray)), body) with OMP_PotentiallyParallel) with OMP_PotentiallyParallel
      } else {
        if (stepsize(0) >= 0) {
          return new ForLoopStatement(
            loopvar(0).dtype ~ " " ~ s"${loopvar(0).name}0" ~ " = " ~ start(0),
            s"${loopvar(0).name}0 < " ~ stop(0),
            stepToUpdate(stepsize(0), 0, loopvar(0).name),
            body)
        } else {
          return new ForLoopStatement(
            loopvar(0).dtype ~ " " ~ s"${loopvar(0).name}0" ~ " = " ~ (stop(0) - 1),
            s"${loopvar(0).name}0 >= " ~ start(0),
            stepToUpdate(stepsize(0), 0, loopvar(0).name),
            body)
        }
      }
    }
  }
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
          {
            var pstr = "std::cout << "
            for (p <- paramlist)
              pstr += p.cpp + " << \" \" << "
            pstr += " std::endl; "
            pstr
          },
          "}"))).cpp
      }
      //    return name + " ( " + paramlist.mkString(",") + ");\n"

      var s = ""

      s = objs + name + " ( "
      if (paramlist.length > 0)
        s = s + paramlist(0).cpp
      for (i <- 1 to paramlist.length - 1)
        s = s + "," + paramlist(i).cpp
      return s + ");\n"
    }

  def evaluate {}
}
