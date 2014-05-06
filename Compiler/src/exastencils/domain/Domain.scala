package exastencils.domain

import java.io.PrintWriter
import java.io.File
import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.core.collectors._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._
import exastencils.mpi._
import exastencils.prettyprinting._
import exastencils.omp._

case class PointOutsideDomain(var pos : Expression, var domain : Int) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = PointOutsideDomain\n"

  override def expand : Expression = {
    s"(" ~
      ((pos ~ ".x") < Knowledge.domain_subdomains(domain).lower_x) Or ((pos ~ ".x") > Knowledge.domain_subdomains(domain).upper_x) Or
      ((pos ~ ".y") < Knowledge.domain_subdomains(domain).lower_y) Or ((pos ~ ".y") > Knowledge.domain_subdomains(domain).upper_y) Or
      ((pos ~ ".z") < Knowledge.domain_subdomains(domain).lower_z) Or ((pos ~ ".z") > Knowledge.domain_subdomains(domain).upper_z) ~ ")"
  }
}

case class PointInsideDomain(var pos : Expression, var domain : Int) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = PointInsideDomain\n"

  override def expand : Expression = {
    s"(" ~
      ((pos ~ ".x") >= Knowledge.domain_subdomains(domain).lower_x) And ((pos ~ ".x") <= Knowledge.domain_subdomains(domain).upper_x) And
      ((pos ~ ".y") >= Knowledge.domain_subdomains(domain).lower_y) And ((pos ~ ".y") <= Knowledge.domain_subdomains(domain).upper_y) And
      ((pos ~ ".z") >= Knowledge.domain_subdomains(domain).lower_z) And ((pos ~ ".z") <= Knowledge.domain_subdomains(domain).upper_z) ~ ")"
  }
}

case class PointToFragmentId(var pos : Expression) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = PointToFragmentId\n"

  override def expand : Expression = {
    "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".z") - Knowledge.domain_size.lower_z) / Knowledge.domain_fragWidth_z) * Knowledge.domain_numFragsTotal_y * Knowledge.domain_numFragsTotal_x +
      "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - Knowledge.domain_size.lower_y) / Knowledge.domain_fragWidth_y) * Knowledge.domain_numFragsTotal_x +
      "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - Knowledge.domain_size.lower_x) / Knowledge.domain_fragWidth_x)
  }
}

case class PointToLocalFragmentId(var pos : Expression) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = PointToFragmentId\n"

  override def expand : Expression = {
    (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".z") - Knowledge.domain_size.lower_z) / Knowledge.domain_fragWidth_z)) Mod Knowledge.domain_numFragsPerBlock_z) * Knowledge.domain_numFragsPerBlock_y * Knowledge.domain_numFragsPerBlock_x +
      (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - Knowledge.domain_size.lower_y) / Knowledge.domain_fragWidth_y)) Mod Knowledge.domain_numFragsPerBlock_y) * Knowledge.domain_numFragsPerBlock_x +
      (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - Knowledge.domain_size.lower_x) / Knowledge.domain_fragWidth_x)) Mod Knowledge.domain_numFragsPerBlock_x)
  }
}

case class PointToOwningRank(var pos : Expression, var domain : Int) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = PointToOwningRank\n"

  override def expand : Expression = {
    TernaryConditionExpression(PointOutsideDomain(pos, domain),
      s"MPI_PROC_NULL",
      ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".z") - Knowledge.domain_size.lower_z) / Knowledge.domain_fragWidth_z) / Knowledge.domain_numFragsPerBlock_z) * Knowledge.domain_numBlocks_y * Knowledge.domain_numBlocks_x
        + ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - Knowledge.domain_size.lower_y) / Knowledge.domain_fragWidth_y) / Knowledge.domain_numFragsPerBlock_y) * Knowledge.domain_numBlocks_x
        + ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - Knowledge.domain_size.lower_x) / Knowledge.domain_fragWidth_x) / Knowledge.domain_numFragsPerBlock_x))
  }
}

case class AssertStatement(var check : Expression, var msg : Expression, var abort : Statement) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = AssertStatement\n"

  override def expand : ConditionStatement = {
    new ConditionStatement(check,
      ListBuffer[Statement](
        "LOG_ERROR(" ~ msg ~ ")",
        abort))
  }
}

case class ConnectFragments() extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ConnectFragments\n"

  override def expand : LoopOverFragments = {
    var body = new ListBuffer[Statement]

    var neighbors = StateManager.findFirst[FragmentClass]().get.neighbors

    for (d <- 0 until Knowledge.domain_numSubdomains) {
      body += AssignmentStatement(s"curFragment.isValidForSubdomain[$d]", PointInsideDomain(s"curFragment.pos", d))
    }

    if (Knowledge.useMPI || Knowledge.useOMP) {
      for (neigh <- neighbors) {
        body += new Scope(ListBuffer[Statement](
          s"Vec3 offsetPos = curFragment.pos + Vec3(${neigh.dir(0)} * ${Knowledge.domain_fragWidth_x}, ${neigh.dir(1)} * ${Knowledge.domain_fragWidth_y}, ${neigh.dir(2)} * ${Knowledge.domain_fragWidth_z})") ++
          (0 until Knowledge.domain_numSubdomains).toArray[Int].map(d =>
            new ConditionStatement(s"curFragment.isValidForSubdomain[$d]" And PointInsideDomain(s"offsetPos", d),
              if (Knowledge.useMPI) {
                (if (Knowledge.useOMP)
                  new ConditionStatement(s"mpiRank ==" ~ PointToOwningRank("offsetPos", d),
                  s"curFragment.connectLocalElement(${neigh.index}, fragmentMap[" ~ PointToFragmentId("offsetPos") ~ s"], $d)",
                  s"curFragment.connectRemoteElement(${neigh.index}," ~ PointToLocalFragmentId("offsetPos") ~ "," ~ PointToOwningRank("offsetPos", d) ~ s", $d)")
                else
                  s"curFragment.connectRemoteElement(${neigh.index}," ~ PointToLocalFragmentId("offsetPos") ~ "," ~ PointToOwningRank("offsetPos", d) ~ s", $d)") : Statement
              } else {
                (s"curFragment.connectLocalElement(${neigh.index}, fragmentMap[" ~ PointToFragmentId("offsetPos") ~ s"], $d)") : Statement
              })))
      }
    }
    return new LoopOverFragments(body) with OMP_PotentiallyParallel
  }
}

case class SetupBuffers() extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = SetupBuffers\n"

  override def expand : LoopOverFragments = {
    new LoopOverFragments("curFragment.setupBuffers()") with OMP_PotentiallyParallel
  }
}

case class ValidatePrimitives() extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ValidatePrimitives\n"

  //  TODO
  //  override def expand : LoopOverFragments = {
  //    new LoopOverFragments("curFragment.validate();") with OMP_PotentiallyParallel
  //  }
  override def expand : NullStatement = {
    NullStatement()
  }
}

case class InitGeneratedDomain() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = InitGeneratedDomain\n"

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"initDomain", ListBuffer(),
      ListBuffer(
        new MPI_SetRankAndSize,

        if (Knowledge.useMPI)
          AssertStatement(s"mpiSize != ${Knowledge.domain_numBlocks}",
          "\"Invalid number of MPI processes (\" << mpiSize << \") should be \" << " + (Knowledge.domain_numBlocks),
          "return")
        else
          new NullStatement,

        "std::map<size_t, Fragment3DCube*> fragmentMap",

        s"Vec3 positions[${Knowledge.domain_numFragsPerBlock}]",
        s"unsigned int posWritePos = 0",
        if (Knowledge.useMPI)
          s"Vec3 rankPos(mpiRank % ${Knowledge.domain_numBlocks_x}, (mpiRank / ${Knowledge.domain_numBlocks_x}) % ${Knowledge.domain_numBlocks_y}, mpiRank / ${Knowledge.domain_numBlocks_x * Knowledge.domain_numBlocks_y})"
        else
          s"Vec3 rankPos(1, 1, 1)",

        new LoopOverDimensions(IndexRange(MultiIndex(0, 0, 0), MultiIndex(Knowledge.domain_numFragsPerBlock_x, Knowledge.domain_numFragsPerBlock_y, Knowledge.domain_numFragsPerBlock_z)),
          new AssignmentStatement("positions[posWritePos++]", "Vec3("
            ~ (("rankPos.x" : Expression) * Knowledge.domain_numFragsPerBlock_x + 0.5 + "x") * Knowledge.domain_fragWidth_x ~ ","
            ~ (if (Knowledge.dimensionality > 1) (("rankPos.y" : Expression) * Knowledge.domain_numFragsPerBlock_y + 0.5 + "y") * Knowledge.domain_fragWidth_y else 0) ~ ","
            ~ (if (Knowledge.dimensionality > 2) (("rankPos.z" : Expression) * Knowledge.domain_numFragsPerBlock_z + 0.5 + "z") * Knowledge.domain_fragWidth_z else 0) ~ ")")),
        LoopOverFragments(ListBuffer(
          s"fragments[f] = new Fragment3DCube()",
          s"fragments[f]->id = " ~ PointToFragmentId("positions[f]"),
          s"fragments[f]->commId = " ~ PointToLocalFragmentId("positions[f]"),
          s"fragments[f]->pos = positions[f]",
          s"fragmentMap[fragments[f]->id] = fragments[f]"),
          None,
          false),
        ConnectFragments(),
        new SetupBuffers))
  }
}

case class DomainGenerated() extends Node with FilePrettyPrintable {
  var statements_HACK : ListBuffer[Statement] = new ListBuffer

  statements_HACK += new InitGeneratedDomain

  override def printToFile = {
    val writer = PrettyprintingManager.getPrinter(s"Domains/DomainGenerated.h")

    // TODO: add includes to class node or similar
    writer << (
      "#include <map>\n"
      + "#include \"Util/Log.h\"\n"
      + "#include \"Util/Vector.h\"\n")

    for (s <- statements_HACK)
      writer << s.cpp + "\n"
  }
}
