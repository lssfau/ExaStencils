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

// FIXME: incorporate fragLengthPerDim

case class PointOutsideDomain(var pos : Expression) extends Expression {
  def cpp : String = {
    (s"(${pos.cpp}.x < 0.0 || ${pos.cpp}.x > ${Knowledge.domain_numFragsTotal_x} || ${pos.cpp}.y < 0.0 || ${pos.cpp}.y > ${Knowledge.domain_numFragsTotal_y} || ${pos.cpp}.z < 0.0 || ${pos.cpp}.z > ${Knowledge.domain_numFragsTotal_z})");
  }
}

case class PointInsideDomain(var pos : Expression) extends Expression {
  def cpp : String = {
    (s"(${pos.cpp}.x >= 0.0 && ${pos.cpp}.x <= ${Knowledge.domain_numFragsTotal_x} && ${pos.cpp}.y >= 0.0 && ${pos.cpp}.y <= ${Knowledge.domain_numFragsTotal_y} && ${pos.cpp}.z >= 0.0 && ${pos.cpp}.z <= ${Knowledge.domain_numFragsTotal_z})");
  }
}

case class PointToFragmentId(var pos : Expression) extends Expression {
  def cpp : String = {
    (s"((size_t)(floor(${pos.cpp}.z) * ${Knowledge.domain_numFragsTotal_y * Knowledge.domain_numFragsTotal_x} + floor(${pos.cpp}.y) * ${Knowledge.domain_numFragsTotal_x} + floor(${pos.cpp}.x)))");
  }
}

case class PointToOwningRank(var pos : Expression) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = PointToOwningRank\n";

  override def expand(collector : StackCollector) : Expression = {
    TernaryConditionExpression(PointOutsideDomain(pos).cpp,
      s"MPI_PROC_NULL",
      s"(int)(floor(${pos.cpp}.z / ${Knowledge.domain_numFragsPerBlock_z}) * ${Knowledge.domain_numBlocks_y * Knowledge.domain_numBlocks_x} + floor(${pos.cpp}.y / ${Knowledge.domain_numFragsPerBlock_y}) * ${Knowledge.domain_numBlocks_x} + floor(${pos.cpp}.x / ${Knowledge.domain_numFragsPerBlock_x}))");
  }
}

case class AssertStatement(var check : Expression, var msg : Expression, var abort : Statement) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = AssertStatement\n";

  override def expand(collector : StackCollector) : ConditionStatement = {
    new ConditionStatement(check,
      ListBuffer[Statement](
        "LOG_ERROR(" ~ msg ~ ");",
        abort));
  }
}

case class ConnectFragments() extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ConnectFragments\n";

  override def expand(collector : StackCollector) : LoopOverFragments = {
    var body = new ListBuffer[Statement];

    // TODO: get these neighbors from the fragment class
    var neighbors : ListBuffer[NeighborInfo] = ListBuffer();
    if (6 == Knowledge.comm_strategyFragment) {
      neighbors += new NeighborInfo(Array(-1, 0, 0), 0);
      neighbors += new NeighborInfo(Array(+1, 0, 0), 1);
      if (Knowledge.dimensionality > 1) {
        neighbors += new NeighborInfo(Array(0, -1, 0), 2);
        neighbors += new NeighborInfo(Array(0, +1, 0), 3);
      }
      if (Knowledge.dimensionality > 2) {
        neighbors += new NeighborInfo(Array(0, 0, -1), 4);
        neighbors += new NeighborInfo(Array(0, 0, +1), 5);
      }
    } else if (26 == Knowledge.comm_strategyFragment) {
      var i = 0;
      for (
        z <- (if (Knowledge.dimensionality > 2) (-1 to 1) else (0 to 0));
        y <- (if (Knowledge.dimensionality > 1) (-1 to 1) else (0 to 0));
        x <- -1 to 1;
        if (0 != x || 0 != y || 0 != z)
      ) {
        neighbors += new NeighborInfo(Array(x, y, z), i);
        i += 1;
      }
    }

    for (neigh <- neighbors) {
      body += new Scope(ListBuffer(
        s"Vec3 offsetPos = curFragment.pos + Vec3(${neigh.dir(0)}, ${neigh.dir(1)}, ${neigh.dir(2)});",
        new ConditionStatement(s"mpiRank ==" ~ PointToOwningRank("offsetPos"),
          s"curFragment.connectLocalElement(${neigh.index}, fragmentMap[" ~ PointToFragmentId("offsetPos") ~ "]);",
          new ConditionStatement(PointInsideDomain(s"offsetPos"),
            s"curFragment.connectRemoteElement(${neigh.index}," ~ PointToFragmentId("offsetPos").cpp ~ ","
              ~ PointToOwningRank("offsetPos") ~ s");"))));
    }

    return new LoopOverFragments(body) with OMP_PotentiallyParallel;
  }
}

case class SetupBuffers() extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = SetupBuffers\n";

  override def expand(collector : StackCollector) : LoopOverFragments = {
    new LoopOverFragments("curFragment.setupBuffers();") with OMP_PotentiallyParallel;
  }
}

case class ValidatePrimitives() extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = ValidatePrimitives\n";

  //  TODO
  //  override def expand(collector : StackCollector) : LoopOverFragments = {
  //    new LoopOverFragments("curFragment.validate();") with OMP_PotentiallyParallel;
  //  }
  override def expand(collector : StackCollector) : NullStatement = {
    NullStatement();
  }
}

case class InitGeneratedDomain() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = InitGeneratedDomain\n";

  override def expand(collector : StackCollector) : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"initDomain", ListBuffer(),
      ListBuffer(
        new MPI_SetRankAndSize,

        AssertStatement(s"mpiSize != ${Knowledge.domain_numBlocks}",
          "\"Invalid number of MPI processes (\" << mpiSize << \") should be \" << " + (Knowledge.domain_numBlocks),
          "return;"),

        "std::map<size_t, Fragment3DCube*> fragmentMap;",

        s"Vec3 positions[${Knowledge.domain_numFragsPerBlock}];",
        s"unsigned int posWritePos = 0;",
        s"Vec3 rankPos(mpiRank % ${Knowledge.domain_numBlocks_x}, (mpiRank / ${Knowledge.domain_numBlocks_x}) % ${Knowledge.domain_numBlocks_y}, mpiRank / ${Knowledge.domain_numBlocks_x * Knowledge.domain_numBlocks_y});",
        new LoopOverDimensions(IndexRange(MultiIndex(0, 0, 0), MultiIndex(Knowledge.domain_numFragsPerBlock_x - 1, Knowledge.domain_numFragsPerBlock_y - 1, Knowledge.domain_numFragsPerBlock_z - 1)),
          Knowledge.dimensionality. /*FIXME*/ toInt match { // FIXME: avoid match, write in a dimensionless matter
            case 1 => s"positions[posWritePos++] = (Vec3(rankPos.x * ${Knowledge.domain_numFragsPerBlock_x} + 0.5 + x, 0, 0);";
            case 2 => s"positions[posWritePos++] = (Vec3(rankPos.x * ${Knowledge.domain_numFragsPerBlock_x} + 0.5 + x, rankPos.y * ${Knowledge.domain_numFragsPerBlock_y} + 0.5 + y, 0));";
            case 3 => s"positions[posWritePos++] = (Vec3(rankPos.x * ${Knowledge.domain_numFragsPerBlock_x} + 0.5 + x, rankPos.y * ${Knowledge.domain_numFragsPerBlock_y} + 0.5 + y, rankPos.z * ${Knowledge.domain_numFragsPerBlock_z} + 0.5 + z));";
          }),
        LoopOverFragments(ListBuffer(
          "fragments[f] = new Fragment3DCube();",
          "fragments[f]->id = " ~ PointToFragmentId("positions[f]") ~ ";",
          "fragments[f]->pos = positions[f];",
          "fragmentMap[" ~ PointToFragmentId("positions[f]").cpp ~ s"] = fragments[f];"),
          false),
        ConnectFragments(),
        new SetupBuffers));
  }
}

case class DomainGenerated() extends Node with FilePrettyPrintable {
  var statements_HACK : ListBuffer[Statement] = new ListBuffer;

  statements_HACK += new InitGeneratedDomain;

  override def printToFile = {
    val writer = PrettyprintingManager.getPrinter(s"Domains/DomainGenerated.h");

    // TODO: add header guard semi-automatic
    // TODO: add includes to class node or similar
    writer << (
      "#ifndef DOMAINS_DOMAINGENERATED_H\n"
      + "#define DOMAINS_DOMAINGENERATED_H\n"
      + "#include <map>\n"
      + "#include \"Util/Log.h\"\n"
      + "#include \"Util/Vector.h\"\n");

    for (s <- statements_HACK)
      writer << s.cpp + "\n";

    writer << "#endif // DOMAINS_DOMAINGENERATED_H\n";
  }
}
