package exastencils.domain

import java.io.PrintWriter
import java.io.File

import scala.collection.mutable.ListBuffer

import exastencils.core._

import exastencils.knowledge._

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

import exastencils.primitives._

case class PointOutsideDomain(var pos : Expression) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    (s"(${pos.cpp}.x < 0.0 || ${pos.cpp}.x > numFrag.x || ${pos.cpp}.y < 0.0 || ${pos.cpp}.y > numFrag.y || ${pos.cpp}.z < 0.0 || ${pos.cpp}.z > numFrag.z)");
  }
}

case class PointInsideDomain(var pos : Expression) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    (s"(${pos.cpp}.x >= 0.0 && ${pos.cpp}.x <= numFrag.x && ${pos.cpp}.y >= 0.0 && ${pos.cpp}.y <= numFrag.y && ${pos.cpp}.z >= 0.0 && ${pos.cpp}.z <= numFrag.z)");
  }
}

case class PointToFragmentId(var pos : Expression) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    (s"((exa_id_t)(floor(${pos.cpp}.z) * numFrag.y * numFrag.x + floor(${pos.cpp}.y) * numFrag.x + floor(${pos.cpp}.x)))");
  }
}

case class PointToOwningRank(var pos : Expression) extends Expression with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PointToOwningRank\n";

  override def expand : Expression = { // FIXME: use tenary op node
    (s"((" + PointOutsideDomain(pos).cpp + ")"
      + s" ? (-1)" // FIXME: use MPI symbolic constant
      + s" : ((int)(floor(${pos.cpp}.z / lenRank.z) * numRank.y * numRank.x + floor(${pos.cpp}.y / lenRank.y) * numRank.x + floor(${pos.cpp}.x / lenRank.x))))");
  }
}

case class AssertStatement(var check : Expression, var msg : Expression, var abort : Statement) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = InitGeneratedDomain\n";

  override def expand : ConditionStatement = {
    new ConditionStatement(check,
      ListBuffer[Statement](
        s"LOG_ERROR(${msg.cpp});", // FIXME: use composition node instead of cpp
        abort));
  }
}

case class ConnectFragments() extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = InitGeneratedDomain\n";

  override def expand : LoopOverFragments = {
    var body = new ListBuffer[Statement];

    // TODO: get actual neighbors
    for (z <- -1 to 1; y <- -1 to 1; x <- -1 to 1; if (0 != x || 0 != y || 0 != z)) {
      val index = (z + 1) * 9 + (y + 1) * 3 + (x + 1);
      body += new Scope(ListBuffer(
        s"Vec3 offsetPos = curFragment.pos + Vec3($x, $y, $z);",
        s"int owningRank = ", /*This will not be expanded...*/ PointToOwningRank("offsetPos"), ";", // FIXME: include this stuff in the next line
        new ConditionStatement(s"mpiRank == owningRank",
          s"curFragment.connectLocalElement($index, fragmentMap["
            + PointToFragmentId("offsetPos").cpp /* FIXME: combination node */
            + s"]);",
          new ConditionStatement(PointInsideDomain(s"offsetPos"),
            s"curFragment.connectRemoteElement($index, "
              + PointToFragmentId("offsetPos").cpp /* FIXME: combination node */
              + s", "
              + PointToOwningRank("offsetPos").expand.cpp
              + s");"))));
    }

    return new LoopOverFragments(body);
  }
}

case class InitGeneratedDomain() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = InitGeneratedDomain\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"initGeneratedDomain",
      ListBuffer(Variable("std::vector<boost::shared_ptr<Fragment3DCube> >&", "fragments"), Variable("const Vec3u&", "numBlocks"), Variable("const Vec3u&", "numFragmentsPerBlock")),
      ListBuffer(
        "Vec3u numFrag	= numBlocks * numFragmentsPerBlock;",
        "Vec3u numRank	= numBlocks;",
        "Vec3 lenRank	= Vec3(numFragmentsPerBlock.x, numFragmentsPerBlock.y, numFragmentsPerBlock.z);",
        "int	mpiRank;",
        "int			numMpiProc;",
        "MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);",
        "MPI_Comm_size(MPI_COMM_WORLD, &numMpiProc);",
        AssertStatement("numMpiProc != numBlocks.componentProd()",
          "\"Invalid number of MPI processes (\" << numMpiProc << \") should be \" << numBlocks.componentProd()",
          "return;"),

        "std::map<exa_id_t, boost::shared_ptr<Fragment3DCube> >		fragmentMap;",

        "std::vector<Vec3> positions;",
        "Vec3u rankPos(mpiRank % numRank.x, (mpiRank / numRank.x) % numRank.y, mpiRank / (numRank.x * numRank.y));",

        "for (exa_real_t z = rankPos.z * lenRank.z + 0.5; z <= (1 + rankPos.z) * lenRank.z - 0.5; z += 1.0)",
        "for (exa_real_t y = rankPos.y * lenRank.y + 0.5; y <= (1 + rankPos.y) * lenRank.y - 0.5; y += 1.0)",
        "for (exa_real_t x = rankPos.x * lenRank.x + 0.5; x <= (1 + rankPos.x) * lenRank.x - 0.5; x += 1.0)",
        "positions.push_back(Vec3(x, y, z));",

        "fragments.reserve(positions.size());",
        "#pragma omp parallel for schedule(static, 1) ordered", //TODO: integrate into the following loop via traits
        ForLoopStatement("int e = 0", "e < positions.size()", "++e",
          ListBuffer(
            "boost::shared_ptr<Fragment3DCube> fragment(new Fragment3DCube("
              + PointToFragmentId("positions[e]").cpp /* FIXME: combination node */
              + s", positions[e]));",
            "#	pragma omp ordered",
            "{",
            "fragments.push_back(fragment);",
            "fragmentMap["
              + PointToFragmentId("positions[e]").cpp /* FIXME: combination node */
              + s"] = fragment;",
            "}")),
        ConnectFragments(),
        new LoopOverFragments(
          "fragments[e]->setupBuffers();" // TODO: add validation
          )));
  }
}

case class DomainGenerated() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  var statements_HACK : ListBuffer[Statement] = new ListBuffer;

  statements_HACK += new InitGeneratedDomain;

  override def printToFile = {
    val writerHeader = new PrintWriter(new File(Globals.printPath + s"Domains/DomainGenerated.h"));

    // TODO: add header guard semi-automatic
    // TODO: add includes to class node or similar
    writerHeader.write(
      "#ifndef DOMAINS_DOMAINGENERATED_H\n"
        + "#define DOMAINS_DOMAINGENERATED_H\n"
        + "#include <vector>\n"
        + "#include <map>\n"
        + "#include <boost/shared_ptr.hpp>\n"
        + "#include \"Util/Defines.h\"\n"
        + "#include \"Util/Log.h\"\n"
        + "#include \"Util/TypeDefs.h\"\n"
        + "#include \"Util/Vector.h\"\n");

    writerHeader.write(statements_HACK.map(s => s.cpp).mkString("\n") + "\n");

    writerHeader.write("#endif // DOMAINS_DOMAINGENERATED_H\n");
    writerHeader.close();
  }

}
