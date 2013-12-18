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
    (s"(${pos.cpp}.x < 0.0 || ${pos.cpp}.x > (numBlocks.x * numFragmentsPerBlock.x) || ${pos.cpp}.y < 0.0 || ${pos.cpp}.y > (numBlocks.y * numFragmentsPerBlock.y) || ${pos.cpp}.z < 0.0 || ${pos.cpp}.z > (numBlocks.z * numFragmentsPerBlock.z))");
  }
}

case class PointInsideDomain(var pos : Expression) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    (s"(${pos.cpp}.x >= 0.0 && ${pos.cpp}.x <= (numBlocks.x * numFragmentsPerBlock.x) && ${pos.cpp}.y >= 0.0 && ${pos.cpp}.y <= (numBlocks.y * numFragmentsPerBlock.y) && ${pos.cpp}.z >= 0.0 && ${pos.cpp}.z <= (numBlocks.z * numFragmentsPerBlock.z))");
  }
}

case class PointToFragmentId(var pos : Expression) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    (s"((exa_id_t)(floor(${pos.cpp}.z) * (numBlocks.y * numFragmentsPerBlock.y) * (numBlocks.x * numFragmentsPerBlock.x) + floor(${pos.cpp}.y) * (numBlocks.x * numFragmentsPerBlock.x) + floor(${pos.cpp}.x)))");
  }
}

case class PointToOwningRank(var pos : Expression) extends Expression with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = PointToOwningRank\n";

  override def expand : Expression = { // FIXME: use tenary op node
    (s"((" + PointOutsideDomain(pos).cpp + ")"
      + s" ? (-1)" // FIXME: use MPI symbolic constant
      + s" : ((int)(floor(${pos.cpp}.z / numFragmentsPerBlock.z) * numBlocks.y * numBlocks.x + floor(${pos.cpp}.y / numFragmentsPerBlock.y) * numBlocks.x + floor(${pos.cpp}.x / numFragmentsPerBlock.x))))");
  }
}

case class AssertStatement(var check : Expression, var msg : Expression, var abort : Statement) extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = AssertStatement\n";

  override def expand : ConditionStatement = {
    new ConditionStatement(check,
      ListBuffer[Statement](
        "LOG_ERROR(" ~ msg ~ ");",
        abort));
  }
}

case class ConnectFragments() extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = ConnectFragments\n";

  override def expand : LoopOverFragments = {
    var body = new ListBuffer[Statement];

    // TODO: get actual neighbors
    for (z <- -1 to 1; y <- -1 to 1; x <- -1 to 1; if (0 != x || 0 != y || 0 != z)) {
      val index = (z + 1) * 9 + (y + 1) * 3 + (x + 1);
      body += new Scope(ListBuffer(
        s"Vec3 offsetPos = curFragment.pos + Vec3($x, $y, $z);",
        new ConditionStatement(s"mpiRank ==" ~ PointToOwningRank("offsetPos"),
          s"curFragment.connectLocalElement($index, fragmentMap[" ~ PointToFragmentId("offsetPos") ~ "]);",
          new ConditionStatement(PointInsideDomain(s"offsetPos"),
            s"curFragment.connectRemoteElement($index," ~ PointToFragmentId("offsetPos").cpp ~ ","
              ~ PointToOwningRank("offsetPos") ~ s");"))));
    }

    return new LoopOverFragments(body);
  }
}

case class SetupBuffers() extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = SetupBuffers\n";

  override def expand : LoopOverFragments = {
    new LoopOverFragments("curFragment.setupBuffers();");
  }
}

case class ValidatePrimitives() extends Statement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = ValidatePrimitives\n";

  //  TODO
  //  override def expand : LoopOverFragments = {
  //    new LoopOverFragments("curFragment.validate();");
  //  }
  override def expand : NullStatement = {
    NullStatement();
  }
}

case class InitGeneratedDomain() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = InitGeneratedDomain\n";

  override def expand : FunctionStatement = {
    FunctionStatement(new UnitDatatype(), s"initGeneratedDomain",
      ListBuffer(Variable("std::vector<boost::shared_ptr<Fragment3DCube> >&", "fragments"), Variable("const Vec3u&", "numBlocks"), Variable("const Vec3u&", "numFragmentsPerBlock")),
      ListBuffer(
        // TODO: move to specialized node
        "int mpiRank;",
        "int numMpiProc;",
        "MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);",
        "MPI_Comm_size(MPI_COMM_WORLD, &numMpiProc);",

        AssertStatement("numMpiProc != numBlocks.componentProd()",
          "\"Invalid number of MPI processes (\" << numMpiProc << \") should be \" << numBlocks.componentProd()",
          "return;"),

        "std::map<exa_id_t, boost::shared_ptr<Fragment3DCube> > fragmentMap;",

        "std::vector<Vec3> positions;",
        "Vec3 rankPos(mpiRank % numBlocks.x, (mpiRank / numBlocks.x) % numBlocks.y, mpiRank / (numBlocks.x * numBlocks.y));",
        new LoopOverDimensions(IndexRange(Array("0", "0", "0"), Array("numFragmentsPerBlock.x - 1", "numFragmentsPerBlock.y - 1", "numFragmentsPerBlock.z - 1")),
          "positions.push_back(Vec3(rankPos.x * numFragmentsPerBlock.x + 0.5 + x, rankPos.y * numFragmentsPerBlock.y + 0.5 + y, rankPos.z * numFragmentsPerBlock.z + 0.5 + z));"),

        "fragments.reserve(numFragmentsPerBlock.x * numFragmentsPerBlock.y * numFragmentsPerBlock.z);",
        "#pragma omp parallel for schedule(static, 1) ordered", //TODO: integrate into the following loop via traits
        ForLoopStatement("int e = 0", "e < positions.size()", "++e",
          ListBuffer(
            "boost::shared_ptr<Fragment3DCube> fragment(new Fragment3DCube(" ~ PointToFragmentId("positions[e]") ~ s", positions[e]));",
            "#	pragma omp ordered",
            "{",
            "fragments.push_back(fragment);",
            "fragmentMap[" ~ PointToFragmentId("positions[e]").cpp ~ s"] = fragment;",
            "}")),
        ConnectFragments(),
        new SetupBuffers));
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
