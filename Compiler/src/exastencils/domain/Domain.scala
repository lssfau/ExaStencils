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
    (s"(("+ PointOutsideDomain(pos).cpp + ")"
        + s" ? (-1)"	// FIXME: use MPI symbolic constant
        + s" : ((int)(floor(${pos.cpp}.z) * numRank.y * numRank.x + floor(${pos.cpp}.y) * numRank.x + floor(${pos.cpp}.x))))");
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
//        s"int ownRank = ",         PointToOwningRank("offsetPos"), ";",	// FIXME: include this stuff in the next line
//        new ConditionStatement(s"mpiRank == ownRank",
        new ConditionStatement(s"mpiRank == pos2OwnerRank(offsetPos)",
          s"curFragment.connectLocalElement($index, fragmentMap["
            	+ PointToFragmentId("offsetPos").cpp /* FIXME: combination node */
            	+ s"]);",
          new ConditionStatement(PointInsideDomain(s"offsetPos"),
            s"curFragment.connectRemoteElement($index, "
            	+ PointToFragmentId("offsetPos").cpp /* FIXME: combination node */
            	+ s", pos2OwnerRank(offsetPos));"))));
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
        "numFrag	= numBlocks * numFragmentsPerBlock;",
        "numRank	= numBlocks;",
        "lenRank	= Vec3(numFragmentsPerBlock.x, numFragmentsPerBlock.y, numFragmentsPerBlock.z);",
        "int	mpiRank;",
        "int			numMpiProc;",
        "MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);",
        "MPI_Comm_size(MPI_COMM_WORLD, &numMpiProc);",
        AssertStatement("numMpiProc != numBlocks.componentProd()",
          "\"Invalid number of MPI processes (\" << numMpiProc << \") should be \" << numBlocks.componentProd()",
          "return;"),

        "std::map<exa_id_t, boost::shared_ptr<Fragment3DCube> >		fragmentMap;",

        "std::vector<Vec3> positions = rank2FragmentPositions(mpiRank);",
        "fragments.reserve(positions.size());",
        "#pragma omp parallel for schedule(static, 1) ordered", //TODO: integrate into the following loop via traits
        ForLoopStatement("int e = 0", "e < positions.size()", "++e",
          ListBuffer(
            "boost::shared_ptr<Fragment3DCube> fragment(new Fragment3DCube("
            	+ PointToFragmentId("positions[e]").cpp /* FIXME: combination node */
              +s", positions[e]));",
            "#	pragma omp ordered",
            "{",
            "fragments.push_back(fragment);",
            "fragmentMap["
            	+ PointToFragmentId("positions[e]").cpp /* FIXME: combination node */
            +s"] = fragment;",
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

  statements_HACK +=
    """
Vec3	minPos	= Vec3(0.);

Vec3u	numFrag;
Vec3	lenFrag	= Vec3(1.);

Vec3u	numRank;
Vec3	lenRank;

bool		isOutside (const Vec3& pos)
{
	return (pos.x < minPos.x
		|| pos.x > minPos.x + numFrag.x * lenFrag.x
		|| pos.y < minPos.y
		|| pos.y > minPos.y + numFrag.y * lenFrag.y
		|| pos.z < minPos.z
		|| pos.z > minPos.z + numFrag.z * lenFrag.z);
}

int	pos2OwnerRank (const Vec3& pos)
{
	if (isOutside(pos))
		return -1;

	Vec3 rankPos = (pos - minPos) / lenRank;
	return
		(int)floor(rankPos.z) * numRank.y * numRank.x
		+ (int)floor(rankPos.y) * numRank.x
		+ (int)floor(rankPos.x);
}

exa_id_t	pos2FragmentId (const Vec3& pos)
{
	return 
		(exa_id_t)floor((pos.z - minPos.z /*- 0.5 * lenFrag.z*/) / lenFrag.z) * numFrag.y * numFrag.x
		+ (exa_id_t)floor((pos.y - minPos.y /*- 0.5 * lenFrag.y*/) / lenFrag.y) * numFrag.x
		+ (exa_id_t)floor((pos.x - minPos.x /*- 0.5 * lenFrag.x*/) / lenFrag.x);
}

std::vector<Vec3>	rank2FragmentPositions (int mpiRank)
{
	Vec3u rankPos(mpiRank % numRank.x, (mpiRank / numRank.x) % numRank.y, mpiRank / (numRank.x * numRank.y));

	std::vector<Vec3> posVec;
	for (exa_real_t z = minPos.z + rankPos.z * lenRank.z + 0.5 * lenFrag.z; z <= minPos.z + (1 + rankPos.z) * lenRank.z - 0.5 * lenFrag.z; z += lenFrag.z)
		for (exa_real_t y = minPos.y + rankPos.y * lenRank.y + 0.5 * lenFrag.y; y <= minPos.y + (1 + rankPos.y) * lenRank.y - 0.5 * lenFrag.y; y += lenFrag.y)
			for (exa_real_t x = minPos.x + rankPos.x * lenRank.x + 0.5 * lenFrag.x; x <= minPos.x + (1 + rankPos.x) * lenRank.x - 0.5 * lenFrag.x; x += lenFrag.x)
				posVec.push_back(Vec3(x, y, z));

	return posVec;
}
""";

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
