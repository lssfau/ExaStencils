package exastencils.domain

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.omp._
import exastencils.util._

case class PointOutsideDomain(var pos : Expression, var domain : Domain) extends Expression with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = PointOutsideDomain\n"

  override def expand : Output[Expression] = {
    Knowledge.dimensionality match {
      case 1 => s"(" ~ ((pos ~ ".x") < domain.size.lower_x) Or ((pos ~ ".x") > domain.size.upper_x) ~ ")"
      case 2 => s"(" ~ ((pos ~ ".x") < domain.size.lower_x) Or ((pos ~ ".x") > domain.size.upper_x) Or
        ((pos ~ ".y") < domain.size.lower_y) Or ((pos ~ ".y") > domain.size.upper_y) ~ ")"
      case 3 => s"(" ~ ((pos ~ ".x") < domain.size.lower_x) Or ((pos ~ ".x") > domain.size.upper_x) Or
        ((pos ~ ".y") < domain.size.lower_y) Or ((pos ~ ".y") > domain.size.upper_y) Or
        ((pos ~ ".z") < domain.size.lower_z) Or ((pos ~ ".z") > domain.size.upper_z) ~ ")"
    }
  }
}

case class PointInsideDomain(var pos : Expression, var domain : Domain) extends Expression with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = PointInsideDomain\n"

  override def expand : Output[Expression] = {
    Knowledge.dimensionality match {
      case 1 => s"(" ~ ((pos ~ ".x") >= domain.size.lower_x) And ((pos ~ ".x") <= domain.size.upper_x) ~ ")"
      case 2 => s"(" ~ ((pos ~ ".x") >= domain.size.lower_x) And ((pos ~ ".x") <= domain.size.upper_x) And
        ((pos ~ ".y") >= domain.size.lower_y) And ((pos ~ ".y") <= domain.size.upper_y) ~ ")"
      case 3 => s"(" ~ ((pos ~ ".x") >= domain.size.lower_x) And ((pos ~ ".x") <= domain.size.upper_x) And
        ((pos ~ ".y") >= domain.size.lower_y) And ((pos ~ ".y") <= domain.size.upper_y) And
        ((pos ~ ".z") >= domain.size.lower_z) And ((pos ~ ".z") <= domain.size.upper_z) ~ ")"
    }
  }
}

case class PointToFragmentId(var pos : Expression) extends Expression with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = PointToFragmentId\n"

  override def expand : Output[Expression] = {
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val fragWidth_x = globalDomain.size.width(0) / Knowledge.domain_numFragsTotal_x
    val fragWidth_y = globalDomain.size.width(1) / Knowledge.domain_numFragsTotal_y
    val fragWidth_z = globalDomain.size.width(2) / Knowledge.domain_numFragsTotal_z

    Knowledge.dimensionality match {
      case 1 => "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - globalDomain.size.lower_x) / fragWidth_x)
      case 2 => "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - globalDomain.size.lower_y) / fragWidth_y) * Knowledge.domain_numFragsTotal_x +
        "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - globalDomain.size.lower_x) / fragWidth_x)
      case 3 => "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".z") - globalDomain.size.lower_z) / fragWidth_z) * Knowledge.domain_numFragsTotal_y * Knowledge.domain_numFragsTotal_x +
        "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - globalDomain.size.lower_y) / fragWidth_y) * Knowledge.domain_numFragsTotal_x +
        "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - globalDomain.size.lower_x) / fragWidth_x)
    }
  }
}

case class PointToLocalFragmentId(var pos : Expression) extends Expression with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = PointToFragmentId\n"

  override def expand : Output[Expression] = {
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val fragWidth_x = globalDomain.size.width(0) / Knowledge.domain_numFragsTotal_x
    val fragWidth_y = globalDomain.size.width(1) / Knowledge.domain_numFragsTotal_y
    val fragWidth_z = globalDomain.size.width(2) / Knowledge.domain_numFragsTotal_z

    Knowledge.dimensionality match {
      case 1 => (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - globalDomain.size.lower_x) / fragWidth_x)) Mod Knowledge.domain_numFragsPerBlock_x)
      case 2 => (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - globalDomain.size.lower_y) / fragWidth_y)) Mod Knowledge.domain_numFragsPerBlock_y) * Knowledge.domain_numFragsPerBlock_x +
        (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - globalDomain.size.lower_x) / fragWidth_x)) Mod Knowledge.domain_numFragsPerBlock_x)
      case 3 => (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".z") - globalDomain.size.lower_z) / fragWidth_z)) Mod Knowledge.domain_numFragsPerBlock_z) * Knowledge.domain_numFragsPerBlock_y * Knowledge.domain_numFragsPerBlock_x +
        (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - globalDomain.size.lower_y) / fragWidth_y)) Mod Knowledge.domain_numFragsPerBlock_y) * Knowledge.domain_numFragsPerBlock_x +
        (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - globalDomain.size.lower_x) / fragWidth_x)) Mod Knowledge.domain_numFragsPerBlock_x)
    }
  }
}

case class PointToOwningRank(var pos : Expression, var domain : Domain) extends Expression with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = PointToOwningRank\n"

  override def expand : Output[Expression] = {
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val fragWidth_x = globalDomain.size.width(0) / Knowledge.domain_numFragsTotal_x
    val fragWidth_y = globalDomain.size.width(1) / Knowledge.domain_numFragsTotal_y
    val fragWidth_z = globalDomain.size.width(2) / Knowledge.domain_numFragsTotal_z

    Knowledge.dimensionality match {
      case 1 => TernaryConditionExpression(PointOutsideDomain(pos, domain),
        s"MPI_PROC_NULL",
        ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - globalDomain.size.lower_x) / fragWidth_x) / Knowledge.domain_numFragsPerBlock_x))
      case 2 => TernaryConditionExpression(PointOutsideDomain(pos, domain),
        s"MPI_PROC_NULL",
        ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - globalDomain.size.lower_y) / fragWidth_y) / Knowledge.domain_numFragsPerBlock_y) * Knowledge.domain_numBlocks_x
          + ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - globalDomain.size.lower_x) / fragWidth_x) / Knowledge.domain_numFragsPerBlock_x))
      case 3 => TernaryConditionExpression(PointOutsideDomain(pos, domain),
        s"MPI_PROC_NULL",
        ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".z") - globalDomain.size.lower_z) / fragWidth_z) / Knowledge.domain_numFragsPerBlock_z) * Knowledge.domain_numBlocks_y * Knowledge.domain_numBlocks_x
          + ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - globalDomain.size.lower_y) / fragWidth_y) / Knowledge.domain_numFragsPerBlock_y) * Knowledge.domain_numBlocks_x
          + ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - globalDomain.size.lower_x) / fragWidth_x) / Knowledge.domain_numFragsPerBlock_x))
    }
  }
}

case class AssertStatement(var check : Expression, var msg : Expression, var abort : Statement) extends Statement with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = AssertStatement\n"

  override def expand : Output[ConditionStatement] = {
    new ConditionStatement(check,
      ListBuffer[Statement](new PrintStatement(msg), abort))
  }
}

case class ConnectFragments() extends Statement with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = ConnectFragments\n"

  override def expand : Output[LoopOverFragments] = {
    var body = new ListBuffer[Statement]

    val neighbors = Fragment.neighbors
    val domains = DomainCollection.domains
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get

    for (d <- 0 until domains.size) {
      body += AssignmentStatement(iv.IsValidForSubdomain(d), PointInsideDomain(iv.PrimitivePosition(), domains(d)))
    }

    val fragWidth_x = globalDomain.size.width(0) / Knowledge.domain_numFragsTotal_x
    val fragWidth_y = globalDomain.size.width(1) / Knowledge.domain_numFragsTotal_y
    val fragWidth_z = globalDomain.size.width(2) / Knowledge.domain_numFragsTotal_z

    if (Knowledge.domain_canHaveLocalNeighs || Knowledge.domain_canHaveRemoteNeighs) {
      for (neigh <- neighbors) {
        body += new Scope(ListBuffer[Statement](
          AssignmentStatement(s"Vec3 offsetPos", iv.PrimitivePosition() + s"Vec3(${neigh.dir(0)} * ${fragWidth_x}, ${neigh.dir(1)} * ${fragWidth_y}, ${neigh.dir(2)} * ${fragWidth_z})")) ++
          (0 until domains.size).toArray[Int].map(d =>
            new ConditionStatement(iv.IsValidForSubdomain(d) AndAnd PointInsideDomain(s"offsetPos", domains(d)),
              (if (Knowledge.domain_canHaveRemoteNeighs) {
                if (Knowledge.domain_canHaveLocalNeighs)
                  new ConditionStatement(s"mpiRank ==" ~ PointToOwningRank("offsetPos", domains(d)),
                    FunctionCallExpression("connectLocalElement", ListBuffer[Expression](
                      LoopOverFragments.defIt, PointToLocalFragmentId("offsetPos"), neigh.index, d)),
                    FunctionCallExpression("connectRemoteElement", ListBuffer[Expression](
                      LoopOverFragments.defIt, PointToLocalFragmentId("offsetPos"), PointToOwningRank("offsetPos", domains(d)), neigh.index, d)))
                else
                  FunctionCallExpression("connectRemoteElement", ListBuffer[Expression](
                    LoopOverFragments.defIt, PointToLocalFragmentId("offsetPos"), PointToOwningRank("offsetPos", domains(d)), neigh.index, d))
              } else {
                FunctionCallExpression("connectLocalElement", ListBuffer[Expression](
                  LoopOverFragments.defIt, PointToLocalFragmentId("offsetPos"), neigh.index, d))
              }) : Statement)))
      }
    }

    new LoopOverFragments(body) with OMP_PotentiallyParallel
  }
}

case class InitGeneratedDomain() extends AbstractFunctionStatement with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = InitGeneratedDomain\n"
  override def cpp_decl = cpp

  override def expand : Output[FunctionStatement] = {
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val fragWidth_x = globalDomain.size.width(0) / Knowledge.domain_numFragsTotal_x
    val fragWidth_y = globalDomain.size.width(1) / Knowledge.domain_numFragsTotal_y
    val fragWidth_z = globalDomain.size.width(2) / Knowledge.domain_numFragsTotal_z
    val vecDelta = "Vec3(" ~ (0.5 * fragWidth_x) ~ "," ~ (if (Knowledge.dimensionality > 1) (0.5 * fragWidth_y) else 0) ~ "," ~ (if (Knowledge.dimensionality > 2) (0.5 * fragWidth_z) else 0) ~ ")"

    FunctionStatement(new UnitDatatype(), s"initDomain", ListBuffer(),
      ListBuffer(
        if (Knowledge.useMPI)
          AssertStatement(s"mpiSize != ${Knowledge.domain_numBlocks}",
          "\"Invalid number of MPI processes (\" << mpiSize << \") should be \" << " + (Knowledge.domain_numBlocks),
          "return")
        else
          NullStatement,

        s"Vec3 positions[${Knowledge.domain_numFragsPerBlock}]",
        s"unsigned int posWritePos = 0",
        if (Knowledge.useMPI)
          s"Vec3 rankPos(mpiRank % ${Knowledge.domain_numBlocks_x}, (mpiRank / ${Knowledge.domain_numBlocks_x}) % ${Knowledge.domain_numBlocks_y}, mpiRank / ${Knowledge.domain_numBlocks_x * Knowledge.domain_numBlocks_y})"
        else
          s"Vec3 rankPos(0, 0, 0)",

        new LoopOverDimensions(Knowledge.dimensionality, IndexRange(MultiIndex(0, 0, 0), MultiIndex(Knowledge.domain_numFragsPerBlock_x, Knowledge.domain_numFragsPerBlock_y, Knowledge.domain_numFragsPerBlock_z)),
          new AssignmentStatement("positions[posWritePos++]", "Vec3("
            ~ ((("rankPos.x" : Expression) * Knowledge.domain_numFragsPerBlock_x + 0.5 + dimToString(0)) * fragWidth_x) + globalDomain.size.lower_x ~ ","
            ~ (if (Knowledge.dimensionality > 1) ((("rankPos.y" : Expression) * Knowledge.domain_numFragsPerBlock_y + 0.5 + dimToString(1)) * fragWidth_y) + globalDomain.size.lower_y else 0) ~ ","
            ~ (if (Knowledge.dimensionality > 2) ((("rankPos.z" : Expression) * Knowledge.domain_numFragsPerBlock_z + 0.5 + dimToString(2)) * fragWidth_z) + globalDomain.size.lower_z else 0) ~ ")")),
        LoopOverFragments(ListBuffer(
          AssignmentStatement(iv.PrimitiveId(), PointToFragmentId(s"positions[${LoopOverFragments.defIt}]")),
          AssignmentStatement(iv.CommId(), PointToLocalFragmentId(s"positions[${LoopOverFragments.defIt}]")),
          AssignmentStatement(iv.PrimitivePosition(), s"positions[${LoopOverFragments.defIt}]"),
          AssignmentStatement(iv.PrimitivePositionBegin(), s"positions[${LoopOverFragments.defIt}]" - vecDelta),
          AssignmentStatement(iv.PrimitivePositionEnd(), (s"positions[${LoopOverFragments.defIt}]" : Expression) + vecDelta))), // stupid string concat ...
        ConnectFragments(),
        "setupBuffers()" // FIXME: move to app
        ))
  }
}

case class DomainFunctions() extends FunctionCollection("Domains/DomainGenerated",
  (if (Knowledge.useMPI) ListBuffer("mpi.h") else ListBuffer()),
  ListBuffer("Globals/Globals.h", "Util/Vector.h", "CommFunctions/CommFunctions.h")) {

  functions += new InitGeneratedDomain
}
