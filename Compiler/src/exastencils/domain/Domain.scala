package exastencils.domain

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.grid._
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.omp._
import exastencils.prettyprinting._
import exastencils.util._

case class PointOutsideDomain(var pos : IR_Access, var domain : Domain) extends IR_Expression with Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = PointOutsideDomain\n"

  override def expand : Output[IR_Expression] = {
    val size = domain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    def posD = Duplicate(pos)
    Knowledge.dimensionality match {
      // case 1 => s"(" ~ ((pos ~ ".x") < size.lower_x) Or ((pos ~ ".x") > size.upper_x) ~ ")"
      case 1 => (MemberAccess(posD, "x") < size.lower_x) Or (MemberAccess(posD, "x") > size.upper_x)
      // case 2 => s"(" ~ ((pos ~ ".x") < size.lower_x) Or ((pos ~ ".x") > size.upper_x) Or
      //   ((pos ~ ".y") < size.lower_y) Or ((pos ~ ".y") > size.upper_y) ~ ")"
      case 2 => (MemberAccess(posD, "x") < size.lower_x) Or (MemberAccess(posD, "x") > size.upper_x) Or
        (MemberAccess(posD, "y") < size.lower_y) Or (MemberAccess(posD, "y") > size.upper_y)
      // case 3 => s"(" ~ ((pos ~ ".x") < size.lower_x) Or ((pos ~ ".x") > size.upper_x) Or
      //   ((pos ~ ".y") < size.lower_y) Or ((pos ~ ".y") > size.upper_y) Or
      //   ((pos ~ ".z") < size.lower_z) Or ((pos ~ ".z") > size.upper_z) ~ ")"
      case 3 => (MemberAccess(posD, "x") < size.lower_x) Or (MemberAccess(posD, "x") > size.upper_x) Or
        (MemberAccess(posD, "y") < size.lower_y) Or (MemberAccess(posD, "y") > size.upper_y) Or
        (MemberAccess(posD, "z") < size.lower_z) Or (MemberAccess(posD, "z") > size.upper_z)
    }
  }
}

case class PointInsideDomain(var pos : IR_Access, var domain : Domain) extends IR_Expression with Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = PointInsideDomain\n"

  override def expand : Output[IR_Expression] = {
    val size = domain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    def posD = Duplicate(pos)
    Knowledge.dimensionality match {
      // case 1 => s"(" ~ ((pos ~ ".x") >= size.lower_x) And ((pos ~ ".x") <= size.upper_x) ~ ")"
      case 1 => (MemberAccess(posD, "x") >= size.lower_x) And (MemberAccess(posD, "x") <= size.upper_x)
      // case 2 => s"(" ~ ((pos ~ ".x") >= size.lower_x) And ((pos ~ ".x") <= size.upper_x) And
      //   ((pos ~ ".y") >= size.lower_y) And ((pos ~ ".y") <= size.upper_y) ~ ")"
      case 2 => (MemberAccess(posD, "x") >= size.lower_x) And (MemberAccess(posD, "x") <= size.upper_x) And
        (MemberAccess(posD, "y") >= size.lower_y) And (MemberAccess(posD, "y") <= size.upper_y)
      // case 3 => s"(" ~ ((pos ~ ".x") >= size.lower_x) And ((pos ~ ".x") <= size.upper_x) And
      //   ((pos ~ ".y") >= size.lower_y) And ((pos ~ ".y") <= size.upper_y) And
      //   ((pos ~ ".z") >= size.lower_z) And ((pos ~ ".z") <= size.upper_z) ~ ")"
      case 3 => (MemberAccess(posD, "x") >= size.lower_x) And (MemberAccess(posD, "x") <= size.upper_x) And
        (MemberAccess(posD, "y") >= size.lower_y) And (MemberAccess(posD, "y") <= size.upper_y) And
        (MemberAccess(posD, "z") >= size.lower_z) And (MemberAccess(posD, "z") <= size.upper_z)
    }
  }
}

case class PointToFragmentId(var pos : IR_Access) extends IR_Expression with Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = PointToFragmentId\n"

  override def expand : Output[IR_Expression] = {
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val gSize = globalDomain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    val fragWidth_x = gSize.width(0) / Knowledge.domain_rect_numFragsTotal_x
    val fragWidth_y = gSize.width(1) / Knowledge.domain_rect_numFragsTotal_y
    val fragWidth_z = gSize.width(2) / Knowledge.domain_rect_numFragsTotal_z

    def posD = Duplicate(pos)
    Knowledge.dimensionality match {
      // case 1 => "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x)
      case 1 => CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "x") - gSize.lower_x) / fragWidth_x))
      // case 2 => "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - gSize.lower_y) / fragWidth_y) * Knowledge.domain_rect_numFragsTotal_x +
      //   "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x)
      case 2 => CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "y") - gSize.lower_y) / fragWidth_y)) * Knowledge.domain_rect_numFragsTotal_x +
        CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "x") - gSize.lower_x) / fragWidth_x))
      // case 3 => "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".z") - gSize.lower_z) / fragWidth_z) * Knowledge.domain_rect_numFragsTotal_y * Knowledge.domain_rect_numFragsTotal_x +
      //   "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - gSize.lower_y) / fragWidth_y) * Knowledge.domain_rect_numFragsTotal_x +
      //   "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x)
      case 3 => CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "z") - gSize.lower_z) / fragWidth_z)) * Knowledge.domain_rect_numFragsTotal_y * Knowledge.domain_rect_numFragsTotal_x +
        CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "y") - gSize.lower_y) / fragWidth_y)) * Knowledge.domain_rect_numFragsTotal_x +
        CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "x") - gSize.lower_x) / fragWidth_x))
    }
  }
}

case class PointToFragmentIndex(var pos : IR_Access) extends IR_Expression with Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = PointToFragmentIndex\n"

  override def expand : Output[IR_Expression] = {
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val gSize = globalDomain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    val fragWidth_x = gSize.width(0) / Knowledge.domain_rect_numFragsTotal_x
    val fragWidth_y = gSize.width(1) / Knowledge.domain_rect_numFragsTotal_y
    val fragWidth_z = gSize.width(2) / Knowledge.domain_rect_numFragsTotal_z

    def posD = Duplicate(pos)
    val entries = Knowledge.dimensionality match {
      case 1 => ListBuffer[IR_Expression](
        // "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x),
        CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "x") - gSize.lower_x) / fragWidth_x)),
        0,
        0)
      case 2 => ListBuffer[IR_Expression](
        // "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x),
        CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "x") - gSize.lower_x) / fragWidth_x)),
        // "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - gSize.lower_y) / fragWidth_y),
        CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "y") - gSize.lower_y) / fragWidth_y)),
        0)
      case 3 => ListBuffer[IR_Expression](
        // "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x),
        CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "x") - gSize.lower_x) / fragWidth_x)),
        // "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - gSize.lower_y) / fragWidth_y),
        CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "y") - gSize.lower_y) / fragWidth_y)),
        // "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".z") - gSize.lower_z) / fragWidth_z))
        CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "z") - gSize.lower_z) / fragWidth_z)))
    }

    // "Vec3i(" ~ entries.reduceLeft((l, r) => l ~ ", " ~ r) ~ ")"
    FunctionCallExpression("Vec3i", entries) // FIXME: Constructor?
  }
}

case class PointToLocalFragmentId(var pos : IR_Access) extends IR_Expression with Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = PointToFragmentId\n"

  override def expand : Output[IR_Expression] = {
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val gSize = globalDomain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    val fragWidth_x = gSize.width(0) / Knowledge.domain_rect_numFragsTotal_x
    val fragWidth_y = gSize.width(1) / Knowledge.domain_rect_numFragsTotal_y
    val fragWidth_z = gSize.width(2) / Knowledge.domain_rect_numFragsTotal_z

    def posD = Duplicate(pos)
    Knowledge.dimensionality match {
      // case 1 => (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x)) Mod Knowledge.domain_rect_numFragsPerBlock_x)
      case 1 => ((CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "x") - gSize.lower_x) / fragWidth_x))) Mod Knowledge.domain_rect_numFragsPerBlock_x)
      // case 2 => (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - gSize.lower_y) / fragWidth_y)) Mod Knowledge.domain_rect_numFragsPerBlock_y) * Knowledge.domain_rect_numFragsPerBlock_x +
      //   (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x)) Mod Knowledge.domain_rect_numFragsPerBlock_x)
      case 2 => ((CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "y") - gSize.lower_y) / fragWidth_y))) Mod Knowledge.domain_rect_numFragsPerBlock_y) * Knowledge.domain_rect_numFragsPerBlock_x +
        ((CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "x") - gSize.lower_x) / fragWidth_x))) Mod Knowledge.domain_rect_numFragsPerBlock_x)
      // case 3 => (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".z") - gSize.lower_z) / fragWidth_z)) Mod Knowledge.domain_rect_numFragsPerBlock_z) * Knowledge.domain_rect_numFragsPerBlock_y * Knowledge.domain_rect_numFragsPerBlock_x +
      //   (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - gSize.lower_y) / fragWidth_y)) Mod Knowledge.domain_rect_numFragsPerBlock_y) * Knowledge.domain_rect_numFragsPerBlock_x +
      //   (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x)) Mod Knowledge.domain_rect_numFragsPerBlock_x)
      case 3 => ((CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "z") - gSize.lower_z) / fragWidth_z))) Mod Knowledge.domain_rect_numFragsPerBlock_z) * Knowledge.domain_rect_numFragsPerBlock_y * Knowledge.domain_rect_numFragsPerBlock_x +
        ((CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "y") - gSize.lower_y) / fragWidth_y))) Mod Knowledge.domain_rect_numFragsPerBlock_y) * Knowledge.domain_rect_numFragsPerBlock_x +
        ((CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(posD, "x") - gSize.lower_x) / fragWidth_x))) Mod Knowledge.domain_rect_numFragsPerBlock_x)
    }
  }
}

case class PointToOwningRank(var pos : IR_Access, var domain : Domain) extends IR_Expression with Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = PointToOwningRank\n"

  override def expand : Output[IR_Expression] = {
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val gSize = globalDomain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    val fragWidth_x = gSize.width(0) / Knowledge.domain_rect_numFragsTotal_x
    val fragWidth_y = gSize.width(1) / Knowledge.domain_rect_numFragsTotal_y
    val fragWidth_z = gSize.width(2) / Knowledge.domain_rect_numFragsTotal_z

    def posD = Duplicate(pos)
    Knowledge.dimensionality match {
      case 1 => TernaryConditionExpression(PointOutsideDomain(pos, domain),
        s"MPI_PROC_NULL",
        // ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x) / Knowledge.domain_rect_numFragsPerBlock_x))
        CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(pos, "x") - gSize.lower_x) / fragWidth_x) / Knowledge.domain_rect_numFragsPerBlock_x))
      case 2 => TernaryConditionExpression(PointOutsideDomain(pos, domain),
        s"MPI_PROC_NULL",
        // ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - gSize.lower_y) / fragWidth_y) / Knowledge.domain_rect_numFragsPerBlock_y) * Knowledge.domain_rect_numBlocks_x
        //   + ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x) / Knowledge.domain_rect_numFragsPerBlock_x))
        CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(pos, "y") - gSize.lower_y) / fragWidth_y) / Knowledge.domain_rect_numFragsPerBlock_y) * Knowledge.domain_rect_numBlocks_x +
          CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(pos, "x") - gSize.lower_x) / fragWidth_x) / Knowledge.domain_rect_numFragsPerBlock_x))
      case 3 => TernaryConditionExpression(PointOutsideDomain(pos, domain),
        s"MPI_PROC_NULL",
        // ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".z") - gSize.lower_z) / fragWidth_z) / Knowledge.domain_rect_numFragsPerBlock_z) * Knowledge.domain_rect_numBlocks_y * Knowledge.domain_rect_numBlocks_x
        //   + ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - gSize.lower_y) / fragWidth_y) / Knowledge.domain_rect_numFragsPerBlock_y) * Knowledge.domain_rect_numBlocks_x
        //   + ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x) / Knowledge.domain_rect_numFragsPerBlock_x))
        CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(pos, "z") - gSize.lower_z) / fragWidth_z) / Knowledge.domain_rect_numFragsPerBlock_z) * Knowledge.domain_rect_numBlocks_y * Knowledge.domain_rect_numBlocks_x +
          CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(pos, "y") - gSize.lower_y) / fragWidth_y) / Knowledge.domain_rect_numFragsPerBlock_y) * Knowledge.domain_rect_numBlocks_x +
          CastExpression(IR_IntegerDatatype, new FunctionCallExpression("floor", (MemberAccess(pos, "x") - gSize.lower_x) / fragWidth_x) / Knowledge.domain_rect_numFragsPerBlock_x))
    }
  }
}

case class ConnectFragments() extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ConnectFragments\n"

  override def expand : Output[LoopOverFragments] = {
    var body = new ListBuffer[IR_Statement]

    val neighbors = exastencils.knowledge.Fragment.neighbors
    val domains = DomainCollection.domains
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val gSize = globalDomain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    for (d <- 0 until domains.size) {
      if (Knowledge.domain_rect_generate) {
        body += IR_Assignment(iv.IsValidForSubdomain(d), PointInsideDomain(iv.PrimitivePosition(), domains(d)))
      } else {
        body += IR_Assignment(iv.IsValidForSubdomain(d), ReadValueFrom(IR_BooleanDatatype, "data"))
      }
    }

    val fragWidth_x = gSize.width(0) / Knowledge.domain_rect_numFragsTotal_x
    val fragWidth_y = gSize.width(1) / Knowledge.domain_rect_numFragsTotal_y
    val fragWidth_z = gSize.width(2) / Knowledge.domain_rect_numFragsTotal_z

    if (Knowledge.domain_canHaveLocalNeighs || Knowledge.domain_canHaveRemoteNeighs || Knowledge.domain_rect_hasPeriodicity) {
      for (neigh <- neighbors) {
        var statements = ListBuffer[IR_Statement]()

        statements += IR_Assignment(s"Vec3 offsetPos",
          iv.PrimitivePosition() + s"Vec3(${ neigh.dir(0) } * ${ fragWidth_x }, ${ neigh.dir(1) } * ${ fragWidth_y }, ${ neigh.dir(2) } * ${ fragWidth_z })")

        if (Knowledge.domain_rect_periodic_x) {
          statements += IR_IfCondition(IR_GreaterExpression("offsetPos.x", gSize.upper_x), IR_Assignment("offsetPos.x", gSize.upper_x - gSize.lower_x, "-="))
          statements += IR_IfCondition(IR_LowerExpression("offsetPos.x", gSize.lower_x), IR_Assignment("offsetPos.x", gSize.upper_x - gSize.lower_x, "+="))
        }
        if (Knowledge.domain_rect_periodic_y) {
          statements += IR_IfCondition(IR_GreaterExpression("offsetPos.y", gSize.upper_y), IR_Assignment("offsetPos.y", gSize.upper_y - gSize.lower_y, "-="))
          statements += IR_IfCondition(IR_LowerExpression("offsetPos.y", gSize.lower_y), IR_Assignment("offsetPos.y", gSize.upper_y - gSize.lower_y, "+="))
        }
        if (Knowledge.domain_rect_periodic_z) {
          statements += IR_IfCondition(IR_GreaterExpression("offsetPos.z", gSize.upper_z), IR_Assignment("offsetPos.z", gSize.upper_z - gSize.lower_z, "-="))
          statements += IR_IfCondition(IR_LowerExpression("offsetPos.z", gSize.lower_z), IR_Assignment("offsetPos.z", gSize.upper_z - gSize.lower_z, "+="))
        }

        // FIXME: datatype for VariableAccesses
        statements ++= (0 until domains.size).toArray[Int].map(d =>
          IR_IfCondition(iv.IsValidForSubdomain(d) AndAnd PointInsideDomain(IR_VariableAccess("offsetPos", None), domains(d)),
            (if (Knowledge.domain_canHaveRemoteNeighs) {
              if (Knowledge.domain_canHaveLocalNeighs)
                IR_IfCondition(IR_EqEqExpression("mpiRank", PointToOwningRank(IR_VariableAccess("offsetPos", None), domains(d))),
                  FunctionCallExpression("connectLocalElement", ListBuffer[IR_Expression](
                    LoopOverFragments.defIt, PointToLocalFragmentId(IR_VariableAccess("offsetPos", None)), neigh.index, d)),
                  FunctionCallExpression("connectRemoteElement", ListBuffer[IR_Expression](
                    LoopOverFragments.defIt, PointToLocalFragmentId(IR_VariableAccess("offsetPos", None)), PointToOwningRank(IR_VariableAccess("offsetPos", None), domains(d)), neigh.index, d))) // FIXME: datatype for VariableAccess
              else
                FunctionCallExpression("connectRemoteElement", ListBuffer[IR_Expression](
                  LoopOverFragments.defIt, PointToLocalFragmentId(IR_VariableAccess("offsetPos", None)), PointToOwningRank(IR_VariableAccess("offsetPos", None), domains(d)), neigh.index, d)) // FIXME: datatype for VariableAccess
            } else {
              FunctionCallExpression("connectLocalElement", ListBuffer[IR_Expression](
                LoopOverFragments.defIt, PointToLocalFragmentId(IR_VariableAccess("offsetPos", None)), neigh.index, d))
            }) : IR_Statement))

        body += IR_Scope(statements)
      }
    }

    new LoopOverFragments(body) with OMP_PotentiallyParallel
  }
}

case class InitGeneratedDomain() extends IR_AbstractFunction with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = InitGeneratedDomain\n"
  override def prettyprint_decl = prettyprint
  override def name = "initDomain"

  override def expand : Output[IR_Function] = {
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val gSize = globalDomain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    val fragWidth_x = gSize.width(0) / Knowledge.domain_rect_numFragsTotal_x
    val fragWidth_y = gSize.width(1) / Knowledge.domain_rect_numFragsTotal_y
    val fragWidth_z = gSize.width(2) / Knowledge.domain_rect_numFragsTotal_z
    // val vecDelta = "Vec3(" ~ (0.5 * fragWidth_x) ~ "," ~ (if (Knowledge.dimensionality > 1) (0.5 * fragWidth_y) else 0) ~ "," ~ (if (Knowledge.dimensionality > 2) (0.5 * fragWidth_z) else 0) ~ ")"
    // FIXME: Constructor?
    val vecDelta = new FunctionCallExpression("Vec3", (0.5 * fragWidth_x), (if (Knowledge.dimensionality > 1) (0.5 * fragWidth_y) else 0), (if (Knowledge.dimensionality > 2) (0.5 * fragWidth_z) else 0))

    var body = ListBuffer[IR_Statement]()

    if (Knowledge.mpi_enabled)
      body += AssertStatement(IR_EqEqExpression(s"mpiSize", Knowledge.domain_numBlocks),
        ListBuffer("\"Invalid number of MPI processes (\"", "mpiSize", "\") should be \"", Knowledge.mpi_numThreads),
        new FunctionCallExpression("exit", 1))

    body ++= ListBuffer(
      s"Vec3 positions[${ Knowledge.domain_numFragmentsPerBlock }]",
      s"unsigned int posWritePos = 0",
      if (Knowledge.mpi_enabled)
        s"Vec3 rankPos(mpiRank % ${ Knowledge.domain_rect_numBlocks_x }, (mpiRank / ${ Knowledge.domain_rect_numBlocks_x }) % ${ Knowledge.domain_rect_numBlocks_y }, mpiRank / ${ Knowledge.domain_rect_numBlocks_x * Knowledge.domain_rect_numBlocks_y })"
      else
        s"Vec3 rankPos(0, 0, 0)")

    body += new LoopOverDimensions(Knowledge.dimensionality, IndexRange(IR_ExpressionIndex(0, 0, 0), IR_ExpressionIndex(Knowledge.domain_rect_numFragsPerBlock_x, Knowledge.domain_rect_numFragsPerBlock_y, Knowledge.domain_rect_numFragsPerBlock_z)),
      new IR_Assignment("positions[posWritePos++]", new FunctionCallExpression("Vec3",
        ((("rankPos.x" : IR_Expression) * Knowledge.domain_rect_numFragsPerBlock_x + 0.5 + dimToString(0)) * fragWidth_x) + gSize.lower_x,
        (if (Knowledge.dimensionality > 1) ((("rankPos.y" : IR_Expression) * Knowledge.domain_rect_numFragsPerBlock_y + 0.5 + dimToString(1)) * fragWidth_y) + gSize.lower_y else 0),
        (if (Knowledge.dimensionality > 2) ((("rankPos.z" : IR_Expression) * Knowledge.domain_rect_numFragsPerBlock_z + 0.5 + dimToString(2)) * fragWidth_z) + gSize.lower_z else 0)))) // FIXME: Constructor?
    body += LoopOverFragments(ListBuffer(
      IR_Assignment(iv.PrimitiveId(), PointToFragmentId(ArrayAccess("positions", LoopOverFragments.defIt))),
      IR_Assignment(iv.PrimitiveIndex(), PointToFragmentIndex(ArrayAccess("positions", LoopOverFragments.defIt))),
      IR_Assignment(iv.CommId(), PointToLocalFragmentId(ArrayAccess("positions", LoopOverFragments.defIt))),
      IR_Assignment(iv.PrimitivePosition(), ArrayAccess("positions", LoopOverFragments.defIt)),
      IR_Assignment(iv.PrimitivePositionBegin(), ArrayAccess("positions", LoopOverFragments.defIt) - vecDelta),
      IR_Assignment(iv.PrimitivePositionEnd(), ArrayAccess("positions", LoopOverFragments.defIt) + vecDelta)))

    body += ConnectFragments()

    body += new IR_ExpressionStatement(new FunctionCallExpression("setupBuffers")) // FIXME: move to app

    IR_Function(IR_UnitDatatype, name, body)
  }
}

case class InitDomainFromFragmentFile() extends IR_AbstractFunction with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = InitGeneratedDomain\n"
  override def prettyprint_decl = prettyprint
  override def name = "initDomain"

  override def expand : Output[IR_Function] = {
    IR_Function(IR_UnitDatatype, name,
      if (Knowledge.mpi_enabled) {
        ListBuffer(
          VariableDeclarationStatement(IR_IntegerDatatype, "numFragments", Some("0")),
          VariableDeclarationStatement(IR_IntegerDatatype, "bufsize", Some("0")),
          VariableDeclarationStatement(IR_IntegerDatatype, "fileOffset", Some("0")),
          AssertStatement(IR_EqEqExpression("mpiSize", Knowledge.mpi_numThreads),
            ListBuffer("\"Invalid number of MPI processes (\"", "mpiSize", "\") should be \"", Knowledge.domain_numBlocks),
            "return"),
          IR_IfCondition("mpiRank == 0",
            ListBuffer[IR_Statement](
              VariableDeclarationStatement(IR_SpecialDatatype("std::ifstream"), "file(\"./Domains/config.dat\", std::ios::binary | std::ios::ate | std::ios::in)"),
              IR_IfCondition(
                "file.is_open()",
                ListBuffer[IR_Statement](
                  VariableDeclarationStatement(IR_IntegerDatatype, "size", Some("file.tellg()")),
                  VariableDeclarationStatement(IR_PointerDatatype("char"), "memblock", Some("new char[size]")),
                  "file.seekg (0, std::ios::beg)",
                  "file.read (memblock, size)",
                  VariableDeclarationStatement(IR_IntegerDatatype, "numRanks", Some(ReadValueFrom(IR_IntegerDatatype, "memblock"))),
                  IR_Assignment("numFragments", ReadValueFrom(IR_IntegerDatatype, "memblock")),
                  IR_Assignment("bufsize", ReadValueFrom(IR_IntegerDatatype, "memblock")),
                  (if (Knowledge.mpi_enabled) VariableDeclarationStatement(IR_IntegerDatatype, "fileOffset", Some("bufsize"))
                  else IR_NullStatement),
                  (if (Knowledge.mpi_enabled) {
                    IR_ForLoop("int i = 1", " i < numRanks ", "++i",
                      VariableDeclarationStatement(IR_IntegerDatatype, "n", Some(ReadValueFrom(IR_IntegerDatatype, "memblock"))),
                      VariableDeclarationStatement(IR_IntegerDatatype, "b", Some(ReadValueFrom(IR_IntegerDatatype, "memblock"))),
                      MPI_Send("&n", "1", IR_IntegerDatatype, "i", 0, "mpiRequest_Send_0[i][0]"),
                      MPI_Send("&fileOffset", "1", IR_IntegerDatatype, "i", 1, "mpiRequest_Send_0[i][1]"),
                      MPI_Send("&b", "1", IR_IntegerDatatype, "i", 2, "mpiRequest_Send_0[i][2]"),
                      IR_Assignment("fileOffset", IR_AdditionExpression("fileOffset", "b")))
                  } else IR_NullStatement),
                  "file.close()"), ListBuffer[IR_Statement]()),
              IR_Assignment("fileOffset", 0)),
            ListBuffer[IR_Statement](
              "MPI_Irecv(&numFragments, 1, MPI_INT, 0, 0, mpiCommunicator, &mpiRequest_Recv_0[mpiRank][0])",
              "MPI_Irecv(&fileOffset, 1, MPI_INT, 0, 1, mpiCommunicator, &mpiRequest_Recv_0[mpiRank][1])",
              "MPI_Irecv(&bufsize, 1, MPI_INT, 0, 2, mpiCommunicator, &mpiRequest_Recv_0[mpiRank][2])")),
          VariableDeclarationStatement(IR_SpecialDatatype("MPI_File"), "fh"),
          "MPI_File_open(mpiCommunicator, \"./Domains/fragments.dat\", MPI_MODE_RDONLY, MPI_INFO_NULL,&fh)",
          VariableDeclarationStatement(IR_CharDatatype, "buf[bufsize]"),
          "MPI_File_read_at(fh, fileOffset, buf, bufsize,MPI_BYTE, MPI_STATUSES_IGNORE)",
          "MPI_Barrier(MPI_COMM_WORLD)",
          "MPI_File_close(&fh)",
          "setValues(buf,numFragments)",
          new IR_ExpressionStatement(new FunctionCallExpression("setupBuffers")))
      } else {
        ListBuffer(
          VariableDeclarationStatement(IR_IntegerDatatype, "numFragments", Some("0")),
          VariableDeclarationStatement(IR_IntegerDatatype, "bufsize", Some("0")),
          VariableDeclarationStatement(IR_IntegerDatatype, "fileOffset", Some("0")),
          VariableDeclarationStatement(IR_SpecialDatatype("std::ifstream"), "file(\"./Domains/config.dat\", std::ios::binary | std::ios::ate | std::ios::in)"),
          IR_IfCondition(
            "file.is_open()",
            ListBuffer[IR_Statement](
              VariableDeclarationStatement(IR_IntegerDatatype, "size", Some("file.tellg()")),
              VariableDeclarationStatement(IR_PointerDatatype("char"), "memblock", Some("new char[size]")),
              "file.seekg (0, std::ios::beg)",
              "file.read (memblock, size)",
              VariableDeclarationStatement(IR_IntegerDatatype, "numRanks", Some(ReadValueFrom(IR_IntegerDatatype, "memblock"))),
              IR_Assignment("numFragments", ReadValueFrom(IR_IntegerDatatype, "memblock")),
              IR_Assignment("bufsize", ReadValueFrom(IR_IntegerDatatype, "memblock")),
              "file.close()"), ListBuffer[IR_Statement]()),
          VariableDeclarationStatement(IR_SpecialDatatype("std::ifstream"), s"""fileFrags("./Domains/fragments.dat", std::ios::binary | std::ios::in)"""),
          VariableDeclarationStatement(IR_CharDatatype, "buf[bufsize]"),
          "fileFrags.read (buf, bufsize)",
          "fileFrags.close()",
          "setValues(buf,numFragments)",
          new IR_ExpressionStatement(new FunctionCallExpression("setupBuffers")))
      })

  }
}

case class SetValues() extends IR_AbstractFunction with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SetValues\n"
  override def prettyprint_decl = prettyprint
  override def name = "setValues"

  override def expand : Output[IR_Function] = {
    var body = new ListBuffer[IR_Statement]
    for (d <- 0 until DomainCollection.domains.size) {
      body += IR_Assignment(iv.IsValidForSubdomain(d), ReadValueFrom(IR_BooleanDatatype, "data"))
    }
    body += IR_Scope(
      IR_Assignment(iv.PrimitiveId(), ReadValueFrom(IR_IntegerDatatype, "data")),
      IR_Assignment(iv.CommId(), ReadValueFrom(IR_IntegerDatatype, "data")),
      IR_ForLoop(VariableDeclarationStatement(IR_IntegerDatatype, "i", Some(0)),
        IR_LowerExpression(IR_VariableAccess("i", Some(IR_IntegerDatatype)), math.pow(2, Knowledge.dimensionality)),
        IR_PreIncrementExpression(IR_VariableAccess("i", Some(IR_IntegerDatatype))),
        // FIXME: Constructor?
        // s"Vec3 vertPos(" ~ ReadValueFrom(RealDatatype, "data") ~ ",0,0)",
        VariableDeclarationStatement(IR_SpecialDatatype("Vec3"), "vertPos", Some(new FunctionCallExpression("Vec3", ReadValueFrom(IR_RealDatatype, "data"), 0, 0))),
        (if (Knowledge.dimensionality == 2) IR_Assignment("vertPos.y", ReadValueFrom(IR_RealDatatype, "data")) else IR_NullStatement),
        (if (Knowledge.dimensionality == 3) IR_Assignment("vertPos.z", ReadValueFrom(IR_RealDatatype, "data")) else IR_NullStatement),
        SwitchStatement("i", ListBuffer(
          CaseStatement("0", ListBuffer(IR_Assignment(iv.PrimitivePositionBegin(), "vertPos"))),
          CaseStatement("1", ListBuffer(IR_Assignment(iv.PrimitivePositionEnd(), "vertPos"))),
          CaseStatement("3", ListBuffer(IR_Assignment(iv.PrimitivePositionEnd(), "vertPos"))),
          CaseStatement("7", ListBuffer(IR_Assignment(iv.PrimitivePositionEnd(), "vertPos")))))),
      // FIXME: Constructor?
      // s"Vec3 fragPos(" ~ ReadValueFrom(RealDatatype, "data") ~ ",0,0)",
      VariableDeclarationStatement(IR_SpecialDatatype("Vec3"), "fragPos", Some(new FunctionCallExpression("Vec3", ReadValueFrom(IR_RealDatatype, "data"), 0, 0))),
      (if (Knowledge.dimensionality == 2) IR_Assignment("fragPos.y", ReadValueFrom(IR_RealDatatype, "data")) else IR_NullStatement),
      (if (Knowledge.dimensionality == 3) IR_Assignment("fragPos.z", ReadValueFrom(IR_RealDatatype, "data")) else IR_NullStatement),
      IR_Assignment(iv.PrimitivePosition(), s"fragPos") //                  VariableDeclarationStatement(IR_IntegerDatatype,"numNeigbours",Some(FunctionCallExpression("readValue<int>",ListBuffer("data")))),
    )
    for (d <- 0 until DomainCollection.domains.size) {
      body += IR_ForLoop("int location = 0", s" location < ${ FragmentCollection.getNumberOfNeighbors() } ", "++location",
        IR_IfCondition(ReadValueFrom(IR_BooleanDatatype, "data"),
          ListBuffer[IR_Statement](//neighbor is valid
            IR_IfCondition(ReadValueFrom(IR_BooleanDatatype, "data"),
              ListBuffer[IR_Statement](//neighbor is remote
                VariableDeclarationStatement(IR_IntegerDatatype, "neighIdx", Some(ReadValueFrom(IR_IntegerDatatype, "data"))),
                VariableDeclarationStatement(IR_IntegerDatatype, "neighRank", Some(ReadValueFrom(IR_IntegerDatatype, "data"))),
                (if (Knowledge.mpi_enabled) s"connectRemoteElement (${ iv.CommId().prettyprint() }, neighIdx, neighRank, location, $d)" else IR_NullStatement)),
              ListBuffer[IR_Statement](//neighbor is local
                VariableDeclarationStatement(IR_IntegerDatatype, "neighIdx", Some(ReadValueFrom(IR_IntegerDatatype, "data"))),
                if (FragmentCollection.fragments.length > 1) s"connectLocalElement(${ iv.CommId().prettyprint() },neighIdx,location,$d)" else IR_NullStatement)))))
    }
    body += IR_IfCondition(ReadValueFrom(IR_BooleanDatatype, "data"),
      ListBuffer[IR_Statement](
        "Mat4 trafoTmp = Mat4()",
        IR_ForLoop("int i = 0", " i < 12 ", "++i",
          IR_Assignment("trafoTmp[i]", ReadValueFrom(IR_RealDatatype, "data"))),
        IR_Assignment(iv.PrimitiveTransformation(), "trafoTmp")))
    IR_Function(IR_UnitDatatype, name,
      ListBuffer[IR_FunctionArgument](IR_FunctionArgument("data", IR_SpecialDatatype("char*")), IR_FunctionArgument("numFragments", IR_IntegerDatatype)),
      //      ListBuffer((LoopOverFragments(body))))
      IR_ForLoop(" int fragmentIdx = 0 ", " fragmentIdx < numFragments ", " ++fragmentIdx ", body))
  }
}

case class DomainFunctions() extends IR_FunctionCollection(
  "Domains/DomainGenerated",
  ListBuffer(),
  ListBuffer("Globals/Globals.h", "Util/Vector.h", "CommFunctions/CommFunctions.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"

  if (Knowledge.domain_rect_generate) {
    functions += new InitGeneratedDomain
    functions += IR_Function(IR_UnitDatatype, "initGeometry", GridGeometry.getGeometry.generateInitCode())
  } else {
    externalDependencies += ("iostream", "fstream")
    val rvTemplateFunc = IR_Function(
      new IR_SpecialDatatype("template <class T> T"),
      s"readValue",
      ListBuffer[IR_FunctionArgument](
        IR_FunctionArgument("memblock", IR_SpecialDatatype("char*&")),
        IR_FunctionArgument("title = \"\"", IR_SpecialDatatype("std::string"))),
      ListBuffer[IR_Statement](
        VariableDeclarationStatement(IR_IntegerDatatype, "size", Some("sizeof(T)")),
        VariableDeclarationStatement(IR_CharDatatype, "bytes[size]"),
        IR_ForLoop("int j = 0", " j < size ", "++j", "bytes[size-1-j] = memblock[j]"),
        "memblock+=size",
        IR_Return("*(T *)&bytes")))
    rvTemplateFunc.isHeaderOnly = true // annotate("isTemplate")
    functions += rvTemplateFunc
    functions += new SetValues
    functions += new InitDomainFromFragmentFile
  }
}
