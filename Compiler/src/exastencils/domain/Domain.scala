package exastencils.domain

import scala.collection.mutable.ListBuffer
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.omp._
import exastencils.prettyprinting._
import exastencils.util._
import exastencils.mpi._
import exastencils.communication._
import exastencils.core._
import jeremias.dsl._

case class PointOutsideDomain(var pos: Expression, var domain: Domain) extends Expression with Expandable {
  override def prettyprint(out: PpStream): Unit = out << "NOT VALID ; CLASS = PointOutsideDomain\n"

  override def expand: Output[Expression] = {
    val size = domain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    Knowledge.dimensionality match {
      case 1 => s"(" ~ ((pos ~ ".x") < size.lower_x) Or ((pos ~ ".x") > size.upper_x) ~ ")"
      case 2 => s"(" ~ ((pos ~ ".x") < size.lower_x) Or ((pos ~ ".x") > size.upper_x) Or
        ((pos ~ ".y") < size.lower_y) Or ((pos ~ ".y") > size.upper_y) ~ ")"
      case 3 => s"(" ~ ((pos ~ ".x") < size.lower_x) Or ((pos ~ ".x") > size.upper_x) Or
        ((pos ~ ".y") < size.lower_y) Or ((pos ~ ".y") > size.upper_y) Or
        ((pos ~ ".z") < size.lower_z) Or ((pos ~ ".z") > size.upper_z) ~ ")"
    }
  }
}

case class PointInsideDomain(var pos: Expression, var domain: Domain) extends Expression with Expandable {
  override def prettyprint(out: PpStream): Unit = out << "NOT VALID ; CLASS = PointInsideDomain\n"

  override def expand: Output[Expression] = {
    val size = domain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    Knowledge.dimensionality match {
      case 1 => s"(" ~ ((pos ~ ".x") >= size.lower_x) And ((pos ~ ".x") <= size.upper_x) ~ ")"
      case 2 => s"(" ~ ((pos ~ ".x") >= size.lower_x) And ((pos ~ ".x") <= size.upper_x) And
        ((pos ~ ".y") >= size.lower_y) And ((pos ~ ".y") <= size.upper_y) ~ ")"
      case 3 => s"(" ~ ((pos ~ ".x") >= size.lower_x) And ((pos ~ ".x") <= size.upper_x) And
        ((pos ~ ".y") >= size.lower_y) And ((pos ~ ".y") <= size.upper_y) And
        ((pos ~ ".z") >= size.lower_z) And ((pos ~ ".z") <= size.upper_z) ~ ")"
    }
  }
}

case class PointToFragmentId(var pos: Expression) extends Expression with Expandable {
  override def prettyprint(out: PpStream): Unit = out << "NOT VALID ; CLASS = PointToFragmentId\n"

  override def expand: Output[Expression] = {
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val gSize = globalDomain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    val fragWidth_x = gSize.width(0) / Knowledge.domain_rect_numFragsTotal_x
    val fragWidth_y = gSize.width(1) / Knowledge.domain_rect_numFragsTotal_y
    val fragWidth_z = gSize.width(2) / Knowledge.domain_rect_numFragsTotal_z

    Knowledge.dimensionality match {
      case 1 => "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x)
      case 2 => "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - gSize.lower_y) / fragWidth_y) * Knowledge.domain_rect_numFragsTotal_x +
        "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x)
      case 3 => "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".z") - gSize.lower_z) / fragWidth_z) * Knowledge.domain_rect_numFragsTotal_y * Knowledge.domain_rect_numFragsTotal_x +
        "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - gSize.lower_y) / fragWidth_y) * Knowledge.domain_rect_numFragsTotal_x +
        "(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x)
    }
  }
}

case class PointToLocalFragmentId(var pos: Expression) extends Expression with Expandable {
  override def prettyprint(out: PpStream): Unit = out << "NOT VALID ; CLASS = PointToFragmentId\n"

  override def expand: Output[Expression] = {
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val gSize = globalDomain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    val fragWidth_x = gSize.width(0) / Knowledge.domain_rect_numFragsTotal_x
    val fragWidth_y = gSize.width(1) / Knowledge.domain_rect_numFragsTotal_y
    val fragWidth_z = gSize.width(2) / Knowledge.domain_rect_numFragsTotal_z

    Knowledge.dimensionality match {
      case 1 => (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x)) Mod Knowledge.domain_rect_numFragsPerBlock_x)
      case 2 => (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - gSize.lower_y) / fragWidth_y)) Mod Knowledge.domain_rect_numFragsPerBlock_y) * Knowledge.domain_rect_numFragsPerBlock_x +
        (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x)) Mod Knowledge.domain_rect_numFragsPerBlock_x)
      case 3 => (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".z") - gSize.lower_z) / fragWidth_z)) Mod Knowledge.domain_rect_numFragsPerBlock_z) * Knowledge.domain_rect_numFragsPerBlock_y * Knowledge.domain_rect_numFragsPerBlock_x +
        (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - gSize.lower_y) / fragWidth_y)) Mod Knowledge.domain_rect_numFragsPerBlock_y) * Knowledge.domain_rect_numFragsPerBlock_x +
        (("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x)) Mod Knowledge.domain_rect_numFragsPerBlock_x)
    }
  }
}

case class PointToOwningRank(var pos: Expression, var domain: Domain) extends Expression with Expandable {
  override def prettyprint(out: PpStream): Unit = out << "NOT VALID ; CLASS = PointToOwningRank\n"

  override def expand: Output[Expression] = {
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val gSize = globalDomain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    val fragWidth_x = gSize.width(0) / Knowledge.domain_rect_numFragsTotal_x
    val fragWidth_y = gSize.width(1) / Knowledge.domain_rect_numFragsTotal_y
    val fragWidth_z = gSize.width(2) / Knowledge.domain_rect_numFragsTotal_z

    Knowledge.dimensionality match {
      case 1 => TernaryConditionExpression(PointOutsideDomain(pos, domain),
        s"MPI_PROC_NULL",
        ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x) / Knowledge.domain_rect_numFragsPerBlock_x))
      case 2 => TernaryConditionExpression(PointOutsideDomain(pos, domain),
        s"MPI_PROC_NULL",
        ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - gSize.lower_y) / fragWidth_y) / Knowledge.domain_rect_numFragsPerBlock_y) * Knowledge.domain_rect_numBlocks_x
          + ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x) / Knowledge.domain_rect_numFragsPerBlock_x))
      case 3 => TernaryConditionExpression(PointOutsideDomain(pos, domain),
        s"MPI_PROC_NULL",
        ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".z") - gSize.lower_z) / fragWidth_z) / Knowledge.domain_rect_numFragsPerBlock_z) * Knowledge.domain_rect_numBlocks_y * Knowledge.domain_rect_numBlocks_x
          + ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".y") - gSize.lower_y) / fragWidth_y) / Knowledge.domain_rect_numFragsPerBlock_y) * Knowledge.domain_rect_numBlocks_x
          + ("(int)" ~ new FunctionCallExpression("floor", ((pos ~ ".x") - gSize.lower_x) / fragWidth_x) / Knowledge.domain_rect_numFragsPerBlock_x))
    }
  }
}

case class AssertStatement(var check: Expression, var msg: Expression, var abort: Statement) extends Statement with Expandable {
  override def prettyprint(out: PpStream): Unit = out << "NOT VALID ; CLASS = AssertStatement\n"

  override def expand: Output[ConditionStatement] = {
    new ConditionStatement(check,
      ListBuffer[Statement](new PrintStatement(msg), abort))
  }
}

case class ConnectFragments() extends Statement with Expandable {
  override def prettyprint(out: PpStream): Unit = out << "NOT VALID ; CLASS = ConnectFragments\n"

  override def expand: Output[LoopOverFragments] = {
    var body = new ListBuffer[Statement]

    val neighbors = Fragment.neighbors
    val domains = DomainCollection.domains
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val gSize = globalDomain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    for (d <- 0 until domains.size) {
      if (Knowledge.domain_rect_generate) {
        body += AssignmentStatement(iv.IsValidForSubdomain(d), PointInsideDomain(iv.PrimitivePosition(), domains(d)))
      } else {
        body += AssignmentStatement(iv.IsValidForSubdomain(d), ReadValueFrom(new BooleanDatatype, "data"))
      }
    }

    val fragWidth_x = gSize.width(0) / Knowledge.domain_rect_numFragsTotal_x
    val fragWidth_y = gSize.width(1) / Knowledge.domain_rect_numFragsTotal_y
    val fragWidth_z = gSize.width(2) / Knowledge.domain_rect_numFragsTotal_z

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
              }): Statement)))
      }
    }

    new LoopOverFragments(body) with OMP_PotentiallyParallel
  }
}

case class InitGeneratedDomain() extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out: PpStream): Unit = out << "NOT VALID ; CLASS = InitGeneratedDomain\n"
  override def prettyprint_decl = prettyprint

  override def expand: Output[FunctionStatement] = {
    val globalDomain = DomainCollection.getDomainByIdentifier("global").get
    val gSize = globalDomain.asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    val fragWidth_x = gSize.width(0) / Knowledge.domain_rect_numFragsTotal_x
    val fragWidth_y = gSize.width(1) / Knowledge.domain_rect_numFragsTotal_y
    val fragWidth_z = gSize.width(2) / Knowledge.domain_rect_numFragsTotal_z
    val vecDelta = "Vec3(" ~ (0.5 * fragWidth_x) ~ "," ~ (if (Knowledge.dimensionality > 1) (0.5 * fragWidth_y) else 0) ~ "," ~ (if (Knowledge.dimensionality > 2) (0.5 * fragWidth_z) else 0) ~ ")"

    FunctionStatement(new UnitDatatype(), s"initDomain", ListBuffer(),
      ListBuffer(
        if (Knowledge.mpi_enabled)
          AssertStatement(s"mpiSize != ${Knowledge.domain_numBlocks}",
          "\"Invalid number of MPI processes (\" << mpiSize << \") should be \" << " + (Knowledge.mpi_numThreads),
          "exit(1)")
        else
          NullStatement,

        s"Vec3 positions[${Knowledge.domain_numFragmentsPerBlock}]",
        s"unsigned int posWritePos = 0",
        if (Knowledge.mpi_enabled)
          s"Vec3 rankPos(mpiRank % ${Knowledge.domain_rect_numBlocks_x}, (mpiRank / ${Knowledge.domain_rect_numBlocks_x}) % ${Knowledge.domain_rect_numBlocks_y}, mpiRank / ${Knowledge.domain_rect_numBlocks_x * Knowledge.domain_rect_numBlocks_y})"
        else
          s"Vec3 rankPos(0, 0, 0)",

        new LoopOverDimensions(Knowledge.dimensionality, IndexRange(MultiIndex(0, 0, 0), MultiIndex(Knowledge.domain_rect_numFragsPerBlock_x, Knowledge.domain_rect_numFragsPerBlock_y, Knowledge.domain_rect_numFragsPerBlock_z)),
          new AssignmentStatement("positions[posWritePos++]", "Vec3("
            ~ ((("rankPos.x": Expression) * Knowledge.domain_rect_numFragsPerBlock_x + 0.5 + dimToString(0)) * fragWidth_x) + gSize.lower_x ~ ","
            ~ (if (Knowledge.dimensionality > 1) ((("rankPos.y": Expression) * Knowledge.domain_rect_numFragsPerBlock_y + 0.5 + dimToString(1)) * fragWidth_y) + gSize.lower_y else 0) ~ ","
            ~ (if (Knowledge.dimensionality > 2) ((("rankPos.z": Expression) * Knowledge.domain_rect_numFragsPerBlock_z + 0.5 + dimToString(2)) * fragWidth_z) + gSize.lower_z else 0) ~ ")")),
        LoopOverFragments(ListBuffer(
          AssignmentStatement(iv.PrimitiveId(), PointToFragmentId(s"positions[${LoopOverFragments.defIt}]")),
          AssignmentStatement(iv.CommId(), PointToLocalFragmentId(s"positions[${LoopOverFragments.defIt}]")),
          AssignmentStatement(iv.PrimitivePosition(), s"positions[${LoopOverFragments.defIt}]"),
          AssignmentStatement(iv.PrimitivePositionBegin(), s"positions[${LoopOverFragments.defIt}]" - vecDelta),
          AssignmentStatement(iv.PrimitivePositionEnd(), (s"positions[${LoopOverFragments.defIt}]": Expression) + vecDelta))), // stupid string concat ...
        ConnectFragments(),
        "setupBuffers()" // FIXME: move to app
        ))
  }
}

case class InitDomainFromFragmentFile() extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out: PpStream): Unit = out << "NOT VALID ; CLASS = InitGeneratedDomain\n"
  override def prettyprint_decl = prettyprint

  override def expand: Output[FunctionStatement] = {
    FunctionStatement(new UnitDatatype(), s"initDomain", ListBuffer(),
      (if (Knowledge.mpi_enabled) {
        ListBuffer(
          AssertStatement(s"mpiSize != ${Knowledge.mpi_numThreads}",
            "\"Invalid number of MPI processes (\" << mpiSize << \") should be \" << " + (Knowledge.domain_numBlocks),
            "return"),
          ConditionStatement("mpiRank == 0", ListBuffer("readConfig(\"./Domains/config.dat\")"),
            ListBuffer(
              "MPI_Irecv(&numFragments, 1, MPI_INT, 0, 0, mpiCommunicator, &mpiRequest_Recv_0[mpiRank][0])",
              "MPI_Irecv(&fileOffset, 1, MPI_INT, 0, 1, mpiCommunicator, &mpiRequest_Recv_0[mpiRank][1])",
              "MPI_Irecv(&bufsize, 1, MPI_INT, 0, 2, mpiCommunicator, &mpiRequest_Recv_0[mpiRank][2])")),
          VariableDeclarationStatement(SpecialDatatype("MPI_File"), "fh"),
          "MPI_File_open(mpiCommunicator, \"./Domains/fragments.dat\", MPI_MODE_RDONLY, MPI_INFO_NULL,&fh)",
          VariableDeclarationStatement(CharDatatype(), "buf[bufsize]"),
          "MPI_File_read_at(fh, fileOffset, buf, bufsize,MPI_BYTE, MPI_STATUSES_IGNORE)",
          "MPI_Barrier(MPI_COMM_WORLD)",
          "MPI_File_close(&fh)",
          "setValues(buf)",
          "setupBuffers()")
      } else {
        ListBuffer(
          "readConfig(\"./Domains/config.dat\")",
          VariableDeclarationStatement(SpecialDatatype("std::ifstream"), s"""file("./Domains/fragments.dat", std::ios::binary | std::ios::in)"""),
          VariableDeclarationStatement(CharDatatype(), "buf[bufsize]"),
          "file.read (buf, bufsize)",
          "setValues(buf)",
          "setupBuffers()")
      }))

  }
}

case class SetValues() extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out: PpStream): Unit = out << "NOT VALID ; CLASS = SetValues\n"
  override def prettyprint_decl = prettyprint

  override def expand: Output[FunctionStatement] = {
    var body = new ListBuffer[Statement]
    for (d <- 0 until DomainCollection.domains.size) {
      body += AssignmentStatement(iv.IsValidForSubdomain(d), ReadValueFrom(new BooleanDatatype, "data"))
    }
    body += Scope(ListBuffer(
      AssignmentStatement(iv.PrimitiveId(), ReadValueFrom(new IntegerDatatype, "data")),
      AssignmentStatement(iv.CommId(), ReadValueFrom(new IntegerDatatype, "data")),
      //VariableDeclarationStatement(IntegerDatatype(),"numVertices",Some(ReadValueFrom(new IntegerDatatype,"data"))),
      //VariableDeclarationStatement(IntegerDatatype(),"dimensionality",Some(ReadValueFrom(new IntegerDatatype,"data"))),
      ForLoopStatement("int i = 0", " i <" ~ math.pow(2, Knowledge.dimensionality), "++i", ListBuffer(
        //TODO Statement Loop over vertices
        s"Vec3 vertPos(" ~ ReadValueFrom(new RealDatatype, "data") ~ ",0,0)",
        (if (Knowledge.dimensionality == 2) AssignmentStatement("vertPos.y", ReadValueFrom(new RealDatatype, "data")) else NullStatement),
        (if (Knowledge.dimensionality == 3) AssignmentStatement("vertPos.z", ReadValueFrom(new RealDatatype, "data")) else NullStatement),
        SwitchStatement("i", ListBuffer(
          CaseStatement("0", ListBuffer(AssignmentStatement(iv.PrimitivePositionBegin(), "vertPos"))),
          CaseStatement("1", ListBuffer(AssignmentStatement(iv.PrimitivePositionEnd(), "vertPos"))),
          CaseStatement("3", ListBuffer(AssignmentStatement(iv.PrimitivePositionEnd(), "vertPos"))),
          CaseStatement("7", ListBuffer(AssignmentStatement(iv.PrimitivePositionEnd(), "vertPos"))))))),
      s"Vec3 fragPos(" ~ ReadValueFrom(new RealDatatype, "data") ~ ",0,0)",
      (if (Knowledge.dimensionality == 2) AssignmentStatement("fragPos.y", ReadValueFrom(new RealDatatype, "data")) else NullStatement),
      (if (Knowledge.dimensionality == 3) AssignmentStatement("fragPos.z", ReadValueFrom(new RealDatatype, "data")) else NullStatement),
      AssignmentStatement(iv.PrimitivePosition(), s"fragPos") //                  VariableDeclarationStatement(IntegerDatatype(),"numNeigbours",Some(FunctionCallExpression("readValue<int>",ListBuffer("data")))),
      ))
    for (d <- 0 until DomainCollection.domains.size) {
      body += ForLoopStatement("int location = 0", "location <" ~ Knowledge.dimensionality * 2, "++location", ListBuffer(
        ConditionStatement(ReadValueFrom(BooleanDatatype(), "data"),
          ListBuffer( //neighbor is valid
            ConditionStatement(ReadValueFrom(BooleanDatatype(), "data"),
              ListBuffer( //neighbor is remote
                VariableDeclarationStatement(IntegerDatatype(), "neighIdx", Some(ReadValueFrom(new IntegerDatatype, "data"))),
                VariableDeclarationStatement(IntegerDatatype(), "neighRank", Some(ReadValueFrom(new IntegerDatatype, "data"))),
                (if (Knowledge.mpi_enabled) s"connectRemoteElement (commId[fragmentIdx], neighIdx, neighRank, location, $d)" else NullStatement)),
              ListBuffer( //neighbor is local
                VariableDeclarationStatement(IntegerDatatype(), "neighIdx", Some(ReadValueFrom(new IntegerDatatype, "data"))),
                s"connectLocalElement(commId[fragmentIdx],neighIdx,location,$d)"))),
          ListBuffer(
            NullStatement))))
    }
    FunctionStatement(
      new UnitDatatype,
      s"setValues",
      ListBuffer[VariableAccess](VariableAccess("data", Some("char*"))),
      ListBuffer((LoopOverFragments(body))))
  }
}

case class ReadConfig() extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out: PpStream): Unit = out << "NOT VALID ; CLASS = ReadConfig\n"
  override def prettyprint_decl = prettyprint

  override def expand: Output[FunctionStatement] = {
    FunctionStatement(
      new UnitDatatype(),
      s"readConfig",
      ListBuffer(VariableAccess("cName", Some(new StringDatatype()))),
      ListBuffer(
        VariableDeclarationStatement(SpecialDatatype("std::ifstream"), "file(cName, std::ios::binary | std::ios::ate | std::ios::in)"),
        ConditionStatement(
          "file.is_open()",
          ListBuffer[Statement](
            VariableDeclarationStatement(IntegerDatatype(), "size", Some("file.tellg()")),
            VariableDeclarationStatement(PointerDatatype("char"), "memblock", Some("new char[size]")),
            "file.seekg (0, std::ios::beg)",
            "file.read (memblock, size)",
            VariableDeclarationStatement(IntegerDatatype(), "numDomains", Some(ReadValueFrom(new IntegerDatatype, "memblock"))),
            AssignmentStatement("numFragments", ReadValueFrom(new IntegerDatatype, "memblock")),
            AssignmentStatement("bufsize", ReadValueFrom(new IntegerDatatype, "memblock")),
            (if (Knowledge.mpi_enabled) VariableDeclarationStatement(IntegerDatatype(), "offset", Some("bufsize"))
            else NullStatement),
            (if (Knowledge.mpi_enabled) {
              ForLoopStatement("int i = 1", " i < numDomains ", "++i", ListBuffer(
                VariableDeclarationStatement(IntegerDatatype(), "n", Some(ReadValueFrom(new IntegerDatatype, "memblock"))),
                VariableDeclarationStatement(IntegerDatatype(), "b", Some(ReadValueFrom(new IntegerDatatype, "memblock"))),
                MPI_Send("&n", "1", IntegerDatatype(), "i", 0, "mpiRequest_Send_0[i][0]"),
                MPI_Send("&offset", "1", IntegerDatatype(), "i", 1, "mpiRequest_Send_0[i][1]"),
                MPI_Send("&b", "1", IntegerDatatype(), "i", 2, "mpiRequest_Send_0[i][2]"),
                AssignmentStatement("offset", AdditionExpression("offset", "b"))))
            } else NullStatement),
            "file.close()"),
          ListBuffer[Statement]())))
  }
}

case class DomainFunctions() extends FunctionCollection(
  "Domains/DomainGenerated",
  ListBuffer(),
  ListBuffer("Globals/Globals.h", "Util/Vector.h", "CommFunctions/CommFunctions.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"

  if (Knowledge.domain_rect_generate)
    functions += new InitGeneratedDomain
  else {
    externalDependencies += ("iostream", "fstream")
    val rvTemplateFunc = FunctionStatement(
      new SpecialDatatype("template <class T> T"),
      s"readValue",
      ListBuffer[VariableAccess](
        VariableAccess("memblock", Some("char*&")),
        VariableAccess("title = \"\"", Some("std::string"))),
      ListBuffer[Statement](
        VariableDeclarationStatement(IntegerDatatype(), "size", Some("sizeof(T)")),
        VariableDeclarationStatement(CharDatatype(), "bytes[size]"),
        ForLoopStatement("int j = 0", " j < size ", "++j", ListBuffer("bytes[size-1-j] = memblock[j]")),
        "memblock+=size",
        ReturnStatement(Some("*(T *)&bytes"))))
    rvTemplateFunc.annotate("isTemplate")
    functions += rvTemplateFunc
    functions += new ReadConfig
    functions += new SetValues
    functions += new InitDomainFromFragmentFile
  }
}
