package exastencils.domain

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.deprecated.ir.IR_DimToString
import exastencils.domain.ir._
import exastencils.grid._
import exastencils.mpi.ir.MPI_Send
import exastencils.prettyprinting._

//TODO specific expression for reading from fragment data file
case class ReadValueFrom(var innerDatatype : IR_Datatype, data : IR_Expression) extends IR_Expression {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "readValue<" << innerDatatype << '>' << "(" << data << ")"
}

case class InitDomainFromFragmentFile() extends IR_AbstractFunction with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl = prettyprint
  override def name = "initDomain"

  override def expand : Output[IR_Function] = {
    IR_Function(IR_UnitDatatype, name,
      if (Knowledge.mpi_enabled) {
        ListBuffer(
          IR_VariableDeclaration(IR_IntegerDatatype, "numFragments", 0),
          IR_VariableDeclaration(IR_IntegerDatatype, "bufsize", 0),
          IR_VariableDeclaration(IR_IntegerDatatype, "fileOffset", 0),
          IR_Assert(IR_EqEqExpression("mpiSize", Knowledge.mpi_numThreads),
            ListBuffer("\"Invalid number of MPI processes (\"", "mpiSize", "\") should be \"", Knowledge.domain_numBlocks),
            "return"),
          IR_IfCondition("mpiRank == 0",
            ListBuffer[IR_Statement](
              IR_VariableDeclaration(IR_SpecialDatatype("std::ifstream"), "file(\"./Domains/config.dat\", std::ios::binary | std::ios::ate | std::ios::in)"),
              IR_IfCondition(
                "file.is_open()",
                ListBuffer[IR_Statement](
                  IR_VariableDeclaration(IR_IntegerDatatype, "size", "file.tellg()"),
                  IR_VariableDeclaration(IR_PointerDatatype("char"), "memblock", "new char[size]"),
                  "file.seekg (0, std::ios::beg)",
                  "file.read (memblock, size)",
                  IR_VariableDeclaration(IR_IntegerDatatype, "numRanks", Some(ReadValueFrom(IR_IntegerDatatype, "memblock"))),
                  IR_Assignment("numFragments", ReadValueFrom(IR_IntegerDatatype, "memblock")),
                  IR_Assignment("bufsize", ReadValueFrom(IR_IntegerDatatype, "memblock")),
                  (if (Knowledge.mpi_enabled) IR_VariableDeclaration(IR_IntegerDatatype, "fileOffset", "bufsize")
                  else IR_NullStatement),
                  (if (Knowledge.mpi_enabled) {
                    IR_ForLoop("int i = 1", " i < numRanks ", "++i",
                      IR_VariableDeclaration(IR_IntegerDatatype, "n", Some(ReadValueFrom(IR_IntegerDatatype, "memblock"))),
                      IR_VariableDeclaration(IR_IntegerDatatype, "b", Some(ReadValueFrom(IR_IntegerDatatype, "memblock"))),
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
          IR_VariableDeclaration(IR_SpecialDatatype("MPI_File"), "fh"),
          "MPI_File_open(mpiCommunicator, \"./Domains/fragments.dat\", MPI_MODE_RDONLY, MPI_INFO_NULL,&fh)",
          IR_VariableDeclaration(IR_CharDatatype, "buf[bufsize]"),
          "MPI_File_read_at(fh, fileOffset, buf, bufsize,MPI_BYTE, MPI_STATUSES_IGNORE)",
          "MPI_Barrier(MPI_COMM_WORLD)",
          "MPI_File_close(&fh)",
          "setValues(buf,numFragments)",
          IR_FunctionCall("setupBuffers"))
      } else {
        ListBuffer(
          IR_VariableDeclaration(IR_IntegerDatatype, "numFragments", 0),
          IR_VariableDeclaration(IR_IntegerDatatype, "bufsize", 0),
          IR_VariableDeclaration(IR_IntegerDatatype, "fileOffset", 0),
          IR_VariableDeclaration(IR_SpecialDatatype("std::ifstream"), "file(\"./Domains/config.dat\", std::ios::binary | std::ios::ate | std::ios::in)"),
          IR_IfCondition(
            "file.is_open()",
            ListBuffer[IR_Statement](
              IR_VariableDeclaration(IR_IntegerDatatype, "size", "file.tellg()"),
              IR_VariableDeclaration(IR_PointerDatatype("char"), "memblock", "new char[size]"),
              "file.seekg (0, std::ios::beg)",
              "file.read (memblock, size)",
              IR_VariableDeclaration(IR_IntegerDatatype, "numRanks", Some(ReadValueFrom(IR_IntegerDatatype, "memblock"))),
              IR_Assignment("numFragments", ReadValueFrom(IR_IntegerDatatype, "memblock")),
              IR_Assignment("bufsize", ReadValueFrom(IR_IntegerDatatype, "memblock")),
              "file.close()"), ListBuffer[IR_Statement]()),
          IR_VariableDeclaration(IR_SpecialDatatype("std::ifstream"), s"""fileFrags("./Domains/fragments.dat", std::ios::binary | std::ios::in)"""),
          IR_VariableDeclaration(IR_CharDatatype, "buf[bufsize]"),
          "fileFrags.read (buf, bufsize)",
          "fileFrags.close()",
          "setValues(buf,numFragments)",
          IR_FunctionCall("setupBuffers"))
      })

  }
}

case class SetValues() extends IR_AbstractFunction with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl = prettyprint
  override def name = "setValues"

  override def expand : Output[IR_Function] = {
    var body = new ListBuffer[IR_Statement]
    for (d <- 0 until IR_DomainCollection.objects.size) {
      body += IR_Assignment(IR_IV_IsValidForDomain(d), ReadValueFrom(IR_BooleanDatatype, "data"))
    }
    body += IR_Scope(
      IR_Assignment(IR_IV_FragmentId(), ReadValueFrom(IR_IntegerDatatype, "data")),
      IR_Assignment(iv.CommId(), ReadValueFrom(IR_IntegerDatatype, "data")),
      IR_ForLoop(IR_VariableDeclaration(IR_IntegerDatatype, "i", 0),
        IR_LowerExpression(IR_VariableAccess("i", IR_IntegerDatatype), math.pow(2, Knowledge.dimensionality)),
        IR_PreIncrementExpression(IR_VariableAccess("i", IR_IntegerDatatype)),
        IR_VariableDeclaration(IR_RealDatatype, "vertPos_x", ReadValueFrom(IR_RealDatatype, "data")),
        if (Knowledge.dimensionality == 2) IR_VariableDeclaration(IR_RealDatatype, "vertPos_y", ReadValueFrom(IR_RealDatatype, "data")) else IR_NullStatement,
        if (Knowledge.dimensionality == 3) IR_VariableDeclaration(IR_RealDatatype, "vertPos_z", ReadValueFrom(IR_RealDatatype, "data")) else IR_NullStatement,
        IR_Switch("i",
          IR_Case("0", Knowledge.dimensions.map(dim => IR_Assignment(IR_IV_FragmentPositionBegin(dim), s"vertPos_${ IR_DimToString(dim) }") : IR_Statement).to[ListBuffer]),
          IR_Case("1", Knowledge.dimensions.map(dim => IR_Assignment(IR_IV_FragmentPositionEnd(dim), s"vertPos_${ IR_DimToString(dim) }") : IR_Statement).to[ListBuffer]),
          IR_Case("3", Knowledge.dimensions.map(dim => IR_Assignment(IR_IV_FragmentPositionEnd(dim), s"vertPos_${ IR_DimToString(dim) }") : IR_Statement).to[ListBuffer]),
          IR_Case("7", Knowledge.dimensions.map(dim => IR_Assignment(IR_IV_FragmentPositionEnd(dim), s"vertPos_${ IR_DimToString(dim) }") : IR_Statement).to[ListBuffer]))),
      IR_Scope(Knowledge.dimensions.map(dim => IR_Assignment(IR_IV_FragmentPosition(dim), ReadValueFrom(IR_RealDatatype, "data")) : IR_Statement).to[ListBuffer])
      //                  VariableDeclarationStatement(IR_IntegerDatatype,"numNeigbours",Some(FunctionCallExpression("readValue<int>",ListBuffer("data")))),
    )
    for (d <- 0 until IR_DomainCollection.objects.size) {
      body += IR_ForLoop("int location = 0", s" location < ${ FragmentCollection.getNumberOfNeighbors() } ", "++location",
        IR_IfCondition(ReadValueFrom(IR_BooleanDatatype, "data"),
          ListBuffer[IR_Statement](//neighbor is valid
            IR_IfCondition(ReadValueFrom(IR_BooleanDatatype, "data"),
              ListBuffer[IR_Statement](//neighbor is remote
                IR_VariableDeclaration(IR_IntegerDatatype, "neighIdx", Some(ReadValueFrom(IR_IntegerDatatype, "data"))),
                IR_VariableDeclaration(IR_IntegerDatatype, "neighRank", Some(ReadValueFrom(IR_IntegerDatatype, "data"))),
                (if (Knowledge.mpi_enabled) s"connectRemoteElement (${ iv.CommId().prettyprint() }, neighIdx, neighRank, location, $d)" else IR_NullStatement)),
              ListBuffer[IR_Statement](//neighbor is local
                IR_VariableDeclaration(IR_IntegerDatatype, "neighIdx", Some(ReadValueFrom(IR_IntegerDatatype, "data"))),
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
  ListBuffer("Globals/Globals.h", "CommFunctions/CommFunctions.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"

  if (Knowledge.domain_rect_generate) {
    functions += IR_InitGeneratedDomain()
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
        IR_VariableDeclaration(IR_IntegerDatatype, "size", "sizeof(T)"),
        IR_VariableDeclaration(IR_CharDatatype, "bytes[size]"),
        IR_ForLoop("int j = 0", " j < size ", "++j", "bytes[size-1-j] = memblock[j]"),
        "memblock+=size",
        IR_Return("*(T *)&bytes")))
    rvTemplateFunc.isHeaderOnly = true // annotate("isTemplate")
    functions += rvTemplateFunc
    functions += new SetValues
    functions += new InitDomainFromFragmentFile
  }
}
