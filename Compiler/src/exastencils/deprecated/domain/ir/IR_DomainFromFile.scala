package exastencils.deprecated.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.communication.ir.IR_IV_CommunicationId
import exastencils.config.Knowledge
import exastencils.deprecated.domain.FragmentCollection
import exastencils.deprecated.ir._
import exastencils.domain.ir._
import exastencils.globals.ir.IR_AllocateDataFunction
import exastencils.parallelization.api.mpi.MPI_Send
import exastencils.prettyprinting.PpStream

/// IR_InitDomainFromFragmentFile

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
case class IR_InitDomainFromFragmentFile() extends IR_FuturePlainFunction {
  override def prettyprint_decl() = prettyprint
  override var name = "initDomain"

  override def generateFct() = {
    IR_PlainFunction(name,
      IR_UnitDatatype,
      if (Knowledge.mpi_enabled) {
        ListBuffer(
          IR_VariableDeclaration(IR_IntegerDatatype, "numFragments", 0),
          IR_VariableDeclaration(IR_IntegerDatatype, "bufsize", 0),
          IR_VariableDeclaration(IR_IntegerDatatype, "fileOffset", 0),
          IR_Assert(IR_EqEq("mpiSize", Knowledge.mpi_numThreads),
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
                  IR_VariableDeclaration(IR_IntegerDatatype, "numRanks", Some(IR_ReadValueFrom(IR_IntegerDatatype, "memblock"))),
                  IR_Assignment("numFragments", IR_ReadValueFrom(IR_IntegerDatatype, "memblock")),
                  IR_Assignment("bufsize", IR_ReadValueFrom(IR_IntegerDatatype, "memblock")),
                  if (Knowledge.mpi_enabled) IR_VariableDeclaration(IR_IntegerDatatype, "fileOffset", "bufsize")
                  else IR_NullStatement,
                  if (Knowledge.mpi_enabled) {
                    IR_ForLoop("int i = 1", " i < numRanks ", "++i",
                      IR_VariableDeclaration(IR_IntegerDatatype, "n", Some(IR_ReadValueFrom(IR_IntegerDatatype, "memblock"))),
                      IR_VariableDeclaration(IR_IntegerDatatype, "b", Some(IR_ReadValueFrom(IR_IntegerDatatype, "memblock"))),
                      MPI_Send("&n", "1", IR_IntegerDatatype, "i", 0, "mpiRequest_Send_0[i][0]"),
                      MPI_Send("&fileOffset", "1", IR_IntegerDatatype, "i", 1, "mpiRequest_Send_0[i][1]"),
                      MPI_Send("&b", "1", IR_IntegerDatatype, "i", 2, "mpiRequest_Send_0[i][2]"),
                      IR_Assignment("fileOffset", IR_Addition("fileOffset", "b")))
                  } else IR_NullStatement,
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
          IR_FunctionCall(IR_AllocateDataFunction.fctName))
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
              IR_VariableDeclaration(IR_IntegerDatatype, "numRanks", Some(IR_ReadValueFrom(IR_IntegerDatatype, "memblock"))),
              IR_Assignment("numFragments", IR_ReadValueFrom(IR_IntegerDatatype, "memblock")),
              IR_Assignment("bufsize", IR_ReadValueFrom(IR_IntegerDatatype, "memblock")),
              "file.close()"), ListBuffer[IR_Statement]()),
          IR_VariableDeclaration(IR_SpecialDatatype("std::ifstream"), s"""fileFrags("./Domains/fragments.dat", std::ios::binary | std::ios::in)"""),
          IR_VariableDeclaration(IR_CharDatatype, "buf[bufsize]"),
          "fileFrags.read (buf, bufsize)",
          "fileFrags.close()",
          "setValues(buf,numFragments)",
          IR_FunctionCall(IR_AllocateDataFunction.fctName))
      })
  }
}

/// IR_ReadValueFrom

//TODO specific expression for reading from fragment data file
@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
case class IR_ReadValueFrom(var innerDatatype : IR_Datatype, data : IR_Expression) extends IR_Expression {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "readValue<" << innerDatatype << '>' << "(" << data << ")"
}

/// IR_SetValues

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
case class IR_SetValues() extends IR_FuturePlainFunction {
  override var name = "setValues"
  override def prettyprint_decl() = prettyprint

  override def generateFct() = {
    var body = new ListBuffer[IR_Statement]
    for (d <- IR_DomainCollection.objects.indices) {
      body += IR_Assignment(IR_IV_IsValidForDomain(d), IR_ReadValueFrom(IR_BooleanDatatype, "data"))
    }
    body += IR_Scope(
      IR_Assignment(IR_IV_FragmentId(), IR_ReadValueFrom(IR_IntegerDatatype, "data")),
      IR_Assignment(IR_IV_CommunicationId(), IR_ReadValueFrom(IR_IntegerDatatype, "data")),
      IR_ForLoop(IR_VariableDeclaration(IR_IntegerDatatype, "i", 0),
        IR_Lower(IR_VariableAccess("i", IR_IntegerDatatype), math.pow(2, Knowledge.dimensionality)),
        IR_PreIncrement(IR_VariableAccess("i", IR_IntegerDatatype)),
        IR_VariableDeclaration(IR_RealDatatype, "vertPos_x", IR_ReadValueFrom(IR_RealDatatype, "data")),
        if (Knowledge.dimensionality == 2) IR_VariableDeclaration(IR_RealDatatype, "vertPos_y", IR_ReadValueFrom(IR_RealDatatype, "data")) else IR_NullStatement,
        if (Knowledge.dimensionality == 3) IR_VariableDeclaration(IR_RealDatatype, "vertPos_z", IR_ReadValueFrom(IR_RealDatatype, "data")) else IR_NullStatement,
        IR_Switch("i",
          IR_Case("0", Knowledge.dimensions.map(dim => IR_Assignment(IR_IV_FragmentPositionBegin(dim), s"vertPos_${ IR_DimToString(dim) }") : IR_Statement).to[ListBuffer]),
          IR_Case("1", Knowledge.dimensions.map(dim => IR_Assignment(IR_IV_FragmentPositionEnd(dim), s"vertPos_${ IR_DimToString(dim) }") : IR_Statement).to[ListBuffer]),
          IR_Case("3", Knowledge.dimensions.map(dim => IR_Assignment(IR_IV_FragmentPositionEnd(dim), s"vertPos_${ IR_DimToString(dim) }") : IR_Statement).to[ListBuffer]),
          IR_Case("7", Knowledge.dimensions.map(dim => IR_Assignment(IR_IV_FragmentPositionEnd(dim), s"vertPos_${ IR_DimToString(dim) }") : IR_Statement).to[ListBuffer]))),
      IR_Scope(Knowledge.dimensions.map(dim => IR_Assignment(IR_IV_FragmentPosition(dim), IR_ReadValueFrom(IR_RealDatatype, "data")) : IR_Statement).to[ListBuffer])
      //                  VariableDeclarationStatement(IR_IntegerDatatype,"numNeigbours",Some(FunctionCallExpression("readValue<int>",ListBuffer("data")))),
    )
    for (d <- IR_DomainCollection.objects.indices) {
      body += IR_ForLoop("int location = 0", s" location < ${ FragmentCollection.getNumberOfNeighbors() } ", "++location",
        IR_IfCondition(IR_ReadValueFrom(IR_BooleanDatatype, "data"),
          ListBuffer[IR_Statement](//neighbor is valid
            IR_IfCondition(IR_ReadValueFrom(IR_BooleanDatatype, "data"),
              ListBuffer[IR_Statement](//neighbor is remote
                IR_VariableDeclaration(IR_IntegerDatatype, "neighIdx", Some(IR_ReadValueFrom(IR_IntegerDatatype, "data"))),
                IR_VariableDeclaration(IR_IntegerDatatype, "neighRank", Some(IR_ReadValueFrom(IR_IntegerDatatype, "data"))),
                if (Knowledge.mpi_enabled) s"connectRemoteElement (${ IR_IV_CommunicationId().prettyprint() }, neighIdx, neighRank, location, $d)" else IR_NullStatement),
              ListBuffer[IR_Statement](//neighbor is local
                IR_VariableDeclaration(IR_IntegerDatatype, "neighIdx", Some(IR_ReadValueFrom(IR_IntegerDatatype, "data"))),
                if (FragmentCollection.fragments.length > 1) s"connectLocalElement(${ IR_IV_CommunicationId().prettyprint() },neighIdx,location,$d)" else IR_NullStatement)))))
    }
    body += IR_IfCondition(IR_ReadValueFrom(IR_BooleanDatatype, "data"),
      ListBuffer[IR_Statement](
        "Mat4 trafoTmp = Mat4()",
        IR_ForLoop("int i = 0", " i < 12 ", "++i",
          IR_Assignment("trafoTmp[i]", IR_ReadValueFrom(IR_RealDatatype, "data"))),
        IR_Assignment(IR_IV_PrimitiveTransformation(), "trafoTmp")))

    IR_PlainFunction(name,
      IR_UnitDatatype,
      ListBuffer[IR_FunctionArgument](IR_FunctionArgument("data", IR_SpecialDatatype("char*")), IR_FunctionArgument("numFragments", IR_IntegerDatatype)),
      //      ListBuffer((LoopOverFragments(body))))
      IR_ForLoop(" int fragmentIdx = 0 ", " fragmentIdx < numFragments ", " ++fragmentIdx ", body))
  }
}
