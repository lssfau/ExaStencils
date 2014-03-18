package exastencils.primitives

import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.Transformation._
import exastencils.primitives._
import exastencils.strategies._

object SetupFragmentClass extends Strategy("Setting up fragment class") {
  val communicationFunctions = StateManager.findFirst[CommunicationFunctions]().get;
  val fieldCollection = StateManager.findFirst[FieldCollection]().get;

  this += new Transformation("Initing FragmentClass", {
    case frag : FragmentClass =>
      frag.init;
      Some(frag);
  });

  // FIXME: move
  this += new Transformation("Adding fields to FragmentClass", {
    case collection : FieldCollection =>
      for (level <- 0 to Knowledge.maxLevel) {
        collection.fields += new Field("Solution", "solData", "double",
          (0 to 2).toArray.map(dim => new FieldLayoutPerDim(0, Knowledge.data_numGhostLayers, 1, ((Knowledge.domain_fragLengthPerDim(dim) * (1 << level)) + 1) - 2 /*dup*/ , 1, Knowledge.data_numGhostLayers, 0)),
          level, Knowledge.data_numSolSlots, true);
        collection.fields += new Field("Residual", "resData", "double",
          (0 to 2).toArray.map(dim => new FieldLayoutPerDim(0, Knowledge.data_numGhostLayers, 1, ((Knowledge.domain_fragLengthPerDim(dim) * (1 << level)) + 1) - 2 /*dup*/ , 1, Knowledge.data_numGhostLayers, 0)),
          level, 1, false);
        collection.fields += new Field("RHS", "rhsData", "double",
          (0 to 2).toArray.map(dim => new FieldLayoutPerDim(0, Knowledge.data_numGhostLayers, 1, ((Knowledge.domain_fragLengthPerDim(dim) * (1 << level)) + 1) - 2 /*dup*/ , 1, Knowledge.data_numGhostLayers, 0)),
          level, 1, false);
      }
      Some(collection);
  });

  this += new Transformation("Updating FragmentClass with required field declarations", {
    case frag : FragmentClass =>
      for (field <- fieldCollection.fields) {
        if (field.level == Knowledge.maxLevel /*QUICKFIX: only take fields on one level into consideration to avoid redefinition*/ )
          frag.declarations += s"Container* ${field.codeName}[${field.numSlots}][${Knowledge.numLevels}];";
      }
      Some(frag);
  });

  this += new Transformation("Adding basic functions to CommunicationFunctions", {
    case commFu : CommunicationFunctions =>
      commFu.functions += new WaitForMPIReq;
      Some(commFu);
  });

  this += new Transformation("Adding basic functions to FragmentClass", {
    case frag : FragmentClass =>
      frag.functions += new ConnectLocalElement();
      frag.functions += new ConnectRemoteElement();
      frag.functions += new SetupBuffers(fieldCollection.fields, frag.neighbors);
      Some(frag);
  });

  this += new Transformation("Adding communication functions to FragmentClass", {
    case frag : FragmentClass =>
      communicationFunctions.functions += new WaitForMPISendOps(frag.neighbors);
      communicationFunctions.functions += new WaitForMPIRecvOps(frag.neighbors);
      for (field <- fieldCollection.fields) {
        if (field.level == Knowledge.maxLevel /*QUICKFIX: only take fields on one level into consideration to avoid redefinition*/ )
          communicationFunctions.functions += new ExchangeDataSplitter(field);
        if (6 == Knowledge.comm_strategyFragment) // FIXME: generic call pattern
          communicationFunctions.functions += new ExchangeData_6(field, frag.neighbors);
        else if (26 == Knowledge.comm_strategyFragment)
          communicationFunctions.functions += new ExchangeData_26(field, frag.neighbors);
      }
      Some(frag);
  });
}