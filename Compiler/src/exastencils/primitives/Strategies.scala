package exastencils.primitives

import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._
import exastencils.strategies._

object SetupFragmentClass extends Strategy("Setting up fragment class") {
  // FIXME: find fieldCollection and communicationFunctions on a different way (more robust, more readable)
  val communicationFunctions : CommunicationFunctions = StateManager.root_.asInstanceOf[Root].statements.find(s => s.isInstanceOf[CommunicationFunctions]).get.asInstanceOf[CommunicationFunctions];
  val fieldCollection : FieldCollection = StateManager.root_.asInstanceOf[Root].statements.find(s => s.isInstanceOf[FieldCollection]).get.asInstanceOf[FieldCollection];

  this += new Transformation("Initing FragmentClass", {
    case frag : FragmentClass =>
      frag.init;
      Some(frag);
  });

  this += new Transformation("Adding fields to FragmentClass", {
    case collection : FieldCollection =>
      collection.fields += new Field("Solution", "solData", "double", Knowledge.numSolSlots, true);
      collection.fields += new Field("Residual", "resData", "double", 1, false);
      collection.fields += new Field("RHS", "rhsData", "double", 1, false);
      Some(collection);
  });

  this += new Transformation("Updating FragmentClass with required field declarations", {
    case frag : FragmentClass =>
      for (field <- fieldCollection.fields) {
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
      for (field <- fieldCollection.fields) {
        communicationFunctions.functions += new ExchangeDataSplitter(field);
        for (level <- (0 to Knowledge.maxLevel)) {
          if (6 == Knowledge.fragmentCommStrategy) // FIXME: generic call pattern
            communicationFunctions.functions += new ExchangeData_6(field, level, frag.neighbors);
          else if (26 == Knowledge.fragmentCommStrategy)
            communicationFunctions.functions += new ExchangeData_26(field, level, frag.neighbors);
        }
      }
      Some(frag);
  });
}