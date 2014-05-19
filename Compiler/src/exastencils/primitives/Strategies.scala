package exastencils.primitives

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.Transformation._
import exastencils.primitives._
import exastencils.strategies._

object SetupFragmentClass extends DefaultStrategy("Setting up fragment class") {
  val communicationFunctions = StateManager.findFirst[CommunicationFunctions]().get
  val fieldCollection = StateManager.findFirst[FieldCollection]().get

  this += new Transformation("Initing FragmentClass", {
    case frag : FragmentClass =>
      frag.init
      frag
  })

  this += new Transformation("Updating FragmentClass with required field declarations", {
    case frag : FragmentClass =>
      for (field <- fieldCollection.fields) {
        frag.declarations += field.dataType. /*FIXME*/ cpp ~ "*" ~ field.codeName ~ s"[${field.numSlots}]"
        frag.dTorBody ++= (0 until field.numSlots).map(slot => ("delete[] " ~ field.codeName ~ s"[$slot]") : Statement).to[ListBuffer]
      }
      frag
  })

  if (Knowledge.useMPI) {
    this += new Transformation("Adding basic functions to CommunicationFunctions", {
      case commFu : CommunicationFunctions =>
        commFu.functions += new WaitForMPIReq
        Some(commFu)
    })
  }

  this += new Transformation("Adding basic functions to FragmentClass", {
    case frag : FragmentClass =>
      if (Knowledge.domain_canHaveLocalNeighs)
        frag.functions += new ConnectLocalElement()
      if (Knowledge.domain_canHaveRemoteNeighs)
        frag.functions += new ConnectRemoteElement()
      frag.functions += new SetupBuffers(fieldCollection.fields, frag.neighbors)
      frag
  })

  this += new Transformation("Adding communication functions to FragmentClass", {
    case frag : FragmentClass =>
      if (Knowledge.useMPI) {
        communicationFunctions.functions += new WaitForMPISendOps(frag.neighbors)
        communicationFunctions.functions += new WaitForMPIRecvOps(frag.neighbors)
      }
      for (field <- fieldCollection.fields) {
        if (6 == Knowledge.comm_strategyFragment) // FIXME: generic call pattern
          communicationFunctions.functions += new ExchangeData_6(field, frag.neighbors)
        else if (26 == Knowledge.comm_strategyFragment)
          communicationFunctions.functions += new ExchangeData_26(field, frag.neighbors)
      }
      frag
  })
}

object ResolveLoopOverDimensions extends DefaultStrategy("Resolving LoopOverDimensions nodes") {
  this += new Transformation("Resolving", {
    case loop : LoopOverDimensions =>
      loop.expandSpecial
  })
}

object LinearizeFieldAccesses extends DefaultStrategy("Linearizing FieldAccess nodes") {
  this += new Transformation("Linearizing", {
    case loop : DirectFieldAccess =>
      loop.linearize
    case loop : FieldAccess =>
      loop.linearize
  })
}