package exastencils.primitives

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.util._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.Transformation._
import exastencils.primitives._
import exastencils.strategies._

object SetupFragmentClass extends DefaultStrategy("Setting up fragment class") {
  var communicationFunctions : Option[CommunicationFunctions] = None
  var fieldCollection : Option[FieldCollection] = None
  var externalFieldCollection : Option[ExternalFieldCollection] = None

  override def apply(node : Option[Node] = None) = {
    communicationFunctions = StateManager.findFirst[CommunicationFunctions]()
    fieldCollection = StateManager.findFirst[FieldCollection]()
    externalFieldCollection = StateManager.findFirst[ExternalFieldCollection]()
    super.apply(node)
  }

  this += new Transformation("Setting up FragmentClass", {
    case frag : FragmentClass =>
      frag.setupNeighbors
      frag.setupDefaultMembers
      frag.setupBasicNeighborhoodMembers
      if (Knowledge.domain_canHaveRemoteNeighs)
        frag.setupRemoteNeighborhoodMembers
      frag
  })

  this += new Transformation("Updating FragmentClass with required field declarations", {
    case frag : FragmentClass =>
      for (field <- fieldCollection.get.fields) {
        frag.declarations += field.dataType. /*FIXME*/ cpp ~ "*" ~ field.codeName ~ s"[${field.numSlots}]"
        frag.dTorBody ++= (0 until field.numSlots).map(slot => ("delete[] " ~ field.codeName ~ s"[$slot]") : Statement).to[ListBuffer]
      }
      frag
  })

  if (Knowledge.useMPI) {
    this += new Transformation("Adding basic functions to CommunicationFunctions", {
      case commFu : CommunicationFunctions =>
        commFu.functions += new WaitForMPIRequestFunc
        Some(commFu)
    })
  }

  this += new Transformation("Adding basic functions to FragmentClass", {
    case frag : FragmentClass =>
      if (Knowledge.domain_canHaveLocalNeighs)
        frag.functions += new ConnectLocalElement()
      if (Knowledge.domain_canHaveRemoteNeighs)
        frag.functions += new ConnectRemoteElement()
      frag.functions += new SetupBuffers(fieldCollection.get.fields, frag.neighbors)
      frag
  })

  this += new Transformation("Adding communication functions to FragmentClass", {
    case frag : FragmentClass =>
      //      if (Knowledge.useMPI) {
      //        communicationFunctions.get.functions += new WaitForMPISendOps(frag.neighbors)
      //        communicationFunctions.get.functions += new WaitForMPIRecvOps(frag.neighbors)
      //      }
      for (field <- fieldCollection.get.fields) {
        Knowledge.comm_strategyFragment match {
          case 6  => communicationFunctions.get.functions += new ExchangeData_6(field, frag.neighbors)
          case 26 => communicationFunctions.get.functions += new ExchangeData_26(field, frag.neighbors)
        }
      }
      frag
  })

  this += new Transformation("Adding external field transfer functions", {
    case frag : FragmentClass =>
      for (extField <- externalFieldCollection.get.fields) {
        frag.functions += new GetFromExternalField(fieldCollection.get.getFieldByIdentifier(extField.targetFieldIdentifier, extField.level).get, extField)
        frag.functions += new SetFromExternalField(fieldCollection.get.getFieldByIdentifier(extField.targetFieldIdentifier, extField.level).get, extField)
      }
      frag
  })
}

object AddFragmentMember extends DefaultStrategy("Adding members for fragment communication") {
  var declarationMap : Map[String, Statement] = Map()
  var ctorMap : Map[String, Statement] = Map()
  var dtorMap : Map[String, Statement] = Map()

  this += new Transformation("Collecting", {
    case mem : FragCommMember => // TODO: don't overwrite for performance reasons
      declarationMap += (mem.resolveName -> mem.getDeclaration)
      if (mem.getCtor().isDefined)
        ctorMap += (mem.resolveName -> mem.getCtor().get)
      if (mem.getDtor().isDefined)
        dtorMap += (mem.resolveName -> mem.getDtor().get)
      mem
  })

  this += new Transformation("Adding", {
    case frag : FragmentClass =>
      for (decl <- declarationMap)
        frag.declarations += decl._2
      if (!ctorMap.isEmpty)
        frag.cTorBody ++= ctorMap.map(_._2)
      if (!dtorMap.isEmpty)
        frag.dTorBody ++= dtorMap.map(_._2)
      frag
  })

  var bufferSizes : Map[Expression, Int] = Map()

  this += new Transformation("Collecting buffer sizes", {
    case buf : FragMember_TmpBuffer =>
      val size = SimplifyExpression.evalIntegral(buf.size).toInt
      val id = buf.resolveAccess(buf.resolveName, new NullExpression, buf.field.identifier /*FIXME: id*/ , buf.field.level, buf.neighIdx)
      bufferSizes += (id -> (size max bufferSizes.getOrElse(id, 0)))
      buf
  })

  this += new Transformation("Extending SetupBuffers function", {
    // FIXME: this kind of matching is awkward, I want trafos that don't return nodes
    case func : FunctionStatement if (("setupBuffers" : Expression) == func.name) =>
      for (buf <- bufferSizes) {
        func.body += new AssignmentStatement(buf._1, s"new double[${buf._2}]")
      }
      func
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
    case loop : ExternalFieldAccess =>
      loop.linearize
  })
}

object ResolveIndexOffsets extends DefaultStrategy("Resolving OffsetIndex nodes") {
  this += new Transformation("Resolving", {
    case index : OffsetIndex =>
      index.expandSpecial
  })
}
