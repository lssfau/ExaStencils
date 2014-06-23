package exastencils.primitives

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.util._
import exastencils.globals._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.Transformation._
import exastencils.primitives._
import exastencils.strategies._
import exastencils.omp._

object SetupFragmentClass extends DefaultStrategy("Setting up fragment class") {
  var communicationFunctions : Option[CommunicationFunctions] = None

  override def apply(node : Option[Node] = None) = {
    communicationFunctions = StateManager.findFirst[CommunicationFunctions]()
    super.apply(node)
  }

  this += new Transformation("Setting up FragmentClass", {
    case frag : FragmentClass =>
      frag.setupNeighbors
      frag.setupDefaultMembers
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
      StateManager.findFirst[Globals]().get.functions += new SetupBuffers(FieldCollection.fields, frag.neighbors)
      frag
  })

  this += new Transformation("Adding communication functions to FragmentClass", {
    case frag : FragmentClass =>
      //      if (Knowledge.useMPI) {
      //        communicationFunctions.get.functions += new WaitForMPISendOps(frag.neighbors)
      //        communicationFunctions.get.functions += new WaitForMPIRecvOps(frag.neighbors)
      //      }
      for (field <- FieldCollection.fields) {
        Knowledge.comm_strategyFragment match {
          case 6  => communicationFunctions.get.functions += new ExchangeData_6(FieldSelection(field, "slot", -1), frag.neighbors)
          case 26 => communicationFunctions.get.functions += new ExchangeData_26(FieldSelection(field, "slot", -1), frag.neighbors)
        }
      }
      frag
  })

  this += new Transformation("Adding external field transfer functions", {
    case frag : FragmentClass =>
      for (extField <- ExternalFieldCollection.fields) {
        frag.functions += new GetFromExternalField(extField.targetField, extField)
        frag.functions += new SetFromExternalField(extField.targetField, extField)
      }
      frag
  })
}

object AddFragmentMember extends DefaultStrategy("Adding members for fragment communication") {
  var declarationMap_fragment : Map[String, VariableDeclarationStatement] = Map()
  var ctorMap_fragment : Map[String, Statement] = Map()
  var dtorMap_fragment : Map[String, Statement] = Map()
  var declarationMap : Map[String, VariableDeclarationStatement] = Map()
  var ctorMap : Map[String, Statement] = Map()
  var dtorMap : Map[String, Statement] = Map()

  this += new Transformation("Collecting", {
    case mem : iv.InternalVariable => // TODO: don't overwrite for performance reasons
      if (!mem.canBePerFragment) {
        declarationMap_fragment += (mem.resolveName -> mem.getDeclaration)
        if (mem.getCtor().isDefined)
          ctorMap_fragment += (mem.resolveName -> mem.getCtor().get)
        if (mem.getDtor().isDefined)
          dtorMap_fragment += (mem.resolveName -> mem.getDtor().get)
      } else {
        declarationMap += (mem.resolveName -> mem.getDeclaration)
        if (mem.getCtor().isDefined)
          ctorMap += (mem.resolveName -> mem.getCtor().get)
        if (mem.getDtor().isDefined)
          dtorMap += (mem.resolveName -> mem.getDtor().get)
      }
      mem
  })

  this += new Transformation("Adding to fragment class", {
    case frag : FragmentClass =>
      for (decl <- declarationMap_fragment)
        frag.declarations += decl._2
      if (!ctorMap_fragment.isEmpty)
        frag.cTorBody ++= ctorMap_fragment.map(_._2)
      if (!dtorMap_fragment.isEmpty)
        frag.dTorBody ++= dtorMap_fragment.map(_._2)
      frag
  })

  this += new Transformation("Adding to globals", {
    case globals : Globals =>
      for (decl <- declarationMap)
        globals.variables += decl._2
      globals
    case func : FunctionStatement if (("initGlobals" : Expression) == func.name) =>
      func.body ++= ctorMap.map(_._2)
      func
    // FIXME: globals d'tor
    //if (!dtorMap.isEmpty)
    //  globals.dTorBody ++= dtorMap.map(_._2)
  })

  var bufferSizes : Map[Expression, Int] = Map()

  this += new Transformation("Collecting buffer sizes", {
    case buf : iv.TmpBuffer =>
      val size = SimplifyExpression.evalIntegral(buf.size).toInt
      val id = buf.resolveAccess(buf.resolveName, "fragmentIdx", new NullExpression, buf.field.identifier /*FIXME: id*/ , buf.field.level, buf.neighIdx)
      bufferSizes += (id -> (size max bufferSizes.getOrElse(id, 0)))
      buf
  })

  this += new Transformation("Extending SetupBuffers function", {
    // FIXME: this kind of matching is awkward, I want trafos that don't return nodes
    case func : FunctionStatement if (("setupBuffers" : Expression) == func.name) =>
      func.body += new ForLoopStatement(s"int fragmentIdx = 0", s"fragmentIdx < " ~ Knowledge.domain_numFragsPerBlock, s"++fragmentIdx",
        bufferSizes.map(buf => new AssignmentStatement(buf._1, s"new double[${buf._2}]") : Statement).to[ListBuffer]) with OMP_PotentiallyParallel
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
