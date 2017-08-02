package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.baseExt.l4._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.l4._
import exastencils.logger.Logger

/// L4_DefaultVirtualFields

object L4_DefaultVirtualFields {
  def assemble() = assembleForDomain(L4_DomainCollection.getByIdentifier("global").get)
  def assembleWithoutDomain() = assembleForDomain(L4_DummyDomain())

  def assembleForDomain(domain : L4_Domain) = {
    var collection = ListBuffer[L4_VirtualField]()
    def forEachDim(fct : Int => Unit) = (0 until domain.numDims).foreach(fct)

    Knowledge.levels.foreach(level => {
      // basic geometric positions

      collection += L4_VF_NodePositionAsVec(level, domain)
      forEachDim(dim => collection += L4_VF_NodePositionPerDim(level, domain, dim))

      collection += L4_VF_CellCenterAsVec(level, domain)
      forEachDim(dim => collection += L4_VF_CellCenterPerDim(level, domain, dim))

      collection += L4_VF_BoundaryPositionAsVec(level, domain)
      forEachDim(dim => collection += L4_VF_BoundaryPositionPerDim(level, domain, dim))

      // composed geometric data

      collection += L4_VF_CellWidthAsVec(level, domain)
      forEachDim(dim => collection += L4_VF_CellWidthPerDim(level, domain, dim))

      collection += L4_VF_CellVolume(level, domain)

      // geometric data of staggered grids

      if (Knowledge.grid_isStaggered) {
        forEachDim(stagDim => {
          collection += L4_VF_StagCellWidthAsVec(level, domain, stagDim)
          forEachDim(dim => collection += L4_VF_StagCellWidthPerDim(level, domain, stagDim, dim))

          collection += L4_VF_StagCellVolume(level, domain, stagDim)
        })
      }
    })

    collection
  }
}

/// L4_PrepareVirtualFieldDeclaration

object L4_PrepareVirtualFieldDeclarations extends DefaultStrategy("Prepare knowledge for L4 virtual fields") {
  // map of aliases for backwards compatibility and/or convenience: "baseName" <- ListBuffer(of, aliases)
  var fieldAliases = Map[String, ListBuffer[String]]()

  override def apply(applyAtNode : Option[Node] = None) = {
    val fields = L4_DefaultVirtualFields.assembleWithoutDomain()

    fieldAliases = fields.map(vf => (vf.name, vf.knownAliases)).toMap
    super.apply(applyAtNode)

    fields.foreach(vf => L4_VirtualFieldCollection.addDeclared(vf.name, vf.level))

    Logger.debug(s"Added ${ fields.length } virtual fields")
  }

  this += new Transformation("Resolve aliases", {
    case access : L4_UnresolvedAccess =>
      var searchString = access.name
      if (!searchString.startsWith("vf_")) searchString = "vf_" + searchString

      val tryFind = fieldAliases.find(_._2.contains(searchString))
      if (tryFind.isDefined) {
        Logger.debug(s"Replacing $searchString with ${ tryFind.get._1 }")
        access.name = tryFind.get._1
      }

      access
  })
}

/// L4_ProcessVirtualFieldDeclarations

object L4_ProcessVirtualFieldDeclarations extends DefaultStrategy("Integrate L4 virtual field declarations with knowledge") {
  override def apply(applyAtNode : Option[Node] = None) = {
    L4_DefaultVirtualFields.assemble().foreach(L4_VirtualFieldCollection.add)

    super.apply(applyAtNode)
  }
}
