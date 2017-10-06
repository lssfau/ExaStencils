package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.baseExt.l3._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.l3._
import exastencils.logger.Logger

/// L3_DefaultVirtualFields

object L3_DefaultVirtualFields {
  def assemble() = assembleForDomain(L3_DomainCollection.getByIdentifier("global").get)
  def assembleWithoutDomain() = assembleForDomain(L3_DummyDomain())

  def assembleForDomain(domain : L3_Domain) = {
    var collection = ListBuffer[L3_VirtualField]()
    def forEachDim(fct : Int => Unit) = (0 until domain.numDims).foreach(fct)

    Knowledge.levels.foreach(level => {
      // basic geometric positions

      collection += L3_VF_NodePositionAsVec(level, domain)
      forEachDim(dim => collection += L3_VF_NodePositionPerDim(level, domain, dim))

      collection += L3_VF_CellCenterAsVec(level, domain)
      forEachDim(dim => collection += L3_VF_CellCenterPerDim(level, domain, dim))

      collection += L3_VF_BoundaryPositionAsVec(level, domain)
      forEachDim(dim => collection += L3_VF_BoundaryPositionPerDim(level, domain, dim))

      // composed geometric data

      collection += L3_VF_CellWidthAsVec(level, domain)
      forEachDim(dim => collection += L3_VF_CellWidthPerDim(level, domain, dim))

      collection += L3_VF_CellVolume(level, domain)

      // geometric data of staggered grids

      if (Knowledge.grid_isStaggered) {
        forEachDim(stagDim => {
          collection += L3_VF_StagCellWidthAsVec(level, domain, stagDim)
          forEachDim(dim => collection += L3_VF_StagCellWidthPerDim(level, domain, stagDim, dim))

          collection += L3_VF_StagCellVolume(level, domain, stagDim)
        })
      }
    })

    collection
  }
}

/// L3_PrepareVirtualFieldDeclaration

object L3_PrepareVirtualFieldDeclarations extends DefaultStrategy("Prepare knowledge for L3 virtual fields") {
  // map of aliases for backwards compatibility and/or convenience: "baseName" <- ListBuffer(of, aliases)
  var fieldAliases = Map[String, ListBuffer[String]]()

  override def apply(applyAtNode : Option[Node] = None) = {
    val fields = L3_DefaultVirtualFields.assembleWithoutDomain()

    fieldAliases = fields.map(vf => (vf.name, vf.knownAliases)).toMap
    super.apply(applyAtNode)

    fields.foreach(vf => L3_VirtualFieldCollection.addDeclared(vf.name, vf.level))

    Logger.debug(s"Added ${ fields.length } virtual fields")
  }

  this += new Transformation("Resolve aliases", {
    case access : L3_UnresolvedAccess =>
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

/// L3_ProcessVirtualFieldDeclarations

object L3_ProcessVirtualFieldDeclarations extends DefaultStrategy("Integrate L3 virtual field declarations with knowledge") {
  override def apply(applyAtNode : Option[Node] = None) = {
    L3_DefaultVirtualFields.assemble().foreach(vf =>
      if (!L3_VirtualFieldCollection.exists(vf.name, vf.level))
        L3_VirtualFieldCollection.add(vf))

    super.apply(applyAtNode)
  }
}
