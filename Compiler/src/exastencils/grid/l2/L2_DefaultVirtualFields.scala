package exastencils.grid.l2

import scala.collection.mutable.ListBuffer

import exastencils.baseExt.l2._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.l2._
import exastencils.logger.Logger

/// L2_DefaultVirtualFields

object L2_DefaultVirtualFields {
  def assemble() = assembleForDomain(L2_DomainCollection.getByIdentifier("global").get)
  def assembleWithoutDomain() = assembleForDomain(L2_DummyDomain())

  def assembleForDomain(domain : L2_Domain) = {
    var collection = ListBuffer[L2_VirtualField]()
    def forEachDim(fct : Int => Unit) = (0 until domain.numDims).foreach(fct)

    Knowledge.levels.foreach(level => {
      // basic geometric positions

      collection += L2_VF_NodePositionAsVec(level, domain)
      forEachDim(dim => collection += L2_VF_NodePositionPerDim(level, domain, dim))

      collection += L2_VF_CellCenterAsVec(level, domain)
      forEachDim(dim => collection += L2_VF_CellCenterPerDim(level, domain, dim))

      collection += L2_VF_BoundaryPositionAsVec(level, domain)
      forEachDim(dim => collection += L2_VF_BoundaryPositionPerDim(level, domain, dim))

      // composed geometric data

      collection += L2_VF_CellWidthAsVec(level, domain)
      forEachDim(dim => collection += L2_VF_CellWidthPerDim(level, domain, dim))

      collection += L2_VF_CellVolume(level, domain)

      // geometric data of staggered grids

      if (Knowledge.grid_isStaggered) {
        forEachDim(stagDim => {
          collection += L2_VF_StagCellWidthAsVec(level, domain, stagDim)
          forEachDim(dim => collection += L2_VF_StagCellWidthPerDim(level, domain, stagDim, dim))

          collection += L2_VF_StagCellVolume(level, domain, stagDim)
        })
      }
    })

    collection
  }
}

/// L2_PrepareVirtualFieldDeclaration

object L2_PrepareVirtualFieldDeclarations extends DefaultStrategy("Prepare knowledge for L2 virtual fields") {
  // map of aliases for backwards compatibility and/or convenience: "baseName" <- ListBuffer(of, aliases)
  var fieldAliases = Map[String, ListBuffer[String]]()

  override def apply(applyAtNode : Option[Node] = None) = {
    val fields = L2_DefaultVirtualFields.assembleWithoutDomain()

    fieldAliases = fields.map(vf => (vf.name, vf.knownAliases)).toMap
    super.apply(applyAtNode)

    fields.foreach(vf => L2_VirtualFieldCollection.addDeclared(vf.name, vf.level))

    Logger.debug(s"Added ${ fields.length } virtual fields")
  }

  this += new Transformation("Resolve aliases", {
    case access : L2_UnresolvedAccess =>
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

/// L2_ProcessVirtualFieldDeclarations

object L2_ProcessVirtualFieldDeclarations extends DefaultStrategy("Integrate L2 virtual field declarations with knowledge") {
  override def apply(applyAtNode : Option[Node] = None) = {
    L2_DefaultVirtualFields.assemble().foreach(vf =>
      if (!L2_VirtualFieldCollection.exists(vf.name, vf.level))
        L2_VirtualFieldCollection.add(vf))

    super.apply(applyAtNode)
  }
}
