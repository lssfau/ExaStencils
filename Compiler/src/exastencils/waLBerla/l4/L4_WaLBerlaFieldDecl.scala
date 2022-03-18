package exastencils.waLBerla.l4

import exastencils.base.l4.L4_MayBlockResolution
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.fieldlike.l4.L4_FieldDeclLike
import exastencils.logger.Logger

abstract class L4_WaLBerlaFieldDecl extends L4_FieldDeclLike {
  override def progress = Logger.error(s"Trying to progress l4 field declaration for field $name; this is not supported")
}

/// L4_WaLBerlaPrepareFieldDeclarations

object L4_WaLBerlaPrepareFieldDeclarations extends DefaultStrategy("Prepare knowledge for waLBerla L4 fields") {
  this += Transformation("Process new fields", {
    case decl : L4_WaLBerlaFieldDecl =>
      L4_WaLBerlaFieldCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L4_WaLBerlaProcessFieldDeclarations

object L4_WaLBerlaProcessFieldDeclarations extends DefaultStrategy("Integrate L4 waLBerla field declarations with knowledge") {
  this += Transformation("Process field declarations", {
    case decl : L4_WaLBerlaFieldDecl if L4_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}