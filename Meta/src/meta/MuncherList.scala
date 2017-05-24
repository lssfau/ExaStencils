package meta

import scala.collection.mutable.ListBuffer

/// MuncherList

object MuncherList {

  object Entry {def apply(filePackage : String, className : String, layer : Layer) = new Entry(filePackage, className, ListBuffer(layer)) }

  case class Entry(var filePackage : String, var className : String, var layers : ListBuffer[Layer])

  val entries = ListBuffer[Entry]()

  def all = ListBuffer(L2, L3, L4, IR)
  def L2_L3 = ListBuffer(L2, L3)
  def L2_L3_L4 = ListBuffer(L2, L3, L4)

  var curPackage = ""

  if (true) { // in exastencils.base
    curPackage = "exastencils.base"

    entries += Entry(curPackage, "Access", L2_L3)
    entries += Entry(curPackage, "BinaryOp", L2_L3_L4)
    entries += Entry(curPackage, "Constant", L2_L3_L4)
    //entries += Entry(curPackage, "Datatype", all)
    entries += Entry(curPackage, "Expression", all)
    //entries += Entry(curPackage, "Function", all)
    entries += Entry(curPackage, "FunctionAccess", IR)
    entries += Entry(curPackage, "ImplicitConversion", all)
    //entries += Entry(curPackage, "Index", all)
    entries += Entry(curPackage, "LevelAlias", L2_L3_L4)
    entries += Entry(curPackage, "LevelCollector", L2_L3_L4)
    entries += Entry(curPackage, "LevelGroup", L2_L3_L4)
    entries += Entry(curPackage, "LevelSingle", L2_L3_L4)
    entries += Entry(curPackage, "LevelSpecification", L2_L3_L4)
    entries += Entry(curPackage, "Node", all)
    entries += Entry(curPackage, "Progressable", L2_L3_L4)
    entries += Entry(curPackage, "Root", L2_L3)
    entries += Entry(curPackage, "Statement", all)
    entries += Entry(curPackage, "UnaryOp", L2_L3_L4)
  }

  if (true) { // in exastencils.baseExt
    curPackage = "exastencils.baseExt"

    entries += Entry(curPackage, "FieldIteratorAccess", all)
  }

  if (true) { // in exastencils.domain
    curPackage = "exastencils.domain"

    entries += Entry(curPackage, "Domain", L2_L3)
    entries += Entry(curPackage, "DomainCollection", L2_L3)
    entries += Entry(curPackage, "DomainDecl", L2_L3)
  }

  if (true) { // in exastencils.field
    curPackage = "exastencils.field"

    entries += Entry(curPackage, "BaseFieldDecl", L2_L3)
    entries += Entry(curPackage, "BoundaryFieldDecl", L2_L3)
    entries += Entry(curPackage, "Field", L2_L3)
    entries += Entry(curPackage, "FieldAccess", L2_L3)
    entries += Entry(curPackage, "FieldCollection", L2_L3)
    entries += Entry(curPackage, "FieldDecl", L2_L3)
    entries += Entry(curPackage, "FutureFieldAccess", L2_L3)

    entries += Entry(curPackage, "FieldFromOther", L3)
    entries += Entry(curPackage, "FieldOverride", L3)
  }

  if (true) { // in exastencils.grid
    curPackage = "exastencils.grid"

    entries += Entry(curPackage, "DefaultVirtualFields", L2_L3)
    entries += Entry(curPackage, "FutureVirtualFieldAccess", L2_L3)
    entries += Entry(curPackage, "VirtualField", L2_L3)
    entries += Entry(curPackage, "VirtualFieldAccess", L2_L3)
    entries += Entry(curPackage, "VirtualFieldCollection", L2_L3)
  }

  if (true) { // in exastencils.knowledge
    curPackage = "exastencils.knowledge"

    entries += Entry(curPackage, "FutureKnowledgeAccess", L2_L3)
    entries += Entry(curPackage, "KnowledgeAccess", L2_L3)
    entries += Entry(curPackage, "KnowledgeCollection", L2_L3)
    entries += Entry(curPackage, "KnowledgeDecl", L2_L3)
    entries += Entry(curPackage, "KnowledgeObject", L2_L3)
    entries += Entry(curPackage, "KnowledgeContainer", L2_L3)
  }

  if (true) { // in exastencils.operator
    curPackage = "exastencils.operator"

    entries += Entry(curPackage, "BaseStencilDecl", L2_L3)
    entries += Entry(curPackage, "FutureStencilAccess", L2_L3)
    entries += Entry(curPackage, "Stencil", L2_L3)
    entries += Entry(curPackage, "StencilAccess", L2_L3)
    entries += Entry(curPackage, "StencilCollection", L2_L3)
    entries += Entry(curPackage, "StencilDecl", L2_L3)
    entries += Entry(curPackage, "StencilEntry", L2_L3)
    entries += Entry(curPackage, "StencilFromExpression", L2_L3)
    entries += Entry(curPackage, "StencilOps", L2_L3)

    entries += Entry(curPackage, "StencilTemplate", L2_L3)
    entries += Entry(curPackage, "StencilTemplateCollection", L2_L3)
    entries += Entry(curPackage, "StencilTemplateDecl", L2_L3)

    entries += Entry(curPackage, "StencilFromDefault", L3)
    entries += Entry(curPackage, "DefaultProlongation", L3)
    entries += Entry(curPackage, "DefaultRestriction", L3)
  }

  if (true) { // in exastencils.optimization
    curPackage = "exastencils.optimization"

    entries += Entry(curPackage, "FlattenComputation", L2_L3)
    entries += Entry(curPackage, "GeneralSimplify", L2_L3)
    entries += Entry(curPackage, "SimplifyExpression", L2_L3)
  }

  if (true) { // in exastencils.util
    curPackage = "exastencils.util"

    entries += Entry(curPackage, "MathFunctions", all)
    entries += Entry(curPackage, "ReplaceExpressions", L2_L3)
  }
}
