package meta

import scala.collection.mutable.ListBuffer

/// MuncherList

object MuncherList {

  import Layer._

  object Entry {def apply(filePackage : String, className : String, layer : Layer) = new Entry(filePackage, className, ListBuffer(layer)) }

  case class Entry(var filePackage : String, var className : String, var layers : ListBuffer[Layer])

  val entries = ListBuffer[Entry]()

  var curPackage = ""

  if (true) { // in exastencils.base
    curPackage = "exastencils.base"

    entries += Entry(curPackage, "Access", L2_L3)
    entries += Entry(curPackage, "BinaryOp", L2_L3_L4)
    entries += Entry(curPackage, "CanBeOffset", all)
    entries += Entry(curPackage, "Constant", L2_L3_L4)
    //entries += Entry(curPackage, "Datatype", all)
    entries += Entry(curPackage, "Equation", all)
    entries += Entry(curPackage, "Expression", all)
    entries += Entry(curPackage, "ImplicitConversion", all)
    entries += Entry(curPackage, "Index", all)
    entries += Entry(curPackage, "LevelAlias", L2_L3_L4)
    entries += Entry(curPackage, "LevelGroup", L2_L3_L4)
    entries += Entry(curPackage, "LevelSingle", L2_L3_L4)
    entries += Entry(curPackage, "LevelSpecification", L2_L3_L4)
    entries += Entry(curPackage, "MayBlockResolution", L2_L3_L4)
    entries += Entry(curPackage, "Node", all)
    entries += Entry(curPackage, "Progressable", L2_L3_L4)
    entries += Entry(curPackage, "Root", L2_L3)
    entries += Entry(curPackage, "Statement", all)
    entries += Entry(curPackage, "UnaryOp", L2_L3_L4)
    entries += Entry(curPackage, "VariableAccess", L2_L3)
    entries += Entry(curPackage, "VariableDeclaration", L2_L3)

    entries += Entry(curPackage, "DslFunctionReference", L3)
    entries += Entry(curPackage, "Function", L3)
    entries += Entry(curPackage, "FunctionReference", all)
    entries += Entry(curPackage, "FunctionCall", L3)
    entries += Entry(curPackage, "FunctionDecl", L3)
    entries += Entry(curPackage, "Return", L3)
  }

  if (true) { // in exastencils.baseExt
    curPackage = "exastencils.baseExt"

    entries += Entry(curPackage, "UnresolvedAccess", L2_L3_L4)
    entries += Entry(curPackage, "FieldIteratorAccess", all)
    entries += Entry(curPackage, "FunctionInstantiation", L3_L4)
    entries += Entry(curPackage, "GlobalSection", L2_L3_L4)
  }

  if (true) { // in exastencils.domain
    curPackage = "exastencils.domain"

    entries += Entry(curPackage, "DomainAccess", L2_L3_L4)
    entries += Entry(curPackage, "Domain", L2_L3_L4)
    entries += Entry(curPackage, "DomainCollection", L2_L3_L4)
    entries += Entry(curPackage, "DomainDecl", L2_L3_L4)
    entries += Entry(curPackage, "DomainFromAABB", L2_L3_L4)
    entries += Entry(curPackage, "FutureDomainAccess", L2_L3_L4)
  }

  if (true) { // in exastencils.field
    curPackage = "exastencils.field"

    entries += Entry(curPackage, "BaseFieldDecl", L2_L3)
    entries += Entry(curPackage, "BoundaryFieldDecl", L2_L3)
    entries += Entry(curPackage, "Field", L2_L3)
    entries += Entry(curPackage, "FieldAccess", L2_L3_L4)
    entries += Entry(curPackage, "FieldCollection", L2_L3)
    entries += Entry(curPackage, "FieldDecl", L2_L3)
    entries += Entry(curPackage, "FutureFieldAccess", L2_L3)

    entries += Entry(curPackage, "FieldFromOther", L3)
    entries += Entry(curPackage, "FieldOverride", L3)

    entries += Entry(curPackage, "Slot", L2_L3_L4)
  }

  if (true) { // in exastencils.grid
    curPackage = "exastencils.grid"

    entries += Entry(curPackage, "DefaultVirtualFields", L2_L3_L4)
    entries += Entry(curPackage, "FutureVirtualFieldAccess", L2_L3_L4)
    entries += Entry(curPackage, "VirtualField", all)
    entries += Entry(curPackage, "VirtualFieldAccess", all)
    entries += Entry(curPackage, "VirtualFieldCollection", all)

    entries += Entry(curPackage, "GridUtil", all)
    entries += Entry(curPackage, "Localization", all)

    entries += Entry(curPackage, "EvaluateFunctions", all)
    entries += Entry(curPackage, "EvaluateOnGrid", all)
    entries += Entry(curPackage, "IntegrateFunctions", L2_L3_L4)
    entries += Entry(curPackage, "IntegrateOnGrid", all)

    entries += Entry(curPackage, "VF_BoundaryPosition", all)
    entries += Entry(curPackage, "VF_CellCenter", all)
    entries += Entry(curPackage, "VF_CellVolume", all)
    entries += Entry(curPackage, "VF_CellWidth", all)
    entries += Entry(curPackage, "VF_NodePosition", all)
    entries += Entry(curPackage, "VF_StagCellVolume", all)
    entries += Entry(curPackage, "VF_StagCellWidth", all)
  }

  if (true) { // in exastencils.knowledge
    curPackage = "exastencils.knowledge"

    entries += Entry(curPackage, "FutureKnowledgeAccess", L2_L3_L4)
    entries += Entry(curPackage, "KnowledgeAccess", L2_L3_L4)
    entries += Entry(curPackage, "KnowledgeCollection", L2_L3_L4)
    entries += Entry(curPackage, "KnowledgeDecl", L2_L3_L4)
    entries += Entry(curPackage, "KnowledgeObject", L2_L3_L4)
    entries += Entry(curPackage, "KnowledgeContainer", L2_L3_L4)
  }

  if (true) { // in exastencils.operator
    curPackage = "exastencils.operator"

    entries += Entry(curPackage, "OperatorAccess", all)

    entries += Entry(curPackage, "BaseStencilDecl", L2_L3_L4)
    entries += Entry(curPackage, "FutureStencilAccess", L2_L3_L4)
    entries += Entry(curPackage, "Stencil", all)
    entries += Entry(curPackage, "StencilAccess", L2_L3_L4)
    entries += Entry(curPackage, "StencilCollection", L2_L3_L4)
    entries += Entry(curPackage, "StencilDecl", L2_L3_L4)
    entries += Entry(curPackage, "StencilEntry", L2_L3_L4)
    entries += Entry(curPackage, "StencilFromExpression", L2_L3_L4)

    entries += Entry(curPackage, "StencilOps", all)

    entries += Entry(curPackage, "FutureStencilFieldAccess", L2_L3_L4)
    entries += Entry(curPackage, "StencilField", L2_L3_L4)
    entries += Entry(curPackage, "StencilFieldAccess", L2_L3_L4)
    entries += Entry(curPackage, "StencilFieldCollection", L2_L3_L4)
    entries += Entry(curPackage, "StencilFieldDecl", L2_L3_L4)

    entries += Entry(curPackage, "StencilFromDefault", L2_L3_L4)
    entries += Entry(curPackage, "DefaultProlongation", L2_L3_L4)
    entries += Entry(curPackage, "DefaultRestriction", L2_L3_L4)
  }

  if (true) { // in exastencils.optimization
    curPackage = "exastencils.optimization"

    entries += Entry(curPackage, "FlattenComputation", all)
    entries += Entry(curPackage, "GeneralSimplify", all)
    entries += Entry(curPackage, "SimplifyExpression", all)
    entries += Entry(curPackage, "GeneralSimplifyWrapper", IR)
  }

  if (true) { // in exastencils.solver
    curPackage = "exastencils.solver"

    entries += Entry(curPackage, "EquationAccess", all)
    entries += Entry(curPackage, "EquationCollection", all)
    entries += Entry(curPackage, "EquationDecl", L2_L3_L4)
    entries += Entry(curPackage, "FutureEquationAccess", L2_L3_L4)
    entries += Entry(curPackage, "LocalSolve", ListBuffer(L3, L4, IR))
    entries += Entry(curPackage, "NamedEquation", all)
  }

  if (true) { // in exastencils.util
    curPackage = "exastencils.util"

    entries += Entry(curPackage, "MathFunctions", all)
    entries += Entry(curPackage, "ReplaceExpressions", L2_L3)
    entries += Entry(curPackage, "AABB", L4)

    entries += Entry(curPackage, "CollectFieldAccesses", all)

    entries += Entry(curPackage, "LevelCollector", L2_L3_L4)
    entries += Entry(curPackage, "StackCollector", IR)
    entries += Entry(curPackage, "VariableDeclarationCollector", L2_L3_L4)
  }
}
