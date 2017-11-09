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

    entries += Entry(curPackage, "Access", all)
    entries += Entry(curPackage, "BinaryOp", all)
    entries += Entry(curPackage, "CanBeOffset", L2AndUp)
    entries += Entry(curPackage, "Constant", all)
    entries += Entry(curPackage, "Datatype", all)
    entries += Entry(curPackage, "Equation", all)
    entries += Entry(curPackage, "Expression", all)
    entries += Entry(curPackage, "ImplicitConversion", all)
    entries += Entry(curPackage, "Index", all)
    entries += Entry(curPackage, "LevelAlias", allButIR)
    entries += Entry(curPackage, "LevelGroup", allButIR)
    entries += Entry(curPackage, "LevelSingle", allButIR)
    entries += Entry(curPackage, "LevelSpecification", allButIR)
    entries += Entry(curPackage, "MayBlockResolution", allButIR)
    entries += Entry(curPackage, "Node", all)
    entries += Entry(curPackage, "Progressable", allButIR)
    entries += Entry(curPackage, "Root", all)
    entries += Entry(curPackage, "Statement", all)
    entries += Entry(curPackage, "UnaryOp", all)
    entries += Entry(curPackage, "VariableAccess", all)
    entries += Entry(curPackage, "VariableDeclaration", all)

    entries += Entry(curPackage, "DslFunctionReference", L3AndUp)
    entries += Entry(curPackage, "Function", L3AndUp)
    entries += Entry(curPackage, "FunctionReference", all)
    entries += Entry(curPackage, "FunctionCall", L3AndUp)
    entries += Entry(curPackage, "FunctionDecl", L3_L4)
    entries += Entry(curPackage, "InternalFunctionReference", L3AndUp)
    entries += Entry(curPackage, "Return", L3AndUp)
  }

  if (true) { // in exastencils.baseExt
    curPackage = "exastencils.baseExt"

    entries += Entry(curPackage, "ApplicationHints", L1_L2_L3)
    entries += Entry(curPackage, "UnresolvedAccess", allButIR)
    entries += Entry(curPackage, "FieldIteratorAccess", L2AndUp)
    entries += Entry(curPackage, "FunctionInstantiation", L3_L4)
    entries += Entry(curPackage, "GlobalSection", allButIR)
  }

  if (true) { // in exastencils.boundary
    curPackage = "exastencils.boundary"

    entries += Entry(curPackage, "BoundaryCondition", all)
    entries += Entry(curPackage, "DirichletBC", all)
    entries += Entry(curPackage, "FunctionBC", all)
    entries += Entry(curPackage, "NeumannBC", all)
  }

  if (true) { // in exastencils.domain
    curPackage = "exastencils.domain"

    entries += Entry(curPackage, "DomainAccess", allButIR)
    entries += Entry(curPackage, "Domain", all)
    entries += Entry(curPackage, "DomainCollection", all)
    entries += Entry(curPackage, "DomainDecl", allButIR)
    entries += Entry(curPackage, "DomainFromAABB", all)
    entries += Entry(curPackage, "FutureDomainAccess", allButIR)
  }

  if (true) { // in exastencils.field
    curPackage = "exastencils.field"

    entries += Entry(curPackage, "BaseFieldDecl", allButIR)
    entries += Entry(curPackage, "BoundaryFieldDecl", L1_L2_L3)
    entries += Entry(curPackage, "Field", all)
    entries += Entry(curPackage, "FieldAccess", all)
    entries += Entry(curPackage, "FieldCollection", all)
    entries += Entry(curPackage, "FieldDecl", allButIR)
    entries += Entry(curPackage, "FutureFieldAccess", allButIR)

    entries += Entry(curPackage, "FieldFromOther", L3)
    entries += Entry(curPackage, "FieldOverride", L3)

    entries += Entry(curPackage, "Slot", L2_L3_L4)
  }

  if (true) { // in exastencils.grid
    curPackage = "exastencils.grid"

    entries += Entry(curPackage, "DefaultVirtualFields", L2_L3_L4)
    entries += Entry(curPackage, "FutureVirtualFieldAccess", L2_L3_L4)
    entries += Entry(curPackage, "VirtualField", L2AndUp)
    entries += Entry(curPackage, "VirtualFieldAccess", L2AndUp)
    entries += Entry(curPackage, "VirtualFieldCollection", L2AndUp)

    entries += Entry(curPackage, "GridUtil", L2AndUp)
    entries += Entry(curPackage, "Localization", all)

    entries += Entry(curPackage, "EvaluateFunctions", L2AndUp)
    entries += Entry(curPackage, "EvaluateOnGrid", L2AndUp)
    entries += Entry(curPackage, "IntegrateFunctions", L2_L3_L4)
    entries += Entry(curPackage, "IntegrateOnGrid", L2AndUp)

    entries += Entry(curPackage, "VF_BoundaryPosition", L2AndUp)
    entries += Entry(curPackage, "VF_CellCenter", L2AndUp)
    entries += Entry(curPackage, "VF_CellVolume", L2AndUp)
    entries += Entry(curPackage, "VF_CellWidth", L2AndUp)
    entries += Entry(curPackage, "VF_NodePosition", L2AndUp)
    entries += Entry(curPackage, "VF_StagCellVolume", L2AndUp)
    entries += Entry(curPackage, "VF_StagCellWidth", L2AndUp)
  }

  if (true) { // in exastencils.knowledge
    curPackage = "exastencils.knowledge"

    entries += Entry(curPackage, "FutureKnowledgeAccess", allButIR)
    entries += Entry(curPackage, "KnowledgeAccess", all)
    entries += Entry(curPackage, "KnowledgeCollection", all)
    entries += Entry(curPackage, "KnowledgeDecl", allButIR)
    entries += Entry(curPackage, "KnowledgeObject", all)
    entries += Entry(curPackage, "KnowledgeContainer", allButIR)

    entries += Entry(curPackage, "InlineKnowledge", all)
  }

  if (true) { // in exastencils.operator
    curPackage = "exastencils.operator"

    entries += Entry(curPackage, "OperatorAccess", all)

    entries += Entry(curPackage, "BaseStencilDecl", L2_L3_L4)
    entries += Entry(curPackage, "FutureStencilAccess", L2_L3_L4)
    entries += Entry(curPackage, "Stencil", all)
    entries += Entry(curPackage, "StencilAccess", all)
    entries += Entry(curPackage, "StencilCollection", L2AndUp)
    entries += Entry(curPackage, "StencilDecl", L2_L3_L4)
    entries += Entry(curPackage, "StencilEntry", all)
    entries += Entry(curPackage, "StencilFromExpression", L2_L3_L4)

    entries += Entry(curPackage, "StencilOps", all)

    entries += Entry(curPackage, "FutureStencilFieldAccess", L2_L3_L4)
    entries += Entry(curPackage, "StencilField", L2AndUp)
    entries += Entry(curPackage, "StencilFieldAccess", L2AndUp)
    entries += Entry(curPackage, "StencilFieldCollection", L2AndUp)
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
    entries += Entry(curPackage, "GeneralSimplifyWrapper", all)
  }

  if (true) { // in exastencils.solver
    curPackage = "exastencils.solver"

    entries += Entry(curPackage, "EquationAccess", all)
    entries += Entry(curPackage, "EquationCollection", all)
    entries += Entry(curPackage, "EquationDecl", allButIR)
    entries += Entry(curPackage, "FutureEquationAccess", allButIR)
    entries += Entry(curPackage, "LocalSolve", L3AndUp)
    entries += Entry(curPackage, "NamedEquation", all)
  }

  if (true) { // in exastencils.util
    curPackage = "exastencils.util"

    entries += Entry(curPackage, "MathFunctions", all)
    entries += Entry(curPackage, "ReplaceExpressions", L2AndUp)
    entries += Entry(curPackage, "AABB", all)

    entries += Entry(curPackage, "CollectFieldAccesses", L2AndUp)

    entries += Entry(curPackage, "LevelCollector", all)
    entries += Entry(curPackage, "StackCollector", all)
    entries += Entry(curPackage, "VariableDeclarationCollector", allButIR)

    entries += Entry(curPackage, "GeneralParameter", all)
  }
}
