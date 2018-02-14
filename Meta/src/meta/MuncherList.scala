package meta

import scala.collection.mutable.ListBuffer

/// MuncherList

object MuncherList {

  import Layer._

  object Entry {def apply(filePackage : String, className : String, layer : Layer) = new Entry(filePackage, className, ListBuffer(layer)) }

  case class Entry(var filePackage : String, var className : String, var layers : ListBuffer[Layer])

  val entries = ListBuffer[Entry]()

  var curPackage = ""

  // exastencils.app
  if (true) {
    curPackage = "exastencils.app"

    entries += Entry(curPackage, "LayerHandler", all)
  }

  // exastencils.base
  if (true) {
    curPackage = "exastencils.base"

    entries += Entry(curPackage, "FunctionReference", all)
    entries += Entry(curPackage, "Scope", L4AndUp)
    entries += Entry(curPackage, "FunctionCall", all)
    entries += Entry(curPackage, "Equation", all)
    entries += Entry(curPackage, "MayBlockResolution", allButIR)
    entries += Entry(curPackage, "Cast", IR)
    entries += Entry(curPackage, "VariableDeclaration", all)
    entries += Entry(curPackage, "Root", all)
    entries += Entry(curPackage, "LevelAlias", allButIR)
    entries += Entry(curPackage, "Datatype", all)
    entries += Entry(curPackage, "ExternalFunctionReference", IR)
    entries += Entry(curPackage, "UnaryOp", all)
    entries += Entry(curPackage, "LevelSingle", allButIR)
    entries += Entry(curPackage, "Node", all)
    entries += Entry(curPackage, "Assignment", L3AndUp)
    entries += Entry(curPackage, "Interval", L1)
    entries += Entry(curPackage, "FutureFunction", IR)
    entries += Entry(curPackage, "Expandable", IR)
    entries += Entry(curPackage, "Reduction", L4AndUp)
    entries += Entry(curPackage, "Bounded", IR)
    entries += Entry(curPackage, "LevelGroup", allButIR)
    entries += Entry(curPackage, "Progressable", allButIR)
    entries += Entry(curPackage, "Index", all)
    entries += Entry(curPackage, "InternalFunctionReference", all)
    entries += Entry(curPackage, "Class", IR)
    entries += Entry(curPackage, "BinaryOp", all)
    entries += Entry(curPackage, "FunctionDecl", L3_L4)
    entries += Entry(curPackage, "FunctionLike", IR)
    entries += Entry(curPackage, "Loop", L3AndUp)
    entries += Entry(curPackage, "VariableAccess", all)
    entries += Entry(curPackage, "Return", L3AndUp)
    entries += Entry(curPackage, "CanBeOffset", L2AndUp)
    entries += Entry(curPackage, "LevelSpecification", allButIR)
    entries += Entry(curPackage, "Constant", all)
    entries += Entry(curPackage, "DslFunctionReference", L3AndUp)
    entries += Entry(curPackage, "PointerOffset", IR)
    entries += Entry(curPackage, "ImplicitConversion", all)
    entries += Entry(curPackage, "Access", all)
    entries += Entry(curPackage, "Function", L3AndUp)
    entries += Entry(curPackage, "Condition", L3AndUp)
    entries += Entry(curPackage, "Alloc", IR)
    entries += Entry(curPackage, "Statement", all)
    entries += Entry(curPackage, "Misc", IR)
    entries += Entry(curPackage, "Expression", all)
  }

  // exastencils.baseExt
  if (true) {
    curPackage = "exastencils.baseExt"

    entries += Entry(curPackage, "ApplicationHints", L1_L2_L3)
    entries += Entry(curPackage, "HigherDimSelection", L2_L3_L4)
    entries += Entry(curPackage, "LoopOverFields", IR)
    entries += Entry(curPackage, "VectorAccess", L4AndUp)
    entries += Entry(curPackage, "HigherDimDatatype", L2_L3)
    entries += Entry(curPackage, "LoopOverDomains", IR)
    entries += Entry(curPackage, "LoopOverField", L4)
    entries += Entry(curPackage, "LoopOverFragments", L4AndUp)
    entries += Entry(curPackage, "UserFunctions", IR)
    entries += Entry(curPackage, "GlobalSection", allButIR)
    entries += Entry(curPackage, "FunctionTemplate", L3_L4)
    entries += Entry(curPackage, "ResolveLoopItAccesses", L4)
    entries += Entry(curPackage, "LoopOverPointsInOneFragment", IR)
    entries += Entry(curPackage, "FunctionInstantiation", L3_L4)
    entries += Entry(curPackage, "FunctionCollection", IR)
    entries += Entry(curPackage, "LoopOverDimensions", IR)
    entries += Entry(curPackage, "ColorLoops", L3_L4)
    entries += Entry(curPackage, "Linearization", IR)
    entries += Entry(curPackage, "MatrixAccess", L4AndUp)
    entries += Entry(curPackage, "FieldIteratorAccess", L2AndUp)
    entries += Entry(curPackage, "LoopOverNeighbors", IR)
    entries += Entry(curPackage, "HigherDimensionalDatatype", L4AndUp)
    entries += Entry(curPackage, "UnresolvedAccess", allButIR)
    entries += Entry(curPackage, "LevelScope", L3_L4)
    entries += Entry(curPackage, "IndexRange", IR)
    entries += Entry(curPackage, "VectorExpression", L2_L3)
    entries += Entry(curPackage, "InternalVariable", IR)
    entries += Entry(curPackage, "LoopOverPoints", IR)
    entries += Entry(curPackage, "LoopOverLevels", IR)
    entries += Entry(curPackage, "ContractingLoop", L4AndUp)
  }

  // exastencils.boundary
  if (true) {
    curPackage = "exastencils.boundary"

    entries += Entry(curPackage, "HandleBoundaries", IR)
    entries += Entry(curPackage, "FunctionBC", all)
    entries += Entry(curPackage, "BoundaryCondition", all)
    entries += Entry(curPackage, "IsOnBoundary", IR)
    entries += Entry(curPackage, "ApplyBC", L4AndUp)
    entries += Entry(curPackage, "ApplyBCFunction", IR)
    entries += Entry(curPackage, "NeumannBC", all)
    entries += Entry(curPackage, "DirichletBC", all)
  }

  // exastencils.communication
  if (true) {
    curPackage = "exastencils.communication"

    entries += Entry(curPackage, "RemoteCommunication", IR)
    entries += Entry(curPackage, "LocalCommunicationFinish", IR)
    entries += Entry(curPackage, "CommVariable", IR)
    entries += Entry(curPackage, "CommunicateFunction", IR)
    entries += Entry(curPackage, "RemoteCommunicationStart", IR)
    entries += Entry(curPackage, "RemoteRecv", IR)
    entries += Entry(curPackage, "RemoteCommunicationFlags", IR)
    entries += Entry(curPackage, "Communicate", L4AndUp)
    entries += Entry(curPackage, "LocalRecv", IR)
    entries += Entry(curPackage, "AdaptFieldLayoutsForComm", L4)
    entries += Entry(curPackage, "AddCommunicationToLoops", L4)
    entries += Entry(curPackage, "MergeCommunicateAndLoop", IR)
    entries += Entry(curPackage, "WaitForRemoteTransfer", IR)
    entries += Entry(curPackage, "CommunicationFunctions", IR)
    entries += Entry(curPackage, "CommunicationId", IR)
    entries += Entry(curPackage, "SplitLoops", IR)
    entries += Entry(curPackage, "TempBuffer", IR)
    entries += Entry(curPackage, "FieldAccessRangeCollector", L4)
    entries += Entry(curPackage, "CommBuffer", IR)
    entries += Entry(curPackage, "LocalCommunicationFlags", IR)
    entries += Entry(curPackage, "RemoteSend", IR)
    entries += Entry(curPackage, "LocalCommunication", IR)
    entries += Entry(curPackage, "RemoteCommunicationFinish", IR)
    entries += Entry(curPackage, "SetupCommunication", IR)
    entries += Entry(curPackage, "LocalCommunicationStart", IR)
    entries += Entry(curPackage, "LocalSend", IR)
  }

  // exastencils.deprecated
  if (true) {
    curPackage = "exastencils.deprecated"

    entries += Entry(curPackage, "PrimitiveTransformation", IR)
    entries += Entry(curPackage, "AddDefaultGlobals", IR)
    entries += Entry(curPackage, "UnifyInnerTypes", IR)
    entries += Entry(curPackage, "StencilFieldSelection", IR)
    entries += Entry(curPackage, "GeometricCoordinates", IR)
    entries += Entry(curPackage, "DimToString", IR)
    entries += Entry(curPackage, "ConstVec", L4)
    entries += Entry(curPackage, "CallTracker", IR)
    entries += Entry(curPackage, "Matrix", IR)
    entries += Entry(curPackage, "FieldSelection", IR)
  }

  // exastencils.deprecated.domain
  if (true) {
    curPackage = "exastencils.deprecated.domain"

    entries += Entry(curPackage, "DomainFromFile", IR)
  }

  // exastencils.discretization
  if (true) {
    curPackage = "exastencils.discretization"

    entries += Entry(curPackage, "OperatorDiscretization", L1)
    entries += Entry(curPackage, "FD_LagrangeApproach", L1)
    entries += Entry(curPackage, "EquationDiscretization", L1)
    entries += Entry(curPackage, "FieldDiscretization", L1)
    entries += Entry(curPackage, "FD_TaylorApproach", L1)
    entries += Entry(curPackage, "FD_DiscretizeSubtree", L1)
    entries += Entry(curPackage, "DiscretizationHints", L1)
    entries += Entry(curPackage, "FD_Approach", L1)
    entries += Entry(curPackage, "FD_EqSystem", L1)
  }

  // exastencils.domain
  if (true) {
    curPackage = "exastencils.domain"

    entries += Entry(curPackage, "FragmentConnection", IR)
    entries += Entry(curPackage, "InitGeneratedDomain", IR)
    entries += Entry(curPackage, "DomainFromAABB", all)
    entries += Entry(curPackage, "IterationOffset", IR)
    entries += Entry(curPackage, "DomainCollection", all)
    entries += Entry(curPackage, "DomainFunctions", IR)
    entries += Entry(curPackage, "Domain", all)
    entries += Entry(curPackage, "HACK_DomainDecl", L4)
    entries += Entry(curPackage, "FutureDomainAccess", allButIR)
    entries += Entry(curPackage, "FragmentPosition", IR)
    entries += Entry(curPackage, "DomainFromIntervalsDecl", L1)
    entries += Entry(curPackage, "ConnectFragments", IR)
    entries += Entry(curPackage, "FragmentData", IR)
    entries += Entry(curPackage, "DomainDecl", allButIR)
    entries += Entry(curPackage, "DomainAccess", allButIR)
  }

  // exastencils.field
  if (true) {
    curPackage = "exastencils.field"

    entries += Entry(curPackage, "BaseFieldDecl", allButIR)
    entries += Entry(curPackage, "InitFieldsWithZero", IR)
    entries += Entry(curPackage, "FieldLayoutCollection", L4AndUp)
    entries += Entry(curPackage, "AddLoopsToFieldAssignments", L4)
    entries += Entry(curPackage, "BoundaryFieldDecl", L1_L2_L3)
    entries += Entry(curPackage, "FieldFromOther", L3)
    entries += Entry(curPackage, "IndexFromField", IR)
    entries += Entry(curPackage, "FieldLayout", L4AndUp)
    entries += Entry(curPackage, "FieldLayoutAccess", L4)
    entries += Entry(curPackage, "FieldAccess", all)
    entries += Entry(curPackage, "FieldLayoutAdaption", L4)
    entries += Entry(curPackage, "FutureFieldLayoutAccess", L4)
    entries += Entry(curPackage, "FieldData", IR)
    entries += Entry(curPackage, "Slot", L2AndUp)
    entries += Entry(curPackage, "FieldOverride", L3)
    entries += Entry(curPackage, "FutureFieldAccess", allButIR)
    entries += Entry(curPackage, "GenerateIndexManipFcts", IR)
    entries += Entry(curPackage, "FieldDecl", allButIR)
    entries += Entry(curPackage, "FieldFieldConvolution", L3_L4)
    entries += Entry(curPackage, "IntroduceSlots", L3_L4)
    entries += Entry(curPackage, "FieldFlag", IR)
    entries += Entry(curPackage, "FieldLayoutDecl", L4)
    entries += Entry(curPackage, "FieldCollection", all)
    entries += Entry(curPackage, "AddPaddingToFieldLayouts", IR)
    entries += Entry(curPackage, "PrintField", L4AndUp)
    entries += Entry(curPackage, "Field", all)
  }

  // exastencils.globals
  if (true) {
    curPackage = "exastencils.globals"

    entries += Entry(curPackage, "AddInternalVariables", IR)
    entries += Entry(curPackage, "AllocateDataFunction", IR)
    entries += Entry(curPackage, "AddDefaultApplication", L4)
    entries += Entry(curPackage, "GlobalCollection", IR)
    entries += Entry(curPackage, "ResolveConstIVs", IR)
  }

  // exastencils.grid
  if (true) {
    curPackage = "exastencils.grid"

    entries += Entry(curPackage, "Localization", all)
    entries += Entry(curPackage, "VF_BoundaryPosition", L2AndUp)
    entries += Entry(curPackage, "VF_CellCenter", L2AndUp)
    entries += Entry(curPackage, "DefaultVirtualFields", L2_L3_L4)
    entries += Entry(curPackage, "VF_CellWidth", L2AndUp)
    entries += Entry(curPackage, "CollectFieldAccess", IR)
    entries += Entry(curPackage, "IntegrateOnGrid", L2AndUp)
    entries += Entry(curPackage, "VirtualFieldCollection", L2AndUp)
    entries += Entry(curPackage, "SetupStagCellWidth", IR)
    entries += Entry(curPackage, "VF_StagCellWidth", L2AndUp)
    entries += Entry(curPackage, "VF_CellVolume", L2AndUp)
    entries += Entry(curPackage, "IntegrateFunctions", L2_L3_L4)
    entries += Entry(curPackage, "SetupCellCenter", IR)
    entries += Entry(curPackage, "SetupNodePositions", IR)
    entries += Entry(curPackage, "EvaluateOnGrid", L2AndUp)
    entries += Entry(curPackage, "FutureVirtualFieldAccess", L2_L3_L4)
    entries += Entry(curPackage, "EvaluateFunctions", L2AndUp)
    entries += Entry(curPackage, "GridUtil", L2AndUp)
    entries += Entry(curPackage, "VF_NodePosition", L2AndUp)
    entries += Entry(curPackage, "VirtualFieldAccess", L2AndUp)
    entries += Entry(curPackage, "VirtualField", L2AndUp)
    entries += Entry(curPackage, "VF_StagCellVolume", L2AndUp)
  }

  // exastencils.hack
  if (true) {
    curPackage = "exastencils.hack"

    entries += Entry(curPackage, "UndeterminedFunctionAccess", IR)
    entries += Entry(curPackage, "ResolveSpecialFunctionsAndConstants", IR)
    entries += Entry(curPackage, "Native", L4AndUp)
  }

  // exastencils.interfacing
  if (true) {
    curPackage = "exastencils.interfacing"

    entries += Entry(curPackage, "ExternalFieldAccess", IR)
    entries += Entry(curPackage, "CopyToExternalField", IR)
    entries += Entry(curPackage, "ExternalFieldCollection", L4AndUp)
    entries += Entry(curPackage, "CopyFromExternalField", IR)
    entries += Entry(curPackage, "ExternalFieldDecl", L4)
    entries += Entry(curPackage, "Fortranify", IR)
    entries += Entry(curPackage, "ExternalField", L4AndUp)
  }

  // exastencils.knowledge
  if (true) {
    curPackage = "exastencils.knowledge"

    entries += Entry(curPackage, "ProgressKnowledge", L4)
    entries += Entry(curPackage, "KnowledgeDecl", allButIR)
    entries += Entry(curPackage, "InlineKnowledge", all)
    entries += Entry(curPackage, "KnowledgeContainer", allButIR)
    entries += Entry(curPackage, "FutureKnowledgeAccess", allButIR)
    entries += Entry(curPackage, "KnowledgeAccess", all)
    entries += Entry(curPackage, "ResolveKnowledgeParameterAccess", L4)
    entries += Entry(curPackage, "KnowledgeObject", all)
    entries += Entry(curPackage, "ProcessKnowledgeDeclarations", L4)
    entries += Entry(curPackage, "ClearKnowledge", IR)
    entries += Entry(curPackage, "PrintKnowledgeDecl", L4)
    entries += Entry(curPackage, "KnowledgeCollection", all)
  }

  // exastencils.layoutTransformation
  if (true) {
    curPackage = "exastencils.layoutTransformation"

    entries += Entry(curPackage, "LayoutTransformStatement", L4AndUp)
    entries += Entry(curPackage, "LayoutTansformation", IR)
    entries += Entry(curPackage, "LayoutSection", L4)
    entries += Entry(curPackage, "LayoutTransformationCollection", IR)
  }

  // exastencils.operator
  if (true) {
    curPackage = "exastencils.operator"

    entries += Entry(curPackage, "FutureOperatorAccess", L1)
    entries += Entry(curPackage, "OperatorFromEquation", L2)
    entries += Entry(curPackage, "StencilFieldDecl", L2_L3_L4)
    entries += Entry(curPackage, "Stencil", all)
    entries += Entry(curPackage, "StencilField", L2AndUp)
    entries += Entry(curPackage, "Operator", ListBuffer(L1, IR))
    entries += Entry(curPackage, "OperatorDecl", L1)
    entries += Entry(curPackage, "GenerateStencilFromEquation", L2)
    entries += Entry(curPackage, "DefaultProlongation", L2_L3_L4)
    entries += Entry(curPackage, "StencilFromDefault", L2_L3_L4)
    entries += Entry(curPackage, "OperatorFromEqEntry", L2)
    entries += Entry(curPackage, "StencilOps", all)
    entries += Entry(curPackage, "FutureStencilFieldAccess", L2_L3_L4)
    entries += Entry(curPackage, "DefaultRestriction", L2_L3_L4)
    entries += Entry(curPackage, "StencilCollection", L2AndUp)
    entries += Entry(curPackage, "OffsetAccesses", L4)
    entries += Entry(curPackage, "OperatorTimesOperator", L4AndUp)
    entries += Entry(curPackage, "StencilFunctions", L4)
    entries += Entry(curPackage, "OperatorTimesField", L3AndUp)
    entries += Entry(curPackage, "OperatorAccess", all)
    entries += Entry(curPackage, "StencilDecl", L2_L3_L4)
    entries += Entry(curPackage, "BaseStencilDecl", L2_L3_L4)
    entries += Entry(curPackage, "StencilAccess", all)
    entries += Entry(curPackage, "StencilFieldAccess", L2AndUp)
    entries += Entry(curPackage, "PartialDerivative", L1)
    entries += Entry(curPackage, "StencilFieldCollection", L2AndUp)
    entries += Entry(curPackage, "OperatorCollection", L1)
    entries += Entry(curPackage, "StencilEntry", all)
    entries += Entry(curPackage, "FutureStencilAccess", L2_L3_L4)
    entries += Entry(curPackage, "StencilFromExpression", L2_L3_L4)
  }

  // exastencils.optimization
  if (true) {
    curPackage = "exastencils.optimization"

    entries += Entry(curPackage, "FlattenComputation", all)
    entries += Entry(curPackage, "TypeInference", IR)
    entries += Entry(curPackage, "MergeConditions", IR)
    entries += Entry(curPackage, "Inlining", IR)
    entries += Entry(curPackage, "GeneralSimplify", all)
    entries += Entry(curPackage, "CommonSubexpressionElimination", IR)
    entries += Entry(curPackage, "Vectorization", IR)
    entries += Entry(curPackage, "RemoveDupSIMDLoads", IR)
    entries += Entry(curPackage, "GeneralSimplifyWrapper", all)
    entries += Entry(curPackage, "SimplifyExpression", all)
    entries += Entry(curPackage, "AddressPrecalculation", IR)
    entries += Entry(curPackage, "SimplifyFloatExpressions", IR)
    entries += Entry(curPackage, "Unrolling", IR)
  }

  // exastencils.parallelization
  if (true) {
    curPackage = "exastencils.parallelization"

    entries += Entry(curPackage, "ParallelizationInfo", IR)
    entries += Entry(curPackage, "PotentiallyCritical", IR)
  }

  // exastencils.parsers
  if (true) {
    curPackage = "exastencils.parsers"

    entries += Entry(curPackage, "Parser", allButIR)
    entries += Entry(curPackage, "Validation", allButIR)
    entries += Entry(curPackage, "Lexer", L2_L3_L4)//allButIR)
  }

  // exastencils.performance
  if (true) {
    curPackage = "exastencils.performance"

    entries += Entry(curPackage, "SlotAccessAsConst", IR)
  }

  // exastencils.solver
  if (true) {
    curPackage = "exastencils.solver"

    entries += Entry(curPackage, "SolverModification", L3)
    entries += Entry(curPackage, "SolverHints", ListBuffer(L1, L2))
    entries += Entry(curPackage, "SolverForEqEntry", L1_L2_L3)
    entries += Entry(curPackage, "VankaAsSolverForEquation", L3)
    entries += Entry(curPackage, "VankaForEquation", L3)
    entries += Entry(curPackage, "EquationAccess", all)
    entries += Entry(curPackage, "ConjugateGradientForEquation", L3)
    entries += Entry(curPackage, "LocalSchurCompl", IR)
    entries += Entry(curPackage, "ResolveIntergridIndices", IR)
    entries += Entry(curPackage, "LocalSolve", L3AndUp)
    entries += Entry(curPackage, "NamedEquation", all)
    entries += Entry(curPackage, "MinResForEquation", L3)
    entries += Entry(curPackage, "EquationCollection", all)
    entries += Entry(curPackage, "SolverForEquation", L1_L2_L3)
    entries += Entry(curPackage, "ConjugateResidualForEquation", L3)
    entries += Entry(curPackage, "EquationDecl", allButIR)
    entries += Entry(curPackage, "IterativeSolverForEquation", L3)
    entries += Entry(curPackage, "FutureEquationAccess", allButIR)
    entries += Entry(curPackage, "GenerateSmootherHint", L3)
    entries += Entry(curPackage, "BiCGStabForEquation", L3)
    entries += Entry(curPackage, "LocalDirectInvert", IR)
  }

  // exastencils.stencil
  if (true) {
    curPackage = "exastencils.stencil"

    entries += Entry(curPackage, "StencilConvolution", IR)
    entries += Entry(curPackage, "StencilStencilConvolution", IR)
    entries += Entry(curPackage, "StencilFunction", IR)
    entries += Entry(curPackage, "FindStencilConvolutions", IR)
    entries += Entry(curPackage, "MapStencilAssignments", IR)
  }

  // exastencils.timing
  if (true) {
    curPackage = "exastencils.timing"

    entries += Entry(curPackage, "PrintAllTimers", IR)
    entries += Entry(curPackage, "CollectTimers", IR)
    entries += Entry(curPackage, "TimerFunction", IR)
    entries += Entry(curPackage, "StartTimer", IR)
    entries += Entry(curPackage, "TimerDetail", IR)
    entries += Entry(curPackage, "Timer", IR)
    entries += Entry(curPackage, "PrintAllTimersToFile", IR)
    entries += Entry(curPackage, "Stopwatch", IR)
    entries += Entry(curPackage, "StopTimer", IR)
    entries += Entry(curPackage, "TimerFunctions", L4AndUp)
    entries += Entry(curPackage, "GetTime", IR)
  }

  // exastencils.util
  if (true) {
    curPackage = "exastencils.util"

    entries += Entry(curPackage, "BuildString", L4AndUp)
    entries += Entry(curPackage, "ResultingDatatype", IR)
    entries += Entry(curPackage, "StackCollector", all)
    entries += Entry(curPackage, "FctNameCollector", IR)
    entries += Entry(curPackage, "ScopeCollector", IR)
    entries += Entry(curPackage, "ReplaceVariableAccess", IR)
    entries += Entry(curPackage, "Print", L4AndUp)
    entries += Entry(curPackage, "LevelCollector", all)
    entries += Entry(curPackage, "ReplaceExpressions", L2AndUp)
    entries += Entry(curPackage, "ResolveSpecialConstants", L4)
    entries += Entry(curPackage, "AABB", all)
    entries += Entry(curPackage, "CollectFieldAccesses", L2AndUp)
    entries += Entry(curPackage, "MathFunctions", all)
    entries += Entry(curPackage, "GeneralParameter", all)
    entries += Entry(curPackage, "VariableDeclarationCollector", allButIR)
  }
}
