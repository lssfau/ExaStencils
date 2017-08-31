package meta

import scala.collection.mutable.ListBuffer

import exastencils.solver.meta._
import exastencils.optimization.meta._
import exastencils.base.meta._
import exastencils.baseExt.meta._
import exastencils.util.meta._
import exastencils.field.meta._
import exastencils.knowledge.meta._
import exastencils.operator.meta._
import exastencils.grid.meta._
import exastencils.domain.meta._

/// GeneratorList

object GeneratorList {
  val entries = ListBuffer[Generatable]()

  // in exastencils.solver

  entries += ME_EquationAccess
  entries += ME_EquationCollection
  entries += ME_EquationDecl
  entries += ME_FutureEquationAccess
  entries += ME_LocalSolve
  entries += ME_NamedEquation

  // in exastencils.optimization

  entries += ME_FlattenComputation
  entries += ME_GeneralSimplify
  entries += ME_SimplifyExpression
  entries += ME_GeneralSimplifyWrapper

  // in exastencils.base

  entries += ME_Access
  entries += ME_BinaryOp
  entries += ME_CanBeOffset
  entries += ME_Constant
  entries += ME_Equation
  entries += ME_Expression
  entries += ME_ImplicitConversion
  entries += ME_Index
  entries += ME_LevelAlias
  entries += ME_LevelGroup
  entries += ME_LevelSingle
  entries += ME_LevelSpecification
  entries += ME_MayBlockResolution
  entries += ME_Node
  entries += ME_Progressable
  entries += ME_Root
  entries += ME_Statement
  entries += ME_UnaryOp
  entries += ME_VariableAccess
  entries += ME_VariableDeclaration
  entries += ME_DslFunctionReference
  entries += ME_Function
  entries += ME_FunctionReference
  entries += ME_FunctionCall
  entries += ME_FunctionDecl
  entries += ME_Return

  // in exastencils.baseExt

  entries += ME_UnresolvedAccess
  entries += ME_FieldIteratorAccess
  entries += ME_FunctionInstantiation
  entries += ME_GlobalSection

  // in exastencils.util

  entries += ME_MathFunctions
  entries += ME_ReplaceExpressions
  entries += ME_AABB
  entries += ME_CollectFieldAccesses
  entries += ME_LevelCollector
  entries += ME_StackCollector
  entries += ME_VariableDeclarationCollector

  // in exastencils.field

  entries += ME_BaseFieldDecl
  entries += ME_BoundaryFieldDecl
  entries += ME_Field
  entries += ME_FieldAccess
  entries += ME_FieldCollection
  entries += ME_FieldDecl
  entries += ME_FutureFieldAccess
  entries += ME_FieldFromOther
  entries += ME_FieldOverride
  entries += ME_Slot

  // in exastencils.knowledge

  entries += ME_FutureKnowledgeAccess
  entries += ME_KnowledgeAccess
  entries += ME_KnowledgeCollection
  entries += ME_KnowledgeDecl
  entries += ME_KnowledgeObject
  entries += ME_KnowledgeContainer

  // in exastencils.operator

  entries += ME_OperatorAccess
  entries += ME_BaseStencilDecl
  entries += ME_FutureStencilAccess
  entries += ME_Stencil
  entries += ME_StencilAccess
  entries += ME_StencilCollection
  entries += ME_StencilDecl
  entries += ME_StencilEntry
  entries += ME_StencilFromExpression
  entries += ME_StencilOps
  entries += ME_FutureStencilFieldAccess
  entries += ME_StencilField
  entries += ME_StencilFieldAccess
  entries += ME_StencilFieldCollection
  entries += ME_StencilFieldDecl
  entries += ME_StencilFromDefault
  entries += ME_DefaultProlongation
  entries += ME_DefaultRestriction

  // in exastencils.grid

  entries += ME_DefaultVirtualFields
  entries += ME_FutureVirtualFieldAccess
  entries += ME_VirtualField
  entries += ME_VirtualFieldAccess
  entries += ME_VirtualFieldCollection
  entries += ME_GridUtil
  entries += ME_Localization
  entries += ME_EvaluateFunctions
  entries += ME_EvaluateOnGrid
  entries += ME_IntegrateFunctions
  entries += ME_IntegrateOnGrid
  entries += ME_VF_BoundaryPosition
  entries += ME_VF_CellCenter
  entries += ME_VF_CellVolume
  entries += ME_VF_CellWidth
  entries += ME_VF_NodePosition
  entries += ME_VF_StagCellVolume
  entries += ME_VF_StagCellWidth

  // in exastencils.domain

  entries += ME_DomainAccess
  entries += ME_Domain
  entries += ME_DomainCollection
  entries += ME_DomainDecl
  entries += ME_DomainFromAABB
  entries += ME_FutureDomainAccess

}
