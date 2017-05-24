package meta

import scala.collection.mutable.ListBuffer

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

  // in exastencils.optimization

  entries += ME_FlattenComputation
  entries += ME_GeneralSimplify
  entries += ME_SimplifyExpression

  // in exastencils.base

  entries += ME_Access
  entries += ME_BinaryOp
  entries += ME_Constant
  entries += ME_Expression
  entries += ME_FunctionAccess
  entries += ME_ImplicitConversion
  entries += ME_LevelAlias
  entries += ME_LevelCollector
  entries += ME_LevelGroup
  entries += ME_LevelSingle
  entries += ME_LevelSpecification
  entries += ME_Node
  entries += ME_Progressable
  entries += ME_Root
  entries += ME_Statement
  entries += ME_UnaryOp

  // in exastencils.baseExt

  entries += ME_FieldIteratorAccess

  // in exastencils.util

  entries += ME_MathFunctions
  entries += ME_ReplaceExpressions

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

  // in exastencils.knowledge

  entries += ME_FutureKnowledgeAccess
  entries += ME_KnowledgeAccess
  entries += ME_KnowledgeCollection
  entries += ME_KnowledgeDecl
  entries += ME_KnowledgeObject
  entries += ME_KnowledgeContainer

  // in exastencils.operator

  entries += ME_BaseStencilDecl
  entries += ME_FutureStencilAccess
  entries += ME_Stencil
  entries += ME_StencilAccess
  entries += ME_StencilCollection
  entries += ME_StencilDecl
  entries += ME_StencilEntry
  entries += ME_StencilFromExpression
  entries += ME_StencilOps
  entries += ME_StencilTemplate
  entries += ME_StencilTemplateCollection
  entries += ME_StencilTemplateDecl
  entries += ME_StencilFromDefault
  entries += ME_DefaultProlongation
  entries += ME_DefaultRestriction

  // in exastencils.grid

  entries += ME_DefaultVirtualFields
  entries += ME_FutureVirtualFieldAccess
  entries += ME_VirtualField
  entries += ME_VirtualFieldAccess
  entries += ME_VirtualFieldCollection

  // in exastencils.domain

  entries += ME_Domain
  entries += ME_DomainCollection
  entries += ME_DomainDecl

}
