package meta

import scala.collection.mutable.ListBuffer

import exastencils.base.meta._

/// GeneratorList

object GeneratorList {
  val entries = ListBuffer[Generatable]()

  // in exastencils.base

  entries += ME_BinaryOp
  entries += ME_Constant
  entries += ME_Expression
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

}
