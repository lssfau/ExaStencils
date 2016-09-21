package exastencils.base.l4

import exastencils.prettyprinting._

/// L4_LevelSpecification

trait L4_LevelSpecification extends L4_Node with PrettyPrintable {
  def resolveLevel : Int
}

/// L4_DeclarationLevelSpecification

// can be used for declarations, e.g., functions
trait L4_DeclarationLevelSpecification extends L4_LevelSpecification

/// L4_AccessLevelSpecification

// can be used for accesses, e.g., in fields or function calls
trait L4_AccessLevelSpecification extends L4_LevelSpecification
