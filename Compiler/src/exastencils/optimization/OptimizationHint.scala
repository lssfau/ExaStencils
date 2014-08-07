package exastencils.optimization

trait OptimizationHint {
  var isParallel : Boolean = false
  var isInnermost : Boolean = false
}

final object InScope {
  final val ANNOT : String = "InScope"
}
