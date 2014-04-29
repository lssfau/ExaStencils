package exastencils.knowledge

import exastencils.knowledge._
import exastencils.spl.Configuration
import harald_dep.dsl.DomainKnowledge
import exastencils.util._

object CoarseGridSolverType extends Enumeration {
  type CoarseGridSolverType = Value
  val IP_Smoother = Value("InPlace_Smoother")
}

object SmootherType extends Enumeration {
  type SmootherType = Value
  val Jac = Value("Jacobi")
  val GS = Value("GaussSeidel")
  val RBGS = Value("RedBlack_GaussSeidel")

  // TODO:
  // GS		= Gauss-Seidel
  // GSAC	= Gauss-Seidel with Additional Communication
  // GSOD	= Gauss-Seidel of Death (with additional communication)
  // GSBEAC	= Gauss-Seidel Block Edition with Additional Communication
  // GSRS	= Gauss-Seidel with Random Sampling
  // GSRB	= Gauss-Seidel Red-Black (RBGS)
  // GSRBAC	= Gauss-Seidel Red-Black (RBGS) with Additional Communication
}

object Knowledge {
  // === Level 1 ===  
  var dimensionality : Int = 3 // dimensionality of the problem

  // TODO: check if these parameters will be necessary or can be implicitly assumed once an appropriate field collection is in place 
  var maxLevel : Int = 6 // the finest level
  var numLevels : Int = maxLevel + 1 // the number of levels -> this assumes that the cycle descents to the coarsest level

  // --- Domain Decomposition ---

  // describes to (number of) domains to be used
  var domain_size : AABB = new AABB
  var domain_numSubdomains : Int = 1
  var domain_subdomains : Array[AABB] = Array(new AABB)

  // specifies if fragments within one block should be aggregated 
  // TODO: sanity check if compatible with chosen smoother
  var domain_summarizeBlocks : Boolean = true

  // number of blocks per dimension - one block will usually be mapped to one MPI thread
  var domain_numBlocks_x : Int = 3
  var domain_numBlocks_y : Int = 3
  var domain_numBlocks_z : Int = 3
  def domain_numBlocks : Int = {
    domain_numBlocks_x *
      (if (dimensionality > 1) domain_numBlocks_y else 1) *
      (if (dimensionality > 2) domain_numBlocks_z else 1)
  }

  // the total number of fragments per dimension
  var domain_numFragsTotal_x : Int = domain_numFragsPerBlock_x * domain_numBlocks_x
  var domain_numFragsTotal_y : Int = domain_numFragsPerBlock_y * domain_numBlocks_y
  var domain_numFragsTotal_z : Int = domain_numFragsPerBlock_z * domain_numBlocks_z
  def domain_numFragsTotal : Int = {
    domain_numFragsTotal_x *
      (if (dimensionality > 1) domain_numFragsTotal_y else 1) *
      (if (dimensionality > 2) domain_numFragsTotal_z else 1)
  }

  // number of fragments in each block per dimension - this will usually be one or represent the number of OMP threads per dimension
  var domain_numFragsPerBlock_x : Int = 3
  var domain_numFragsPerBlock_y : Int = 3
  var domain_numFragsPerBlock_z : Int = 3
  def domain_numFragsPerBlock : Int = {
    domain_numFragsPerBlock_x *
      (if (dimensionality > 1) domain_numFragsPerBlock_y else 1) *
      (if (dimensionality > 2) domain_numFragsPerBlock_z else 1)
  }
  def domain_numFragsPerBlockPerDim(index : Int) : Int = {
    Array(domain_numFragsPerBlock_x, domain_numFragsPerBlock_y, domain_numFragsPerBlock_z)(index)
  }

  // the length of each fragment per dimension - this will either be one or specify the length in unit-fragments, i.e. the number of aggregated fragments per dimension
  var domain_fragLength_x : Int = 1
  var domain_fragLength_y : Int = 1
  var domain_fragLength_z : Int = 1
  def domain_fragLength : Int = {
    domain_fragLength_x *
      (if (dimensionality > 1) domain_fragLength_y else 1) *
      (if (dimensionality > 2) domain_fragLength_z else 1)
  }
  def domain_fragLengthPerDim(index : Int) : Int = {
    Array(domain_fragLength_x, domain_fragLength_y, domain_fragLength_z)(index)
  }

  // the width of each fragment per dimension in absolute coordinates
  var domain_fragWidth_x : Double = 1
  var domain_fragWidth_y : Double = 1
  var domain_fragWidth_z : Double = 1
  def domain_fragWidthPerDim(index : Int) : Double = {
    Array(domain_fragWidth_x, domain_fragWidth_y, domain_fragWidth_z)(index)
  }

  // === Level 2 ===

  // === Level 3 ===

  // --- MG ---
  var mg_maxNumIterations : Int = 1024

  // --- Smoother ---
  var mg_smoother : SmootherType.SmootherType = SmootherType.GS
  var mg_smoother_numPre : Int = 3
  var mg_smoother_numPost : Int = 3
  var mg_smoother_omega : Double = (if (SmootherType.Jac == mg_smoother) 0.8 else 1.0)

  // --- CGS ---
  var mg_cgs : CoarseGridSolverType.CoarseGridSolverType = CoarseGridSolverType.IP_Smoother
  var mg_cgs_numSteps : Int = 512

  // === Level 4 ===

  // === Post Level 4 ===

  // --- Data Structures ---
  // specifies the number of ghost layers at each boundary per field
  var data_numGhostLayers : Int = 1
  // TODO: this will probably become obsolete with an appropriate field collection and/or the new level 4
  var data_numSolSlots : Int = (if (SmootherType.Jac == mg_smoother) 2 else 1)

  // --- OpenMP/Hybrid Parallelization ---
  var useOMP : Boolean = true
  var useMPI : Boolean = true

  // --- Communication ---
  var comm_strategyFragment : Int = 6 //26
  var comm_useMPIDatatypes : Boolean = false
  var comm_useLoopsOverNeighbors : Boolean = true

  // === Obsolete ===
  //  var gsodNumIterations : Int = 8
  //  var gsbeNumIterations : Int = 12
  //  var gsbeNumWindowSize : Int = 4
  //  var gsbeNumWindowOverlap : Int = 1

  def update(configuration : Configuration) : Unit = {
    // NOTE: it is required to call update at least once

    // FIXME: disabled until Jac is supported
    if (false) {
      mg_smoother = configuration.getFirstSelectedSubFeatureName("smoother") match {
        case "Jacobi"               => SmootherType.Jac
        case "GaussSeidel"          => SmootherType.GS
        case "RedBlack_GaussSeidel" => SmootherType.RBGS
      }
    }

    mg_cgs = configuration.getFirstSelectedSubFeatureName("cgs") match {
      case "InPlace_Smoother" => CoarseGridSolverType.IP_Smoother
    }

    useOMP = domain_summarizeBlocks || domain_numFragsPerBlock != 1
    useMPI = (domain_numBlocks != 1)

    numLevels = maxLevel + 1

    if (domain_summarizeBlocks) {
      // FIXME: move to transformation
      domain_fragLength_x = domain_numFragsPerBlock_x
      domain_fragLength_y = domain_numFragsPerBlock_y
      domain_fragLength_z = domain_numFragsPerBlock_z

      domain_numFragsPerBlock_x = 1
      domain_numFragsPerBlock_y = 1
      domain_numFragsPerBlock_z = 1
    }

    domain_numFragsTotal_x = domain_numFragsPerBlock_x * domain_numBlocks_x
    domain_numFragsTotal_y = domain_numFragsPerBlock_y * domain_numBlocks_y
    domain_numFragsTotal_z = domain_numFragsPerBlock_z * domain_numBlocks_z

    domain_fragWidth_x = domain_size.width(0) / domain_numFragsTotal_x
    domain_fragWidth_y = domain_size.width(1) / domain_numFragsTotal_y
    domain_fragWidth_z = domain_size.width(2) / domain_numFragsTotal_z

    mg_smoother_omega = (if (SmootherType.Jac == mg_smoother) 0.8 else 1.0)

    data_numSolSlots = (if (SmootherType.Jac == mg_smoother) 2 else 1)
  }
}

object DimArray {
  def apply() : Array[Int] = { (0 until Knowledge.dimensionality).toArray }
}
