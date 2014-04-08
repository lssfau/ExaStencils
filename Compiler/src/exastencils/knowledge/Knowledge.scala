package exastencils.knowledge

import exastencils.knowledge._
import exastencils.spl.Configuration
import harald.dsl.DomainKnowledge

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

  class DomainDescription {
    // number of blocks per dimension - one block will usually be mapped to one MPI thread
    var numBlocks_x : Int = 3
    var numBlocks_y : Int = 3
    var numBlocks_z : Int = 3
    def numBlocks : Int = {
      numBlocks_x *
        (if (Knowledge.dimensionality > 1) numBlocks_y else 1) *
        (if (Knowledge.dimensionality > 2) numBlocks_z else 1)
    }

    // the total number of fragments per dimension
    var numFragsTotal_x : Int = DomainDescription.numFragsPerBlock_x * numBlocks_x
    var numFragsTotal_y : Int = DomainDescription.numFragsPerBlock_y * numBlocks_y
    var numFragsTotal_z : Int = DomainDescription.numFragsPerBlock_z * numBlocks_z
    def numFragsTotal : Int = {
      numFragsTotal_x *
        (if (Knowledge.dimensionality > 1) numFragsTotal_y else 1) *
        (if (Knowledge.dimensionality > 2) numFragsTotal_z else 1)
    }
  }

  object DomainDescription {
    // number of fragments in each block per dimension - this will usually be one or represent the number of OMP threads per dimension
    var numFragsPerBlock_x : Int = 3
    var numFragsPerBlock_y : Int = 3
    var numFragsPerBlock_z : Int = 3
    def numFragsPerBlock : Int = {
      numFragsPerBlock_x *
        (if (Knowledge.dimensionality > 1) numFragsPerBlock_y else 1) *
        (if (Knowledge.dimensionality > 2) numFragsPerBlock_z else 1)
    }
    def numFragsPerBlockPerDim(index : Int) : Int = {
      Array(numFragsPerBlock_x, numFragsPerBlock_y, numFragsPerBlock_z)(index)
    }

    // the length of each fragment per dimension - this will either be one or specify the length in unit-fragments, i.e. the number of aggregated fragments per dimension
    var fragLength_x : Int = 1
    var fragLength_y : Int = 1
    var fragLength_z : Int = 1
    def fragLength : Int = {
      fragLength_x *
        (if (Knowledge.dimensionality > 1) fragLength_y else 1) *
        (if (Knowledge.dimensionality > 2) fragLength_z else 1)
    }
    def fragLengthPerDim(index : Int) : Int = {
      Array(fragLength_x, fragLength_y, fragLength_z)(index)
    }
  }

  // describes to (number of) domains to be used
  var domain_numDomains : Int = 1
  var domain_descriptions : Array[DomainDescription] = Array(new DomainDescription)

  // specifies if fragments within one block should be aggregated 
  // TODO: sanity check if compatible with chosen smoother
  var domain_summarizeBlocks : Boolean = true

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

    useOMP = domain_summarizeBlocks || DomainDescription.numFragsPerBlock_x != 1 || DomainDescription.numFragsPerBlock_y != 1 || DomainDescription.numFragsPerBlock_z != 1

    numLevels = maxLevel + 1

    if (domain_summarizeBlocks) {
      // FIXME: move to transformation
      DomainDescription.fragLength_x = DomainDescription.numFragsPerBlock_x
      DomainDescription.fragLength_y = DomainDescription.numFragsPerBlock_y
      DomainDescription.fragLength_z = DomainDescription.numFragsPerBlock_z

      DomainDescription.numFragsPerBlock_x = 1
      DomainDescription.numFragsPerBlock_y = 1
      DomainDescription.numFragsPerBlock_z = 1
    }

    for (domain <- domain_descriptions) {
      domain.numFragsTotal_x = DomainDescription.numFragsPerBlock_x * domain.numBlocks_x
      domain.numFragsTotal_y = DomainDescription.numFragsPerBlock_y * domain.numBlocks_y
      domain.numFragsTotal_z = DomainDescription.numFragsPerBlock_z * domain.numBlocks_z
    }

    mg_smoother_omega = (if (SmootherType.Jac == mg_smoother) 0.8 else 1.0)

    data_numSolSlots = (if (SmootherType.Jac == mg_smoother) 2 else 1)
  }
}

object DimArray {
  def apply() : Array[Int] = { (0 until Knowledge.dimensionality).toArray }
}
