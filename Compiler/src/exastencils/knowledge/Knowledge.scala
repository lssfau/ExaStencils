package exastencils.knowledge

import exastencils.knowledge._
import exastencils.spl.Configuration

object CoarseGridSolverType extends Enumeration {
  type CoarseGridSolverType = Value;
  val IP_Smoother = Value("InPlace_Smoother");
}

object SmootherType extends Enumeration {
  type SmootherType = Value;
  val Jac = Value("Jacobi");
  val GS = Value("GaussSeidel");
  val RBGS = Value("RedBlack_GaussSeidel");

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
  var dimensionality : Int = 3;

  var numGhostLayers : Int = 1;
  var maxLevel : Int = 6;
  var numLevels : Int = maxLevel + 1;
  var fragmentCommStrategy : Int = 6; //26
  var useMPIDatatypes : Boolean = true;
  var useLoopsOverNeighbors : Boolean = true;

  var summarizeBlocks : Boolean = true; // TODO: sanity check if compatible with chosen smoother
  var useOMP : Boolean = true;

  var numBlocks_x : Int = 4;
  var numBlocks_y : Int = 4;
  var numBlocks_z : Int = 4;
  // TODO: ignore values outside the given dimensionality; also applies for other Knowledge values
  var numBlocks : Int = numBlocks_x * numBlocks_y * numBlocks_z;

  var numFragsPerBlock_x : Int = 4;
  var numFragsPerBlock_y : Int = 4;
  var numFragsPerBlock_z : Int = 4;
  var fragLength_x : Int = 1;
  var fragLength_y : Int = 1;
  var fragLength_z : Int = 1;

  if (summarizeBlocks) {
    // FIXME: move to transformation
    fragLength_x = numFragsPerBlock_x;
    fragLength_y = numFragsPerBlock_y;
    fragLength_z = numFragsPerBlock_z;

    numFragsPerBlock_x = 1;
    numFragsPerBlock_y = 1;
    numFragsPerBlock_z = 1;
  }

  var numFragsTotal_x : Int = numFragsPerBlock_x * numBlocks_x;
  var numFragsTotal_y : Int = numFragsPerBlock_y * numBlocks_y;
  var numFragsTotal_z : Int = numFragsPerBlock_z * numBlocks_z;
  var numFragsTotal : Int = numFragsTotal_x * numFragsTotal_y * numFragsTotal_z;

  var numFragsPerBlock : Int = numFragsPerBlock_x * numFragsPerBlock_y * numFragsPerBlock_z;
  var numFragsPerBlockPerDim : Array[Int] = Array(numFragsPerBlock_x, numFragsPerBlock_y, numFragsPerBlock_z);
  var fragLength : Int = fragLength_x * fragLength_y * fragLength_z;
  var fragLengthPerDim : Array[Int] = Array(fragLength_x, fragLength_y, fragLength_z);

  var smootherNumPre : Int = 3;
  var smootherNumPost : Int = 3;
  var cgsNumSteps : Int = 512;

  var mgMaxNumIterations : Int = 1024;

  var cgs = CoarseGridSolverType.IP_Smoother;
  var smoother = SmootherType.GS;

  var gsodNumIterations : Int = 8;
  var gsbeNumIterations : Int = 12;
  var gsbeNumWindowSize : Int = 4;
  var gsbeNumWindowOverlap : Int = 1;

  var smootherOmega : Double = (if (SmootherType.Jac == smoother) 0.8 else 1.0);

  var numSolSlots : Int = (if (SmootherType.Jac == smoother) 2 else 1);

  def update(configuration : Configuration) : Unit = {

    smoother = configuration.getFirstSelectedSubFeatureName("smoother") match {
      case "Jacobi"               => SmootherType.Jac
      case "GaussSeidel"          => SmootherType.GS
      case "RedBlack_GaussSeidel" => SmootherType.RBGS
    }

    cgs = configuration.getFirstSelectedSubFeatureName("cgs") match {
      case "InPlace_Smoother" => CoarseGridSolverType.IP_Smoother
    }

    useOMP = summarizeBlocks || (numFragsPerBlock_x != 1 || numFragsPerBlock_y != 1 || numFragsPerBlock_z != 1)

    numLevels = maxLevel + 1;

    numBlocks = numBlocks_x * numBlocks_y * numBlocks_z;

    if (summarizeBlocks) {
      // FIXME: move to transformation
      fragLength_x = numFragsPerBlock_x;
      fragLength_y = numFragsPerBlock_y;
      fragLength_z = numFragsPerBlock_z;

      numFragsPerBlock_x = 1;
      numFragsPerBlock_y = 1;
      numFragsPerBlock_z = 1;
    }

    numFragsTotal_x = numFragsPerBlock_x * numBlocks_x;
    numFragsTotal_y = numFragsPerBlock_y * numBlocks_y;
    numFragsTotal_z = numFragsPerBlock_z * numBlocks_z;
    numFragsTotal = numFragsTotal_x * numFragsTotal_y * numFragsTotal_z;

    numFragsPerBlock = numFragsPerBlock_x * numFragsPerBlock_y * numFragsPerBlock_z;
    numFragsPerBlockPerDim = Array(numFragsPerBlock_x, numFragsPerBlock_y, numFragsPerBlock_z);
    fragLength = fragLength_x * fragLength_y * fragLength_z;
    fragLengthPerDim = Array(fragLength_x, fragLength_y, fragLength_z);

    smootherOmega = (if (SmootherType.Jac == smoother) 0.8 else 1.0);

    numSolSlots = (if (SmootherType.Jac == smoother) 2 else 1);
  }

}
