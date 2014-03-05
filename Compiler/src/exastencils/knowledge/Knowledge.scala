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
  // FIXME: Integer is required to support implicit conversion to NumericLiteral
	var dimensionality : Integer = 3;
  
  var numGhostLayers : Integer = 1;
  var maxLevel : Integer = 6;
  var numLevels : Integer = maxLevel + 1;
  var fragmentCommStrategy : Integer = 6; //26
  var useMPIDatatypes : Boolean = true;
  var useLoopsOverNeighbors : Boolean = true;

  var summarizeBlocks : Boolean = true; // TODO: sanity check if compatible with chosen smoother
  var useOMP : Boolean = true;

  var numBlocks_x : Integer = 4;
  var numBlocks_y : Integer = 4;
  var numBlocks_z : Integer = 4;
  // TODO: ignore values outside the given dimensionality; also applies for other Knowledge values
  var numBlocks : Integer = numBlocks_x * numBlocks_y * numBlocks_z;

  var numFragsPerBlock_x : Integer = 4;
  var numFragsPerBlock_y : Integer = 4;
  var numFragsPerBlock_z : Integer = 4;
  var fragLength_x : Integer = 1;
  var fragLength_y : Integer = 1;
  var fragLength_z : Integer = 1;

  if (summarizeBlocks) {
    // FIXME: move to transformation
    fragLength_x = numFragsPerBlock_x;
    fragLength_y = numFragsPerBlock_y;
    fragLength_z = numFragsPerBlock_z;

    numFragsPerBlock_x = 1;
    numFragsPerBlock_y = 1;
    numFragsPerBlock_z = 1;
  }

  var numFragsTotal_x : Integer = numFragsPerBlock_x * numBlocks_x;
  var numFragsTotal_y : Integer = numFragsPerBlock_y * numBlocks_y;
  var numFragsTotal_z : Integer = numFragsPerBlock_z * numBlocks_z;
  var numFragsTotal : Integer = numFragsTotal_x * numFragsTotal_y * numFragsTotal_z;

  var numFragsPerBlock : Integer = numFragsPerBlock_x * numFragsPerBlock_y * numFragsPerBlock_z;
  var numFragsPerBlockPerDim : Array[Integer] = Array(numFragsPerBlock_x, numFragsPerBlock_y, numFragsPerBlock_z);
  var fragLength : Integer = fragLength_x * fragLength_y * fragLength_z;
  var fragLengthPerDim : Array[Integer] = Array(fragLength_x, fragLength_y, fragLength_z);

  var smootherNumPre : Integer = 3;
  var smootherNumPost : Integer = 3;
  var cgsNumSteps : Integer = 512;

  var mgMaxNumIterations : Integer = 1024;

  var cgs = CoarseGridSolverType.IP_Smoother;
  var smoother = SmootherType.GS;

  var gsodNumIterations : Integer = 8;
  var gsbeNumIterations : Integer = 12;
  var gsbeNumWindowSize : Integer = 4;
  var gsbeNumWindowOverlap : Integer = 1;

  var smootherOmega : Double = (if (SmootherType.Jac == smoother) 0.8 else 1.0);

  var numSolSlots : Integer = (if (SmootherType.Jac == smoother) 2 else 1);

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
