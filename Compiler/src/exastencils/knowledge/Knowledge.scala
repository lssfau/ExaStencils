package exastencils.knowledge

import exastencils.knowledge._

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

  val numGhostLayers : Integer = 1;
  val maxLevel : Integer = 6;
  val numLevels : Integer = maxLevel + 1;
  val fragmentCommStrategy : Integer = 6;
  //val fragmentCommStrategy : Integer = 26;

  val summarizeBlocks : Boolean = true; // TODO: sanity check if compatible with chosen smoother

  val numBlocks_x : Integer = 4;
  val numBlocks_y : Integer = 4;
  val numBlocks_z : Integer = 4;
  val numBlocks : Integer = numBlocks_x * numBlocks_y * numBlocks_z;

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

  val numFragsTotal_x : Integer = numFragsPerBlock_x * numBlocks_x;
  val numFragsTotal_y : Integer = numFragsPerBlock_y * numBlocks_y;
  val numFragsTotal_z : Integer = numFragsPerBlock_z * numBlocks_z;
  val numFragsTotal : Integer = numFragsTotal_x * numFragsTotal_y * numFragsTotal_z;

  val numFragsPerBlock : Integer = numFragsPerBlock_x * numFragsPerBlock_y * numFragsPerBlock_z;
  val numFragsPerBlockPerDim : Array[Integer] = Array(numFragsPerBlock_x, numFragsPerBlock_y, numFragsPerBlock_z);
  val fragLength : Integer = fragLength_x * fragLength_y * fragLength_z;
  val fragLengthPerDim : Array[Integer] = Array(fragLength_x, fragLength_y, fragLength_z);

  val smootherNumPre : Integer = 3;
  val smootherNumPost : Integer = 3;
  val cgsNumSteps : Integer = 512;

  val mgMaxNumIterations : Integer = 1024;

  val cgs = CoarseGridSolverType.IP_Smoother;
  val smoother = SmootherType.Jac;

  val gsodNumIterations : Integer = 8;
  val gsbeNumIterations : Integer = 12;
  val gsbeNumWindowSize : Integer = 4;
  val gsbeNumWindowOverlap : Integer = 1;

  val smootherOmega : Double = (if (SmootherType.Jac == smoother) 0.8 else 1.0);

  val numSolSlots : Integer = (if (SmootherType.Jac == smoother) 2 else 1);
}
