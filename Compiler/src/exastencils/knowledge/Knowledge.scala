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
  // FIXME: Integer is required to support implict conversion to NumericLiteral

  val numGhostLayers : Integer = 1;
  val maxLevel : Integer = 5;
  val numLevels : Integer = maxLevel + 1;
  val fragmentCommStrategy : Integer = 6;
  //val fragmentCommStrategy : Integer = 26;

  val numFragsPerBlock_x : Integer = 3;
  val numFragsPerBlock_y : Integer = 3;
  val numFragsPerBlock_z : Integer = 3;
  val numFragsPerBlock : Integer = numFragsPerBlock_x * numFragsPerBlock_y * numFragsPerBlock_z;
  val numBlocks_x : Integer = 3;
  val numBlocks_y : Integer = 3;
  val numBlocks_z : Integer = 3;
  val numBlocks : Integer = numBlocks_x * numBlocks_y * numBlocks_z;
  val numFragsTotal_x : Integer = numFragsPerBlock_x * numBlocks_x;
  val numFragsTotal_y : Integer = numFragsPerBlock_y * numBlocks_y;
  val numFragsTotal_z : Integer = numFragsPerBlock_z * numBlocks_z;
  val numFragsTotal : Integer = numFragsTotal_x * numFragsTotal_y * numFragsTotal_z;

  val smootherOmega : Double = 1.0;
  val smootherNumPre : Integer = 3;
  val smootherNumPost : Integer = 3;
  val cgsNumSteps : Integer = 64;

  val mgMaxNumIterations : Integer = 1024;

  val cgs = CoarseGridSolverType.IP_Smoother;
  val smoother = SmootherType.GS;

  val gsodNumIterations : Integer = 8;
  val gsbeNumIterations : Integer = 12;
  val gsbeNumWindowSize : Integer = 4;
  val gsbeNumWindowOverlap : Integer = 1;

  val numSolSlots : Integer = (if (SmootherType.Jac == smoother) 2 else 1);
}
