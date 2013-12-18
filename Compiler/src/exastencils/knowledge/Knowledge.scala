package exastencils.knowledge

object Knowledge {
  val numGhostLayers = 1;
  val maxLevel = 5;
  val numLevels = maxLevel + 1;
  val fragmentCommStrategy = 6;
  //val fragmentCommStrategy = 26;

  val numSolSlots = 1; // TODO: set according to smoother
  
  val numFragsPerBlock_x = 3;
  val numFragsPerBlock_y = 3;
  val numFragsPerBlock_z = 3;
  val numFragsPerBlock = numFragsPerBlock_x * numFragsPerBlock_y * numFragsPerBlock_z;
  val numBlocks_x = 3;
  val numBlocks_y = 3;
  val numBlocks_z = 3;
  val numBlocks = numBlocks_x * numBlocks_y * numBlocks_z;
  val numFragsTotal_x = numFragsPerBlock_x * numBlocks_x;
  val numFragsTotal_y = numFragsPerBlock_y * numBlocks_y;
  val numFragsTotal_z = numFragsPerBlock_z * numBlocks_z;
  val numFragsTotal = numFragsTotal_x * numFragsTotal_y * numFragsTotal_z;
}

object Globals {
  var printPath = s"C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ExaStencils_DSL/Generated/";
}