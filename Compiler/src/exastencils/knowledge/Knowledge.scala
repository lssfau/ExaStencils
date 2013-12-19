package exastencils.knowledge

object Knowledge {
  // FIXME: Integer is required to support implict conversion to NumericLiteral

  val numGhostLayers : Integer = 1;
  val maxLevel : Integer = 5;
  val numLevels : Integer = maxLevel + 1;
  val fragmentCommStrategy : Integer = 6;
  //val fragmentCommStrategy : Integer = 26;

  val numSolSlots : Integer = 1; // TODO: set according to smoother

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
}

object Globals {
  var printPath = s"C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ExaStencils_DSL/Generated/";
}