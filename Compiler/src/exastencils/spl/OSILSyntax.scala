package exastencils.spl

// if ifCase 1 == aspectRatioOffset < 2
// if ifCase 2 == 2 <= aspectRatioOffset < 4
// if ifCase 3 == 4 <= aspectRatioOffset 

class OSILSyntax(featureToID : scala.collection.mutable.Map[String, Tuple2[Feature, Int]], dimensionality : Int, num_points_per_dim : Int, ifCase : Int, derivedDomainParts : scala.collection.mutable.Map[String, Int]) {

  var nodesTimesRanks =
    "<times>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("sisc2015_numNodes")._2 + "\"/>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("sisc2015_ranksPerNode")._2 + "\"/>\n" +
      "</times>\n";

  var dimBase =
    "<power>\n" +
      "<number type=\"real\" value=\"2.0\"/>\n" +
      "<number type=\"real\" value=\"" + dimensionality + "\"/>\n" +
      "</power>\n";

  //  var domain_rect_numBlocks_x = ((problemDefinition("num_points_per_dim").asInstanceOf[Int] / ( config(FeatureModel.get("domain_fragmentLength_x")) * Math.pow(2, config(FeatureModel.get("maxLevel"))))) / config(FeatureModel.get("domain_rect_numFragsPerBlock_x")))
  var domain_rect_numBlocks_x =
    "<divide>\n" +
      "<divide>\n" +
      "<number type=\"real\" value=\"" + num_points_per_dim + "\"/>\n" +
      "<times>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_x")._2 + "\"/>\n" +
      "<power>\n" +
      "<number type=\"real\" value=\"2.0\"/>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("maxLevel")._2 + "\"/>\n" +
      "</power>\n" +
      "</times>\n" +
      "</divide>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_x")._2 + "\"/>\n" +
      "</divide>\n";

  var domain_rect_numBlocks_y =
    "<divide>\n" +
      "<divide>\n" +
      "<number type=\"real\" value=\"" + num_points_per_dim + "\"/>\n" +
      "<times>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_y")._2 + "\"/>\n" +
      "<power>\n" +
      "<number type=\"real\" value=\"2.0\"/>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("maxLevel")._2 + "\"/>\n" +
      "</power>\n" +
      "</times>\n" +
      "</divide>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_y")._2 + "\"/>\n" +
      "</divide>\n";

  var domain_rect_numBlocks_z = if (dimensionality == 3)
    "<divide>\n" +
      "<divide>\n" +
      "<number type=\"real\" value=\"" + num_points_per_dim + "\"/>\n" +
      "<times>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_z")._2 + "\"/>\n" +
      "<power>\n" +
      "<number type=\"real\" value=\"2.0\"/>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("maxLevel")._2 + "\"/>\n" +
      "</power>\n" +
      "</times>\n" +
      "</divide>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_z")._2 + "\"/>\n" +
      "</divide>\n"
  else "";

  var num_frags_per_block_total = if (dimensionality == 2)
    "<times>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_x")._2 + "\"/>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_y")._2 + "\"/>\n" +
      "</times>\n"
  else
    "<times>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_x")._2 + "\"/>\n" +
      "<times>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_y")._2 + "\"/>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_z")._2 + "\"/>\n" +
      "</times>\n" +
      "</times>\n"

  var frag_volume = if (dimensionality == 2)
    "<times>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_x")._2 + "\"/>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_y")._2 + "\"/>\n" +
      "</times>\n"
  else
    "<times>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_x")._2 + "\"/>\n" +
      "<times>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_y")._2 + "\"/>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_z")._2 + "\"/>\n" +
      "</times>\n" +
      "</times>\n"

  var num_blocks_total = if (dimensionality == 2)
    "<times>\n" +
      domain_rect_numBlocks_x +
      domain_rect_numBlocks_y +
      "</times>\n"
  else
    "<times>\n" +
      domain_rect_numBlocks_x +
      "<times>\n" +
      domain_rect_numBlocks_y +
      domain_rect_numBlocks_z +
      "</times>\n" +
      "</times>\n"

  var num_unit_frags_x = "<times>\n" +
    domain_rect_numBlocks_x +
    "<times>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_x")._2 + "\"/>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_x")._2 + "\"/>\n" +
    "</times>\n" +
    "</times>\n"

  var num_unit_frags_y = "<times>\n" +
    domain_rect_numBlocks_y +
    "<times>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_y")._2 + "\"/>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_y")._2 + "\"/>\n" +
    "</times>\n" +
    "</times>\n"

  var num_unit_frags_z = if (dimensionality == 3) "<times>\n" +
    domain_rect_numBlocks_z +
    "<times>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_z")._2 + "\"/>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_z")._2 + "\"/>\n" +
    "</times>\n" +
    "</times>\n"
  else ""

  var mpi_numThreads = num_blocks_total

  var domain_numFragmentsPerBlock = num_frags_per_block_total

  var omp_numThreads = ""

  if (ifCase == 1) {
    omp_numThreads = "<number type=\"real\" value=\"1\"/>\n"
  } else if (ifCase == 2) {
    omp_numThreads = num_frags_per_block_total
  } else if (ifCase == 3) {
    omp_numThreads = frag_volume
  } else {
    throw new Exception("Error in handling of OSILSyntax class ")
  }

  var numNodes = "<divide>\n" +
    "<times>\n" +
    mpi_numThreads +
    omp_numThreads +
    "</times>\n" +
    "<number type=\"real\" value=\"64\"/>\n" +
    "</divide>\n";

  // var mem_per_node = (8.0 * 4.0 * 4.0 / (3.0 * Math.pow(problemDefinition("num_points_per_dim").asInstanceOf[Int], problemDefinition("dimensionality").asInstanceOf[Int]))) / numNodes
  var mem_per_node = "<divide>\n" +
    "<divide>\n" +
    "<number type=\"real\" value=\"128\"/>\n" +
    "<times>\n" +
    "<number type=\"real\" value=\"3\"/>\n" +
    "<power>\n" +
    "<number type=\"real\" value=\"" + num_points_per_dim + "\"/>\n" +
    "<number type=\"real\" value=\"" + dimensionality + "\"/>\n" +
    "</power>\n" +
    "</times>\n" +
    "</divide>\n" +
    numNodes +
    "</divide>\n"

  var domain_x = "<times>\n" +
    domain_rect_numBlocks_x +
    "<times>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_x")._2 + "\"/>\n" +
    "<times>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_x")._2 + "\"/>\n" +
    "<power>\n" +
    "<number type=\"real\" value=\"2\"/>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("minLevel")._2 + "\"/>\n" +
    "</power>\n" +
    "</times>\n" +
    "</times>\n" +
    "</times>\n"

  var domain_y = "<times>\n" +
    domain_rect_numBlocks_y +
    "<times>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_y")._2 + "\"/>\n" +
    "<times>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_y")._2 + "\"/>\n" +
    "<power>\n" +
    "<number type=\"real\" value=\"2\"/>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("minLevel")._2 + "\"/>\n" +
    "</power>\n" +
    "</times>\n" +
    "</times>\n" +
    "</times>\n"

  var domain_z = if (dimensionality == 3) "<times>\n" +
    domain_rect_numBlocks_z +
    "<times>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_z")._2 + "\"/>\n" +
    "<times>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_z")._2 + "\"/>\n" +
    "<power>\n" +
    "<number type=\"real\" value=\"2\"/>\n" +
    "<variable coef=\"1.0\" idx=\"" + featureToID("minLevel")._2 + "\"/>\n" +
    "</power>\n" +
    "</times>\n" +
    "</times>\n" +
    "</times>\n"
  else ""

  var memory = "<times>\n" +
    "<number type=\"real\" value=\"1024\"/>\n" +
    "<times>\n" +
    "<number type=\"real\" value=\"12\"/>\n" +
    "<times>\n" +
    "<number type=\"real\" value=\"1024\"/>\n" +
    "<number type=\"real\" value=\"1024\"/>\n" +
    "</times>\n" +
    "</times>\n" +
    "</times>\n"

  def getConstraints() : scala.collection.mutable.MutableList[Tuple2[String, String]] = {

    var aspectRatioOffset = 0

    if (ifCase == 1) {
      aspectRatioOffset = 2
    } else if (ifCase == 2) {
      aspectRatioOffset = 4
    }

    var constraints : scala.collection.mutable.MutableList[Tuple2[String, String]] = scala.collection.mutable.MutableList()

    var value = ""
    var constraintTree = ""

    value = "<con lb=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<minus>" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_y")._2 + "\"/>\n" +
      "<number type=\"real\" value=\"1\"/>\n" +
      "</minus>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    //// old........

    value = "<con lb=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<minus>" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_x")._2 + "\"/>\n" +
      "<number type=\"real\" value=\"1\"/>\n" +
      "</minus>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    value = "<con lb=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<minus>" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_fragmentLength_y")._2 + "\"/>\n" +
      "<number type=\"real\" value=\"1\"/>\n" +
      "</minus>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    value = "<con lb=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<minus>" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("poly_tileSize_x")._2 + "\"/>\n" +
      "<number type=\"real\" value=\"112\"/>\n" +
      "</minus>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    value = "<con lb=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<minus>" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_x")._2 + "\"/>\n" +
      "<number type=\"real\" value=\"1\"/>\n" +
      "</minus>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    value = "<con lb=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<minus>" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_y")._2 + "\"/>\n" +
      "<number type=\"real\" value=\"1\"/>\n" +
      "</minus>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    value = "<con lb=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<minus>" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("maxLevel")._2 + "\"/>\n" +
      "<number type=\"real\" value=\"4\"/>\n" +
      "</minus>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))
    // lower bound definition end ----------------

    if (ifCase == 1) {
      value = "<con lb=\"0.0\" ub=\"0.0\"/>\n"
      constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
        "<minus>" +
        "<number type=\"real\" value=\"1\"/>\n" +
        num_frags_per_block_total +
        "</minus>" +
        "</nl>\n"
      constraints += (new Tuple2(value, constraintTree))

      value = "<con lb=\"0.0\" ub=\"0.0\"/>\n"
      constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
        "<minus>" +
        "<number type=\"real\" value=\"1\"/>\n" +
        frag_volume +
        "</minus>" +
        "</nl>\n"
      constraints += (new Tuple2(value, constraintTree))

    }

    if (ifCase == 2) {
      value = "<con lb=\"1.0\"/>\n"
      constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
        "<minus>" +
        num_frags_per_block_total +
        frag_volume +
        "</minus>" +
        "</nl>\n"
      constraints += (new Tuple2(value, constraintTree))

    }

    //    if (!(getNumNodes(config) > 8)) return false
    value = "<con lb=\"8.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      numNodes +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    // if (!(mem_per_node <= memory)) return false
    // to memory - mem_per_node <= 0
    value = "<con lb=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<minus>" +
      memory +
      mem_per_node +
      "</minus>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    // if (!(num_unit_frags_x == num_unit_frags_y) || !(num_unit_frags_y == num_unit_frags_z)) return false
    if (dimensionality == 3) {
      value = "<con lb=\"0.0\" ub=\"0.0\"/>\n"
      constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
        "<minus>" +
        num_unit_frags_y +
        num_unit_frags_z +
        "</minus>" +
        "</nl>\n"
      constraints += (new Tuple2(value, constraintTree))
    }

    //     if (!(frag_volume <= 64.0 && config.partialBaseConfig.apply("domain_numFragmentsPerBlock").asInstanceOf[Int] <= 64)) return false
    value = "<con ub=\"64.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      frag_volume +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    value = "<con ub=\"64.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      domain_numFragmentsPerBlock +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    //if (domain_x > 64.0) return false;
    value = "<con ub=\"64.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      domain_x +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    //if (domain_y > 64.0) return false;
    value = "<con ub=\"64.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      domain_y +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    if (dimensionality == 3) {
      //if (domain_z > 64.0) return false;
      value = "<con ub=\"64.0\"/>\n"
      constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
        domain_z +
        "</nl>\n"
      constraints += (new Tuple2(value, constraintTree))
    }

    //    //if (!(frag_volume == 1.0 || domain_numFragmentsPerBlock == 1)) return false
    //    // transformed to (frag_volume = a,  domain_numFragmentsPerBlock = b) -> (a-(a*b)) * (b-(a*b)) == 0 && a +b => 1
    value = "<con ub=\"0.0\" lb=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<times>" +
      "<minus>" +
      frag_volume +
      "<times>" +
      frag_volume +
      domain_numFragmentsPerBlock +
      "</times>" +
      "</minus>" +
      "<minus>" +
      domain_numFragmentsPerBlock +
      "<times>" +
      frag_volume +
      domain_numFragmentsPerBlock +
      "</times>" +
      "</minus>" +
      "</times>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    value = "<con lb=\"1.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<times>" +
      frag_volume +
      domain_numFragmentsPerBlock +
      "</times>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    //if (config(FeatureModel.get("minLevel")).toInt >= config(FeatureModel.get("maxLevel")).toInt) return false
    value = "<con lb=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<minus>" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("maxLevel")._2 + "\"/>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("minLevel")._2 + "\"/>\n" +
      "</minus>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    value = "<con lb=\"0.0\" ub=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<minus>" +
      num_unit_frags_y +
      num_unit_frags_x +
      "</minus>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    if (dimensionality == 3) {
      value = "<con lb=\"64.0\" ub=\"64.0\"/>\n"
      constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
        numNodes +
        "</nl>\n"
      constraints += (new Tuple2(value, constraintTree))
    }

    value = "<con lb=\"0.0\" ub=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<minus>" +
      "<power>\n" +
      "<number type=\"real\" value=\"2\"/>\n" +
      "<variable coef=\"1.0\" idx=\"" + derivedDomainParts("help_domain_rect_numFragsPerBlock_x") + "\"/>\n" +
      "</power>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_x")._2 + "\"/>\n" +
      "</minus>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    value = "<con lb=\"0.0\" ub=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<minus>" +
      "<power>\n" +
      "<number type=\"real\" value=\"2\"/>\n" +
      "<variable coef=\"1.0\" idx=\"" + derivedDomainParts("help_domain_rect_numFragsPerBlock_y") + "\"/>\n" +
      "</power>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_y")._2 + "\"/>\n" +
      "</minus>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    if (dimensionality == 3) {
      value = "<con lb=\"0.0\" ub=\"0.0\"/>\n"
      constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
        "<minus>" +
        "<power>\n" +
        "<number type=\"real\" value=\"2\"/>\n" +
        "<variable coef=\"1.0\" idx=\"" + derivedDomainParts("help_domain_rect_numFragsPerBlock_z") + "\"/>\n" +
        "</power>\n" +
        "<variable coef=\"1.0\" idx=\"" + featureToID("domain_rect_numFragsPerBlock_z")._2 + "\"/>\n" +
        "</minus>" +
        "</nl>\n"
      constraints += (new Tuple2(value, constraintTree))
    }

    value = "<con lb=\"0.0\" ub=\"1.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<minus>" +
      domain_rect_numBlocks_x +
      "<power>\n" +
      "<number type=\"real\" value=\"2\"/>\n" +
      "<variable coef=\"1.0\" idx=\"" + derivedDomainParts("help_domain_rect_numBlocks_x") + "\"/>\n" +
      "</power>\n" +
      "</minus>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    value = "<con lb=\"0.0\" ub=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<minus>" +
      "<power>\n" +
      "<number type=\"real\" value=\"2\"/>\n" +
      "<variable coef=\"1.0\" idx=\"" + derivedDomainParts("help_domain_rect_numBlocks_y") + "\"/>\n" +
      "</power>\n" +
      domain_rect_numBlocks_y +
      "</minus>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    if (dimensionality == 3) {
      value = "<con lb=\"0.0\" ub=\"0.0\"/>\n"
      constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
        "<minus>" +
        "<power>\n" +
        "<number type=\"real\" value=\"2\"/>\n" +
        "<variable coef=\"1.0\" idx=\"" + derivedDomainParts("help_domain_rect_numBlocks_z") + "\"/>\n" +
        "</power>\n" +
        domain_rect_numBlocks_z +
        "</minus>" +
        "</nl>\n"
      constraints += (new Tuple2(value, constraintTree))
    }

    //    if (!config.xorFeatureValues(FeatureModel.allFeatures("poly_optLevel_fine")).equals("3") && config.getNumericFeatureValue("poly_tileSize_x").toInt != FeatureModel.get("poly_tileSize_x").minValue) return false
    value = "<con lb=\"0.0\" ub=\"0.0\"/>\n"
    constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
      "<times>" +
      "<minus>" +
      "<number type=\"real\" value=\"128\"/>\n" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("poly_tileSize_x")._2 + "\"/>\n" +
      "</minus>" +
      "<minus>" +
      "<variable coef=\"1.0\" idx=\"" + featureToID("poly_optLevel_fine")._2 + "\"/>\n" +
      "<number type=\"real\" value=\"3\"/>\n" +
      "</minus>" +
      "</times>" +
      "</nl>\n"
    constraints += (new Tuple2(value, constraintTree))

    if (dimensionality == 3) {
      value = "<con lb=\"0.0\" ub=\"0.0\"/>\n"
      constraintTree = "<nl idx=\"" + constraints.size + "\">\n" +
        "<times>" +
        "<minus>" +
        "<number type=\"real\" value=\"128\"/>\n" +
        "<variable coef=\"1.0\" idx=\"" + featureToID("poly_tileSize_y")._2 + "\"/>\n" +
        "</minus>" +
        "<minus>" +
        "<variable coef=\"1.0\" idx=\"" + featureToID("poly_optLevel_fine")._2 + "\"/>\n" +
        "<number type=\"real\" value=\"3\"/>\n" +
        "</minus>" +
        "</times>" +
        "</nl>\n"
      constraints += (new Tuple2(value, constraintTree))
    }

    //    var poly_numFinestLevels = 2
    //
    //    // nur wenn poly_optLevel_fine == 3
    //    if (config.xorFeatureValues(FeatureModel.allFeatures("poly_optLevel_fine")).equals("3") && (Math.pow(2, config.getNumericFeatureValue("maxLevel") - poly_numFinestLevels + 1) * config.getNumericFeatureValue("domain_fragmentLength_x")) + 1 < 2 * config.getNumericFeatureValue("poly_tileSize_x"))
    //      return false;
    //
    //    if (dimToConsider == 3) {
    //      if (config.xorFeatureValues(FeatureModel.allFeatures("poly_optLevel_fine")).equals("3") && Math.pow(2, config.getNumericFeatureValue("maxLevel") - poly_numFinestLevels + 1) * config.getNumericFeatureValue("domain_fragmentLength_y") + 1 < 2 * config.getNumericFeatureValue("poly_tileSize_y"))
    //        return false;
    //    }
    //    

    return constraints
  }

}