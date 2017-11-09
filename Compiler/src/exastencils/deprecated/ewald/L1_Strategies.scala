package exastencils.deprecated.ewald

//object L1_UnifyOperators extends DefaultStrategy("Unification and simplification of mathematical operators") {
//  def sanitize(s : String) = {
//    s.replaceAll("\\", "__backslash__")
//  }
//
//  this += new Transformation("Search and destroy", {
//    case L1_BinaryExpression("*", L1_Access("dx"), L1_Access("dx"))                             => L1_Access("dxx")
//    case L1_BinaryExpression("*", L1_Access("dy"), L1_Access("dy"))                             => L1_Access("dyy")
//    case L1_BinaryExpression("*", L1_Access("dz"), L1_Access("dz"))                             => L1_Access("dzz")
//    case L1_BinaryExpression("*", L1_Access("dt"), L1_Access("dt"))                             => L1_Access("dtt")
//    case L1_Access("dx²")                                                                       => L1_Access("dxx")
//    case L1_Access("dy²")                                                                       => L1_Access("dyy")
//    case L1_Access("dz²")                                                                       => L1_Access("dzz")
//    case L1_Access("dt²")                                                                       => L1_Access("dtt")
//    case L1_BinaryExpression("^", L1_Access("dx"), L1_IntegerConstant(2))                       => L1_Access("dxx")
//    case L1_BinaryExpression("^", L1_Access("dy"), L1_IntegerConstant(2) | L1_FloatConstant(2)) => L1_Access("dyy")
//    case L1_BinaryExpression("^", L1_Access("dz"), L1_IntegerConstant(2) | L1_FloatConstant(2)) => L1_Access("dzz")
//    case L1_BinaryExpression("^", L1_Access("dt"), L1_IntegerConstant(2) | L1_FloatConstant(2)) => L1_Access("dtt")
//  })
//}
