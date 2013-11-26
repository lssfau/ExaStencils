package exastencils.datastructures

abstract class Strategy extends (Node => Option[Node]) {

  //  p =>

  //  val body : Any => Option[Any]
  //  
  //  def mkStrategy (f : Any => Option[Any]) : Strategy =
  //        new Strategy {
  //            val body = f
  //        }
  //  
  //  def apply (r : Any) : Option[Any] = {
  //		//val i = start ("event" -> "StratEval", "strategy" -> this,
  //        //               "subject" -> r)
  //        val result = body (r)
  //        //finish (i, "result" -> result)
  //        result
  //  }
  //  
  //  def <* (q : => Strategy) : Strategy =
  //        mkStrategy (t1 =>
  //                p (t1) match {
  //                    case Some (t2) => q (t2)
  //                    case None      => None
  //                }
  //        )
  //        

  //        
  //        
  //   def ruleMacro (c : Context) (f : c.Expr[Any ==> Any]) : c.Expr[Strategy] =
  //        makeCallWithName (c)
  //        
  //   def rule (f : Any ==> Any) : Strategy =
  //        macro RewriterCoreMacros.ruleMacro

}