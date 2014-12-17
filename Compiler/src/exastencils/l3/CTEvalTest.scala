package exastencils.l3

object CTEvalTest extends App {
  
  val env = new Environment;
  env.bind("print", new PrintFun);
  
  new Apply(new Variable("print"), List(new RealNumber(42))) eval env
  new Define("foo", new RealNumber(13)) eval env
  new Apply(new Variable("print"), List(new Variable("foo"))) eval env
  
}