package exastencils.spl

/** 
  * A mathematical expression needed for the ForwardFeatureSelection algorithm. One expression describes interactions and influences of features on an non-functional property.
  * In a expression two features interact with each other if they are combined with a * and interactions are combined with a +. 
  *
  * One expression can simply be validated for a given configuration using a replacement of the feature names in the expression with the feature values in the configuration. 
  * Here a binary feature is replaced with a 1 if the feature is selected and 0 otherwise.
  * 
  * @constructor creates a new Expression for a given feature domain.
  * @param featuresOfDomain: all features of the domain with a mapping between the name of the feature and the feature object
  * @param expression: a string describing the expression 
  * 
  */
class FFS_Expression (featuresOfDomain : scala.collection.mutable.Map[String,Feature], expression : String) {


  var noise = 0.0
  
  var wellFormedExpression : String = ""
    
  var participatingBoolFeatures : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set() 
  var participatingNumFeatures : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set() 
  var numberOfParticipatingFeatures = 0
  var expressionArray : Array[String]  = null
  
  parseExpressionToPolnishNotation(expression);

/**
 * The expression is transformed a format with exactly one whitespace before and after each special character (+,*,[,],....).
 * 
 * @param expression: the expression that have to transformed
 * @return the well formed expression
 * 
 **/
  def createWellFormedExpression(expression : String ): String  = {
    var curr : String = expression
    while (curr.contains(" ")){
      curr = curr.replace(" ", "")
    }
    
    curr = curr.replace("\n", " ");
    curr = curr.replace("\t", " ");

    curr = curr.replace("+", " + ");
    curr = curr.replace("*", " * ");

    curr = curr.replace("(", " ( ");
    curr = curr.replace(")", " ) ");

    curr = curr.replace("[", " [ ");
    curr = curr.replace("]", " ] ");

    while(curr.contains("  "))
    {
      curr = curr.replace("  ", " ");
    }
    return curr;
        
  }
  
 /**
  * The method validates whether the given token is an operator. We only consider + and *.
  * 
  * @param the token being considered
  * @return true if the token is + or *, false instead. 
  * 
  **/
  def isOperator(token : String) : Boolean = {
    var x = token.trim()
    
    if(x.equals("+"))
      return true
      
    if(x.equals("*"))
      return true
    
    return false;
  }

  
  /**
   * The method validates whether the given token is an operator that can be used for validation. We consider +, * and log closing brackets.
   * 
   * @param the token being considered
   * @return true if the token is +, *, or log closing bracket "]" false instead. 
   * 
   **/
  def isOperatorEval(token : String) : Boolean = {
    var x = token.trim()
    
    if(x.equals("+"))
      return true
      
    if(x.equals("*"))
      return true
    
    if(x.equals("]"))
      return true
      
    return false;
  }
  
  /**
   * 
   * In our evaluation the precedence of operations have to be considered.
   * 
   * @param thisToken: the token considered
   * @param otherToken: the token used as a reference
   * 
   * @param whether the token considered has a greater precedence as the otherToken  
   * 
   */
  def operatorHasGreaterPrecedence(thisToken : String, otherToken : String) : Boolean = {
    var thisT = thisToken.trim()
    var otherT = otherToken.trim()
    
    if(thisT.equals("*") && otherT.equals("+"))
      return true
    
    
    return false;
  }
  
  /**
   * 
   * In this method, we transform the expression in polish notation. 
   * In that transformation, we store the parts of the expression in the expressionArray global field. 
   * 
   * @param expression: the expression being transformed
   * 
   */
  def parseExpressionToPolnishNotation(expression : String) = {
	  var queue : scala. collection.mutable.Queue[String] = scala.collection.mutable.Queue()
	  var stack : scala. collection.mutable.Stack[String] = scala.collection.mutable.Stack()
    
	  wellFormedExpression  = createWellFormedExpression(expression)
	  var wellFormedTrim = wellFormedExpression.trim()
	  var expr : Array[String]  = wellFormedTrim. split(" ")
  
	  expr.foreach(token => {
	    if(tokenIsAFeatureOrNumber(token)){
	      queue.enqueue(token)
	    }
	    else if(isOperator(token)) {
	      while(stack.size > 0 && isOperator(stack.top ) && operatorHasGreaterPrecedence(stack.top, token)){
	        queue.enqueue(stack.pop)
	      }
	      stack.push(token)
	    }
	    else if(token.equals("(")){
          stack.push(token)
        }
        else if (token.equals("[")){
          stack.push(token)
	    }
	    else if (token.equals(")")) {
          while (!stack.top.equals("("))
          {
            queue.enqueue(stack.pop);
          }
          stack.pop;
        }
	    else if (token.equals("]")) {
          while (!stack.top.equals("["))
          {
            queue.enqueue(stack.pop);
          }
          queue.enqueue("]");
          stack.pop;
        }
	  })
	  
	  while(stack.size > 0) {
	    queue.enqueue(stack.pop)
	  }
	  expressionArray  = queue.toArray
  
  }
  
  /**
   * 
   * The method return a value for a given token and a configuration. 
   * 
   * @param config: the configuration defining the values of the features
   * @param token: element of the expression that needs a value
   * 
   * @return the value of the token in the configuration
   */
  def getValueOfToken(config: Configuration, token : String) : Double = {
    var curr = token.trim
    if(curr.forall(_.isDigit)){
      return curr.toDouble
    }
    if(featuresOfDomain(curr).isNumerical){
      return config.numericalFeatureValues (featuresOfDomain(curr))
    }
    if(!featuresOfDomain(curr).isNumerical){
      if(config.boolFeatures(featuresOfDomain(curr)))
        return 1.0
    }
    return 0.0
  }
  

  /**
   * 
   * This method performs an evaluation the expression for a given configuration. Thus it evaluates the expression using the feature values used in the 
   * configuration for the features. 
   * 
   * @param config: the configuration
   * 
   * @return the evaluated value of the expression and the configuration
   * 
   */
  def evaluationOfRPN(config: Configuration ): Double = {
    var counter = 0
    
    var stack : scala.collection.mutable.Stack[Double] =  scala.collection.mutable.Stack()
    
    if(expressionArray .length == 0)
      return 1
    
    while(counter < expressionArray .length){
      var curr = expressionArray (counter)
      counter += 1
      if(curr.length() == 0)
        println("error")
      
      if(!isOperatorEval(curr))
        stack.push(getValueOfToken(config, curr))
      else{
        
        if(curr.equals("+")){
          var rightHandSide = stack.pop
          var leftHandSide  = stack.pop
          stack.push(leftHandSide + rightHandSide)
        }
        if(curr.equals("*")){
          var rightHandSide = stack.pop
          var leftHandSide  = stack.pop
          stack.push(leftHandSide * rightHandSide)
        }  
        if(curr.equals("]")){
          var leftHandSide = stack.pop
          if(leftHandSide == 0.0)
          	stack.push(0.0)
          else
            stack.push(Math.log10(leftHandSide))
        }  
      }
    }    
    return stack.pop
  }

  /**
   * The method returns the number of features used in the expression. If a feature is used twice in the expression it is counted as two features. 
   * 
   * @return the number of features. 
   */
  def getNumberOfParticipatingFeatures() : Integer = {
    return numberOfParticipatingFeatures
  }

  /**
   * The method returns the number of distinct features used in this expression.
   * 
   * @return number of distinct features.
   */
  def getNumberOfDistinctParticipatingFeatures() : Integer = {
    return participatingBoolFeatures.size + participatingNumFeatures.size
  }
  
  
  /**
   * 
   * This method evaluates whether a given token is a number of a feature. 
   * 
   * @param the token considered
   * 
   * @return is token a number or feature?
   * 
   */
  def tokenIsAFeatureOrNumber(token : String) : Boolean = {
    var curr = token.trim()
    
    if(curr.forall(_.isDigit)){
      return true
    }
    if(featuresOfDomain.contains(curr)){
      var feature = featuresOfDomain(curr)
      if(feature.isNumerical ){
        if(!participatingNumFeatures .contains(feature))
          participatingNumFeatures.add(feature)
        numberOfParticipatingFeatures +=1
        return true;
      }else{
        if(!participatingBoolFeatures  .contains(feature))
          participatingBoolFeatures.add(feature)
        numberOfParticipatingFeatures +=1
        return true;
      }       
    }
    return false
  }

  override def toString() : String = {
    return wellFormedExpression 
  }
  
}