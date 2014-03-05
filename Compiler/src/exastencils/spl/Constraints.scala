package exastencils.spl

import jp.kobe_u.copris.Constraint
import jp.kobe_u.copris.Iff
import jp.kobe_u.copris.Bool
import jp.kobe_u.copris.Not
import jp.kobe_u.copris.And
import jp.kobe_u.copris.Var
import jp.kobe_u.copris.Expr
import jp.kobe_u.copris.Add
import jp.kobe_u.copris.Term
import jp.kobe_u.copris.Sub
import jp.kobe_u.copris.Mul
import jp.kobe_u.copris.Gt
import jp.kobe_u.copris.ZERO
import jp.kobe_u.copris.Num
import jp.kobe_u.copris.Ge
import jp.kobe_u.copris.Eq
import jp.kobe_u.copris.Lt
import jp.kobe_u.copris.Le

object ConstraintOperator extends Enumeration {
    val implies, excludes = Value
  }

  
  trait ModelConstraint{
    var premise : Feature = null
    var implication : Feature = null
    var condition = ConstraintOperator.implies
    
    def asPropositionalFormular() : Constraint
    
  }

  case class BooleanConstraint(content : String) extends ModelConstraint{
    var literatls = content.trim().split(" ")
    var premiseFinished = false
    for( lit <- literatls){
      var trimmedLit = lit.trim()
      if(trimmedLit.equals("excludes") || trimmedLit.equals("implies")){
        premiseFinished = true
        if(trimmedLit.equals("excludes")){
          this.condition = ConstraintOperator.excludes 
        }
      }else{
    	if(premiseFinished){
    	  implication =  FeatureModel.allFeatures(trimmedLit)
    	}else{
    	  premise = FeatureModel.allFeatures(trimmedLit) 
    	}
        
      }
      
    }
    

    def asPropositionalFormular() : Constraint = {
      val num = condition match {
      		case ConstraintOperator.implies  => Iff(Bool(premise.identifier), Bool(implication.identifier))
      		case ConstraintOperator.excludes => Not(And(Bool(premise.identifier), Bool(implication.identifier)))
      }
      return num
    }
    
  }
  
  /**
   * 
   * 
   * TODO: only one number is allowed on the right hand side 
   * 
   */
  case class NumericalConstraint(content : String) extends ModelConstraint{

    var propositionalFormular : Constraint = null
    var literatls = content.trim().split(" ")
  
    var operator = ""
    var comperator = ""   

    var leftHandSide : Term = Var(literatls(0).trim())
    var lit = 1
    while( lit < literatls.length){
      var trimmedLit = literatls(lit).trim()
      if(!FeatureModel.allFeatures.contains(trimmedLit)){
        // current element is no element -> an operator or an constant
        if(trimmedLit.matches("[\\>\\<\\=\\+\\*\\-]+")){
          // is operator or comperator
          if(trimmedLit.matches("[\\>\\<\\=]+")){
            comperator = trimmedLit
          }else{
            operator = trimmedLit
          }
        }else{
          if(comperator.equals(">"))
        	  propositionalFormular = Gt(leftHandSide,Num(trimmedLit.toInt))
          else if(comperator.equals(">=") || comperator.equals("=>"))
              propositionalFormular = Ge(leftHandSide,Num(trimmedLit.toInt))
          else if(comperator.equals("="))
              propositionalFormular = Eq(leftHandSide,Num(trimmedLit.toInt))
          else if(comperator.equals("<"))
              propositionalFormular = Lt(leftHandSide,Num(trimmedLit.toInt))
          else if(comperator.equals("<=") || comperator.equals("=<"))
              propositionalFormular = Le(leftHandSide,Num(trimmedLit.toInt))
        }
        
      }else{
        // feature found
        if(operator.length() == 0){
          println("Error in numerical constraint parsing")
        }
        if(operator.equals("+")){
          leftHandSide = Add(leftHandSide, Var(trimmedLit)) 
        }else if(operator.equals("-")){
          leftHandSide = Sub(leftHandSide, Var(trimmedLit)) 
        }else if(operator.equals("*")){
            leftHandSide = Mul(leftHandSide, Var(trimmedLit)) 
        }
        operator = ""
      }
      lit +=1
    }
    
    def asPropositionalFormular() : Constraint = {
      return propositionalFormular
    }
    
  }