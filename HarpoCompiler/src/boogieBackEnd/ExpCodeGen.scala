package boogieBackEnd
import frontEnd._
import frontEnd.AST._
import java.io._
import checker.Checker
import checker.CheckerTypes._
import scala.collection.mutable.ArrayBuffer;

class ExpCodeGen() {
 

  var nameExp = ArrayBuffer[String]();
  
  def getExpCode(init: InitExpNd, name: String): String = {
    val result: String = init match {
      case ValueInitExpNd(exp: ExpNd) => getExpCode(exp)
      case _ => init.toString()
    }
    result
  }

  def getExpCode(expNd: ExpNd): String = { 
    
    val result: String = expNd match {

      case NoExpNd() => ""
        
      case IntLiteralExpNd(i: Long) => i.toString()
      
      case FloatLiteralExpNd(x: Double) => x.toString()

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => List("(", getExpCode(x), resBiOp(op), getExpCode(y), ")") mkString ""

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + getExpCode(x)

      case MemberExpNd(x: ExpNd, name: String) => getExpCode(x) + "." + name

      case FetchExpNd(x: ExpNd) => getExpCode(x)

      case AsExpNd(x: ExpNd, _) => getExpCode(x)
      
      case NameExpNd( name : NameNd ) =>
        var expCode = ""
        name.decl.get.parent match {
          case Some(nd) => expCode = nd.name + "." + name+ " "
          case None => {}
        }
      expCode
      
      case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd]) => getExpCode(operands(0)) + resOpChain(ops(0)) + getExpCode(operands(1))

      case _ => expNd.toString()
    }
    result 
  }

    def getNameExpCode(expNd: ExpNd): List [String] = { 
      
     expNd match {

      case NoExpNd() => {}
        
      case IntLiteralExpNd(i: Long) => {}
      
      case FloatLiteralExpNd(x: Double) => {}

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => getNameExpCode(x) ; getNameExpCode(y)
      
      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => getNameExpCode(x)

      case MemberExpNd(x: ExpNd, name: String) => getNameExpCode(x)

      case FetchExpNd(x: ExpNd) => getNameExpCode(x)

      case AsExpNd(x: ExpNd, _) => getNameExpCode(x)
      
      case NameExpNd( name : NameNd ) => println("Reached Here")
        name.decl.get.parent match {
          case Some(nd) => nameExp += (nd.name + "." + name+ " "); 
          case None => {}
        }
      
      case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd]) => {
        for(exp <- operands) 
          getNameExpCode(exp)
      }

      case _ => expNd.toString()
    }
   nameExp.toList
  }
    
  def getLockExpCode(expNd: ExpNd): String = { 
    
    val result: String = expNd match {
      
      case NameExpNd( name : NameNd ) => name.toString
      
      case _ => expNd.toString()
    }
    result 
  }
  
    
    
  private def resBiOp(op: BinaryOperator) = {
    val result = op match {
      //case ImpliesOp()=>
      case EquivOp => "=="
      case OrOp => "||"
      case AndOp => "&&"
      case AddOp => "+"
      case SubOp => "-"
      case MulOp => "*"
      case SlashDivOp => "/"
      //case WordDivOp()=>
      //case RemOp()=>
      //case IndexOp()=>
      case _ => op.toString()
    }
    result
  }
  private def resUnaOp(op: UnaryOperator) = {
    val result = op match {
      case NegativeOp => "-"
      case NotOp => "!"
      case PrimeOp => "'"
    }
    result
  }
  private def resOpChain(op: ChainingOperator): String = {
    val result: String = op match {
      case EqualOp => "=="
      case NotEqualOp => "!="
      case LessOp => "<"
      case LessOrEqualOp => "<="
      case GreaterOp => ">"
      case GreaterOrEqualOp => ">="
    }
    result
  }

}