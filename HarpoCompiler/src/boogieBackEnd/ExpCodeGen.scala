package boogieBackEnd
import frontEnd._
import frontEnd.AST._
import java.io._
import checker.Checker
import checker.CheckerTypes._
import scala.collection.mutable.ArrayBuffer;
import util.OutputBuilder;

class ExpCodeGen() {

  var nameExp = ArrayBuffer[String]();
  
  def initExpCodeGen(init: InitExpNd, name: String): String = {
    val result: String = init match {
      case ValueInitExpNd(exp: ExpNd) => expCodeGen(exp)
      case _ => init.toString()
    }
    result
  }

  def expCodeGen(exp: ExpNd): String = { 
    
    val result: String = exp match {

      case NoExpNd() => ""
        
      case IntLiteralExpNd(i: Long) => i.toString()
      
      case FloatLiteralExpNd(x: Double) => x.toString()

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => expCodeGen(x) +" "+ resBiOp(op) +" "+ expCodeGen(y)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + expCodeGen(x)

      case MemberExpNd(x: ExpNd, name: String) => expCodeGen(x) + "." + name

      case FetchExpNd(x: ExpNd) => expCodeGen(x)

      case AsExpNd(x: ExpNd, _) => expCodeGen(x)
      
      case NameExpNd( name : NameNd ) =>
        var expCode = ""
        name.decl.get.parent match {
          case Some(nd) => expCode = nd.name + "." + name+ " "
          case None => {}
        }
      expCode
      
      case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd]) => expCodeGen(operands(0)) + resOpChain(ops(0)) + expCodeGen(operands(1))
       
      case PermissionOp(objId) => ""
      
      case _ => exp.toString()
    }
    result 
  }

    def nameExpCodeGen(expNd: ExpNd): List [String] = { 
      
     expNd match {

      case NoExpNd() => {}
        
      case IntLiteralExpNd(i: Long) => {}
      
      case FloatLiteralExpNd(x: Double) => {}

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => nameExpCodeGen(x) ; nameExpCodeGen(y)
      
      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => nameExpCodeGen(x)

      case MemberExpNd(x: ExpNd, name: String) => nameExpCodeGen(x)

      case FetchExpNd(x: ExpNd) => nameExpCodeGen(x)

      case AsExpNd(x: ExpNd, _) => nameExpCodeGen(x)
      
      case NameExpNd( name : NameNd ) => println("Reached Here")
        name.decl.get.parent match {
          case Some(nd) => nameExp += (nd.name + "." + name+ " "); 
          case None => {}
        }
      
      case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd]) => {
        for(exp <- operands) 
          nameExpCodeGen(exp)
      }

      case _ => expNd.toString()
    }
   nameExp.toList
  }
    
  def lockExpCodeGen(expNd: ExpNd): String = { 
    
    val result: String = expNd match {
      
      case NameExpNd( name : NameNd ) => name.toString
      
      case _ => expNd.toString()
    }
    result 
  }
  
  def InvExpCodeGen(exp: ExpNd, fqn: String, objRef: String) : String = {
    
    val result: String = exp match {
    
      case NoExpNd() => ""
        
      case IntLiteralExpNd(i: Long) => i.toString
      
      case FloatLiteralExpNd(x: Double) => x.toString

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => InvExpCodeGen(x,fqn,objRef) + " " + resBiOp(op) + " " + InvExpCodeGen(y,fqn,objRef)
      
      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + InvExpCodeGen(x,fqn,objRef)

      case MemberExpNd(x: ExpNd, name: String) => InvExpCodeGen(x,fqn,objRef) + "." + name

      case FetchExpNd(x: ExpNd) => InvExpCodeGen(x,fqn,objRef)

      case AsExpNd(x: ExpNd, _) => InvExpCodeGen(x,fqn,objRef)
      
      case NameExpNd( name : NameNd ) =>
        var expCode = ""
        name.decl.get.parent match {
          case Some(nd) => expCode =  "Heap[ "+objRef+"," + nd.name + "." + name + "] "
          case None => {}
        }
      expCode
      
      case CanReadOp(locSet) =>  "Permission["+objRef+"," + fqn + "." + locSet.getName().qn.toString  + "] > 0.0 "
      
      case CanWriteOp(locSet) => "Permission["+objRef+"," + fqn + "." + locSet.getName().qn.toString  + "] == 1.0 "
        
      case PermissionOp(objId) => "" // return the amount of permission
      
      case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd]) => InvExpCodeGen(operands(0),fqn, objRef) + " " + resOpChain(ops(0)) + " " + InvExpCodeGen(operands(1),fqn,objRef)
      
      case _ => exp.toString()
    
  }
    result
  }
  
  // Build the Boogie equivalent expression.
  def buildExp(expNd: ExpNd, buildFor: TransContext): String = {
    
    val result: String = expNd match {

      case NoExpNd() => ""

      case IntLiteralExpNd(i: Long) => i.toString()

      case FloatLiteralExpNd(x: Double) => x.toString()

      case NameExpNd(name: NameNd) => { 
        buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + name.qn.toString() + "]" // FQN
      }
      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => buildExp(x, buildFor) + " " + resBiOp(op) + " " + buildExp(y, buildFor)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + buildExp(x, buildFor)

      case MemberExpNd(x: ExpNd, name: String) => buildExp(x, buildFor) + "." + name

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => {
        var chainExp : String = ""
        for (op <- ops) {
          chainExp.concat(buildExp(operands(ops.indexOf(op)), buildFor) + " " + chainingOpTrans(op) + " " + buildExp(operands(ops.indexOf(op) + 1), buildFor))
          }
        chainExp;
      }
      case FetchExpNd(x: ExpNd) => buildExp(x, buildFor)

      case AsExpNd(x: ExpNd, _) => buildExp(x, buildFor) // TODO insert type conversions?

      case _ => expNd.toString()
    }
    result
  }

   private def chainingOpTrans(op: ChainingOperator): String = {
    op match {
      case EqualOp => return "=="
      case NotEqualOp => return "!="
      case LessOp => return "<"
      case LessOrEqualOp => return "<="
      case GreaterOp => return ">"
      case GreaterOrEqualOp => return ">="
    }
  }
   
   private def biOpTrans(op: BinaryOperator): String = {
    op match {
      case OrOp => return "||"
      case AndOp => return "&&"
      case AddOp => return "+"
      case SubOp => return "-"
      case MulOp => return "*"
      case SlashDivOp => return "/"
      case WordDivOp => return "/"
      case RemOp => return "%"
      case _ => return ""
    }
  }
  
  private def resBiOp(op: BinaryOperator) = {
    val result = op match {
      case ImpliesOp => "==>"
      case EquivOp => "=="
      case OrOp => "||"
      case AndOp => "&&"
      case AddOp => "+"
      case SubOp => "-"
      case MulOp => "*"
      case SlashDivOp => "/"
      case WordDivOp => "/"
      case RemOp => "%"
      //case IndexOp =>
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