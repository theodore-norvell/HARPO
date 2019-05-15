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
  
  def initExpCodeGen(init: InitExpNd): String = {
    val result: String = init match {
      case ValueInitExpNd(exp: ExpNd) => simpleExpCodeGen(exp)
      case _ => init.toString() 
    }
    result
  }

  // Generate the expression without augmenting heap(s)
  
  def simpleExpCodeGen(exp: ExpNd): String = {
    
    val result: String = exp match {

      case NoExpNd() => ""
        
      case IntLiteralExpNd(i: Long) => i.toString()
      
      case FloatLiteralExpNd(x: Double) => x.toString()

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => simpleExpCodeGen(x) +" "+ resBiOp(op) +" "+ simpleExpCodeGen(y)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + simpleExpCodeGen(x)

      case MemberExpNd(x: ExpNd, name: String) => simpleExpCodeGen(x) + "." + name

      case FetchExpNd(x: ExpNd) => simpleExpCodeGen(x)

      case AsExpNd(x: ExpNd, _) => simpleExpCodeGen(x)
      
      case NameExpNd( name : NameNd ) =>
        var expCode = ""
        name.decl.get.parent match {
          case Some(nd) => expCode = nd.name + "." + name+ " "
          case None => {}
        }
      expCode
      
      case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd]) => simpleExpCodeGen(operands(0)) + resOpChain(ops(0)) + simpleExpCodeGen(operands(1))
       
      case PermissionOp(objId) => ""
      
      case _ => exp.toString()
    }
    result 
  }

  //Collect all the name nodes used in the expression, essentially needed before asserting the expressions are defined
  
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
      
      case NameExpNd( name : NameNd ) =>  nameExp += name.decl.get.fqn.toString()
      
      case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd]) => {
        for(exp <- operands) 
          nameExpCodeGen(exp)
      }
      
      case CanReadOp(x) => {}
      
      case CanWriteOp(x) => {}

      case _ => ""
    }
   nameExp.toList
  }
  
  def checkGuard(guard: ExpNd) : String = {
    
    val result: String = guard match {

      case NoExpNd() => ""
        
      case IntLiteralExpNd(i: Long) => i.toString()
      
      case FloatLiteralExpNd(x: Double) => x.toString()

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => checkGuard(x) +" "+ resBiOp(op) +" "+ checkGuard(y)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + checkGuard(x)

      case MemberExpNd(x: ExpNd, name: String) => checkGuard(x) + "." + name

      case FetchExpNd(x: ExpNd) => checkGuard(x)

      case AsExpNd(x: ExpNd, _) => checkGuard(x)
      
      case NameExpNd( name : NameNd ) => name.toString()
      
      case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd]) => checkGuard(operands(0)) + resOpChain(ops(0)) + checkGuard(operands(1))
       
      case PermissionOp(objId) => ""
      
      case _ => ""
    }
    result 
    
  }

  //Generate the expressions only for the locked objects
  
  def lockExpCodeGen(lock: ExpNd, transContext: TransContext, perVal : String): String = { 
    
    val result: String = lock match {
      
      case NameExpNd( name : NameNd ) => transContext.heap +"[" +transContext.objRef+"," + name.qn.toString() + "]" + perVal + ";" mkString
      
      case ThisExpNd(str: String) => transContext.heap +"[" +transContext.objRef+"," + str  + "]" + perVal + ";" mkString
      
      case _ => ""
    }
    result 
  }
  
  // Get the names for locked objects, particularly needed when more than one lock appears and need to differentiate different locks
  
  def getNamefromLockExp(lock: ExpNd) : String = {
    lock match {
      case NameExpNd( name : NameNd ) => name.toString() + "_Lock"
      
      case ThisExpNd(str: String) => str + "_lock"
      
      case _ => "" 
    }
  }
  
  //Collect the amount of permission from invariant - locked objects need the invariant permissions
  
  def getPerFromInvExp(exp: ExpNd, lock: ExpNd, lockTransContext: TransContext, baseTranContext: TransContext): String = {

   val result : String =  exp match {
      
      case NoExpNd() => ""
        
      case IntLiteralExpNd(i: Long) => ""
      
      case FloatLiteralExpNd(x: Double) => ""

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => getPerFromInvExp(x,lock,lockTransContext,baseTranContext) + getPerFromInvExp(y,lock,lockTransContext,baseTranContext) mkString
      
      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + getPerFromInvExp(x,lock,lockTransContext,baseTranContext) mkString

      case MemberExpNd(x: ExpNd, name: String) => getPerFromInvExp(x,lock,lockTransContext,baseTranContext) + "." + name mkString

      case FetchExpNd(x: ExpNd) => getPerFromInvExp(x,lock,lockTransContext,baseTranContext) mkString

      case AsExpNd(x: ExpNd, _) => getPerFromInvExp(x,lock,lockTransContext,baseTranContext) mkString
      
      case NameExpNd( name : NameNd ) => ""
      
      case CanReadOp(locSet) => {
          "0.0 < " + lockTransContext.heap + "[" + lockTransContext.objRef+"," + locSet.getName().decl.get.fqn.toString()  + "] < 1.0 "  mkString
      }
      
      case CanWriteOp(locSet) => { // need to recheck for the nested lock, because it needs tto check whether the thread has enough permission to give or not.
        baseTranContext.heap + "[" +baseTranContext.objRef+ "," + locSet.getName().decl.get.fqn.toString() + "] := " + baseTranContext.heap + "[" +baseTranContext.objRef+ "," + locSet.getName().decl.get.fqn.toString() + "] - 1.0;\n" + lockTransContext.heap + "[" + lockTransContext.objRef+ "," + locSet.getName().decl.get.fqn.toString() + "] == 1.0; \n"
      }
        
      case PermissionOp(objId) => "" // return the amount of permission
      
      case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd]) => getPerFromInvExp(operands(0),lock,lockTransContext,baseTranContext) + getPerFromInvExp(operands(1),lock, lockTransContext,baseTranContext)
      
      case _ => ""

    }
   result
  }
  
  
  // Generate only the Boogie expressions for the invariants
  
  def InvExpCodeGen(exp: ExpNd, fqn: String, transContext: TransContext) : String = {
    
    val result: String = exp match {
    
      case NoExpNd() => ""
        
      case IntLiteralExpNd(i: Long) => i.toString
      
      case FloatLiteralExpNd(x: Double) => x.toString

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => InvExpCodeGen(x,fqn,transContext) + " " + resBiOp(op) + " " + InvExpCodeGen(y,fqn,transContext)
      
      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + InvExpCodeGen(x,fqn,transContext)

      case MemberExpNd(x: ExpNd, name: String) => InvExpCodeGen(x,fqn,transContext) + "." + name

      case FetchExpNd(x: ExpNd) => InvExpCodeGen(x,fqn,transContext)

      case AsExpNd(x: ExpNd, _) => InvExpCodeGen(x,fqn,transContext)
      
      case NameExpNd( name : NameNd ) =>
        var expCode = ""
        name.decl.get.parent match {
          case Some(nd) => expCode =  transContext.getHeap()+"["+transContext.getObjRef()+"," + name.decl.get.fqn.toString() +"] " mkString
          case None => {}
        }
      expCode
      
      case CanReadOp(locSet) => {
        val transContextPer = new TransContext("Permission", transContext.getObjRef())
        transContextPer.getHeap() + "[" + transContextPer.getObjRef()+"," + locSet.getName().decl.get.fqn.toString() + "] > 0.0 " mkString
      }
      
      case CanWriteOp(locSet) => {
        val transContextPer = new TransContext("Permission", transContext.getObjRef())
        transContextPer.getHeap() + "[" +transContextPer.getObjRef() + "," + locSet.getName().decl.get.fqn.toString() + "] == 1.0 " mkString
      }
        
      case PermissionOp(objId) => ""// return the amount of permission 
      
      case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd]) => InvExpCodeGen(operands(0),fqn,transContext) + " " + resOpChain(ops(0)) + " " + InvExpCodeGen(operands(1),fqn, transContext)
      
      case _ => exp.toString()
    
  }
    result
  }
  
  // Build the Simple Boogie equivalent expression.
  
  def buildBoogieExp(exp: ExpNd, buildFor: TransContext): String = {
    
    val result: String = exp match {

      case NoExpNd() => ""

      case IntLiteralExpNd(i: Long) => i.toString()

      case FloatLiteralExpNd(x: Double) => x.toString()

      case NameExpNd(name: NameNd) => { 
        buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + name.decl.get.fqn.toString() + "]" mkString
        
      }
      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => buildBoogieExp(x, buildFor) + " " + resBiOp(op) + " " + buildBoogieExp(y, buildFor)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => {
        if(resUnaOp(op) == "'")
          buildBoogieExp(x, buildFor) // Heap Switching
          else
            resUnaOp(op) + buildBoogieExp(x,buildFor)      
      }

      case MemberExpNd(x: ExpNd, name: String) => buildBoogieExp(x, buildFor) + "." + name

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => {
        buildBoogieExp(operands(0),buildFor) + " " + resOpChain(ops(0)) + " " + buildBoogieExp(operands(1),buildFor) mkString
      }
      case FetchExpNd(x: ExpNd) => buildBoogieExp(x, buildFor)

      case AsExpNd(x: ExpNd, _) => buildBoogieExp(x, buildFor)

      case _ => exp.toString()
    }
    result
  }

  // only generate  the expressions having name permited to read, false work to implement reading and writing permissions in one sub routine
  
  def defExpGen(exp: ExpNd, buildFor: TransContext) : String = { 
    val result: String = exp match {

      case NoExpNd() => ""

      case IntLiteralExpNd(i: Long) => ""

      case FloatLiteralExpNd(x: Double) => ""

      case NameExpNd(name: NameNd) => { 
        buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + name.decl.get.fqn.toString() +"]" mkString
        
      }
      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => buildBoogieExp(x, buildFor) + " " + resBiOp(op) + " " + buildBoogieExp(y, buildFor)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + buildBoogieExp(x, buildFor)

      case MemberExpNd(x: ExpNd, name: String) => buildBoogieExp(x, buildFor) + "." + name

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => {
        buildBoogieExp(operands(0),buildFor) + " " + resOpChain(ops(0)) + " " + buildBoogieExp(operands(1),buildFor) mkString
      }
      case FetchExpNd(x: ExpNd) => buildBoogieExp(x, buildFor)

      case AsExpNd(x: ExpNd, _) => buildBoogieExp(x, buildFor)

      case _ => ""
    }
    result
  }

  // Get the expressions to assert the reading permissions
  
  def buildReadingPerExp(exp: ExpNd, buildFor: TransContext): String = { // Permission Greater than '0.0'

    val result: String = exp match {

      case NameExpNd(name: NameNd) => buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + name.decl.get.fqn.toString() + "] > 0.0 " mkString
      
      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => {
        val xExp = buildReadingPerExp(x, buildFor)
        val yExp = buildReadingPerExp(y, buildFor)
        val result = (xExp.isEmpty(),yExp.isEmpty()) match {
          case (true,true) => ""
          case (true,false) => buildReadingPerExp(x, buildFor)
          case (false,true) => buildReadingPerExp(y, buildFor)
          case (false,false) => buildReadingPerExp(x, buildFor) + " && " + buildReadingPerExp(y, buildFor) mkString
          
        }
        result
      }

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + buildReadingPerExp(x, buildFor)

      case MemberExpNd(x: ExpNd, name: String) => buildReadingPerExp(x, buildFor) + "." + name

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => {
        val xExp = buildReadingPerExp(operands(0), buildFor)
        val yExp = buildReadingPerExp(operands(1), buildFor)
        val result = (xExp.isEmpty(),yExp.isEmpty()) match {
          case (true,true) => ""
          case (true,false) => buildReadingPerExp(operands(0), buildFor)
          case (false,true) => buildReadingPerExp(operands(1), buildFor)
          case (false,false) => buildReadingPerExp(operands(0), buildFor) + " && " + buildReadingPerExp(operands(1), buildFor) mkString 
        }
        result
        }
      case FetchExpNd(x: ExpNd) => buildReadingPerExp(x, buildFor)

      case AsExpNd(x: ExpNd, _) => buildReadingPerExp(x, buildFor)
      
      case CanReadOp(x) => buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + x.getName().qn.toString() + "] > 0.0 " mkString
      
      case CanWriteOp(y) => buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + y.getName().qn.toString() + "] == 1.0 " mkString

      case _ => ""
    }
    result
  
  }
  
  def assertReadingPerExp(exp: ExpNd, buildFor: => TransContext): String = { 
    // For Defindness of the invariant expression, drop the CanRead,CanWrite operations
    val result: String = exp match {

      case NoExpNd() => "true"

      case IntLiteralExpNd(i: Long) => "true"

      case FloatLiteralExpNd(x: Double) => "true"
      
      case CanReadOp(loc) =>  {
        
        buildFor.getHeap() + "[" + buildFor.getObjRef()+"," + loc.getName().decl.get.fqn.toString() + "] > 0.0 " mkString
      }
        
      case CanWriteOp(loc) => {
        buildFor.getHeap() + "[" + buildFor.getObjRef()+"," + loc.getName().decl.get.fqn.toString() + "] == 1.0 " mkString
      }

      case NameExpNd(name: NameNd) => { 
        buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + name.decl.get.fqn.toString()+ "] > 0.0 " mkString
        
      }
      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => assertReadingPerExp(x, buildFor) + " && " + assertReadingPerExp(y, buildFor)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + assertReadingPerExp(x, buildFor)

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => {
        assertReadingPerExp(operands(0),buildFor) + " && " + assertReadingPerExp(operands(1),buildFor) mkString
      }
      case MemberExpNd(x: ExpNd, name: String) => assertReadingPerExp(x, buildFor) + "." + name

      case FetchExpNd(x: ExpNd) => assertReadingPerExp(x, buildFor)

      case AsExpNd(x: ExpNd, _) => assertReadingPerExp(x, buildFor)

      case _ => ""
    }
    result
  }
  
  //Used for the defindness check to produce the aseertions on writing permission in expressions
  
  def buildWritingPerExp(exp: ExpNd, buildFor: TransContext): String = { // Permission must be equal to 1.0;
    
    val result: String = exp match {

      case NoExpNd() => ""

      case IntLiteralExpNd(i: Long) => ""

      case FloatLiteralExpNd(x: Double) => ""

      case NameExpNd(name: NameNd) => { 
        buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + name.decl.get.fqn.toString()+ "] == 1.0 " mkString
        
      }
      case MemberExpNd(x: ExpNd, name: String) => buildWritingPerExp(x, buildFor) + "." + name

      case FetchExpNd(x: ExpNd) => buildWritingPerExp(x, buildFor)

      case AsExpNd(x: ExpNd, _) => buildWritingPerExp(x, buildFor)

      case _ => ""
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