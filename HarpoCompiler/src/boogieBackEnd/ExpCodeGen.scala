package boogieBackEnd
import frontEnd._
import frontEnd.AST._
import java.io._
import checker.Checker
import checker.CheckerTypes._
import scala.collection.mutable.ArrayBuffer;
import util.OutputBuilder;
import contracts.Contracts

object ExpCodeGen {

  def initExpCodeGen(init: InitExpNd, heapTransContext: TransContext): String = {
    val result: String = init match {
      case ValueInitExpNd(exp: ExpNd) => valueInitExpCodeGen(exp, heapTransContext)
      case NewInitExpNd(ty, args) => "TODO" //TODO
      case ArrayInitExpNd(decl, bound, init) => "TODO" //TODO
      case IfInitExpNd(guard, init_a, init_b) => "TODO" //TODO
      case WidenInitExpNd(init) => initExpCodeGen(init, heapTransContext) // TODO
    }
    result
  }

  //Compute the literal value of Expression

  def computeExpressionValue(exp: ExpNd): String = {

    val result: String = exp match {

      case NoExpNd() => ""

      case BooleanLiteralExpNd(b: String) => b

      case IntLiteralExpNd(i: Long) => i.toString

      case FloatLiteralExpNd(x: Double) => {

        val fmt = new java.text.DecimalFormat("####0.0########################")
        // 24 Decimal places
        // 0 : Digit will always be show otherwise 0 character placed'
        // # show digit only when it is not 0 otherwise omit
        fmt.format(x).toString()
        // ####0.0########################
      }

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => computeExpressionValue(x) + " " + resBiOp(op) + " " + computeExpressionValue(y)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + computeExpressionValue(x)

      case MemberExpNd(x: ExpNd, name: String) => computeExpressionValue(x) + "." + name

      case FetchExpNd(x: ExpNd) => computeExpressionValue(x)

      case AsExpNd(x: ExpNd, _) => computeExpressionValue(x)

      case NameExpNd(name: NameNd) =>
        var expCode = ""
        name.decl.get.parent match {
          case Some(nd) => expCode = nd.name + "." + name + " "
          case None => {}
        }
        expCode

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => simpleExpCodeGen(operands(0)) + resOpChain(ops(0)) + simpleExpCodeGen(operands(1))

      case _ => exp.toString()
    }
    result
  }

  // Generate the expression without augmenting heap(s) need when collecting the numerals

  def valueInitExpCodeGen(exp: ExpNd, heapTransContext: TransContext): String = {

    val result: String = exp match {

      case NoExpNd() => ""

      case BooleanLiteralExpNd(b: String) => b

      case IntLiteralExpNd(i: Long) => i.toString()

      case FloatLiteralExpNd(x: Double) => {

        val fmt = new java.text.DecimalFormat("####0.0########################")
        // 24 Decimal places
        // 0 : Digit will always be show otherwise 0 character placed'
        // # show digit only when it is not 0 otherwise omit
        fmt.format(x).toString()
        // ####0.0########################
      }

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => valueInitExpCodeGen(x, heapTransContext) + " " + resBiOp(op) + " " + valueInitExpCodeGen(y, heapTransContext)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + valueInitExpCodeGen(x, heapTransContext)

      case MemberExpNd(x: ExpNd, name: String) => valueInitExpCodeGen(x, heapTransContext) + "." + name

      case FetchExpNd(x: ExpNd) => valueInitExpCodeGen(x, heapTransContext)

      case AsExpNd(x: ExpNd, _) => valueInitExpCodeGen(x, heapTransContext)

      case NameExpNd(name: NameNd) => {
        var expCode = heapTransContext.getHeap() + "[" + heapTransContext.getObjRef() + "," + name.decl.get.fqn.toString() + "]"
        expCode
      }

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => valueInitExpCodeGen(operands(0), heapTransContext) + resOpChain(ops(0)) + valueInitExpCodeGen(operands(1), heapTransContext)

      case PermissionOp(objId) => ""

      case _ => exp.toString()
    }
    result
  }

  // Generate the expression without augmenting heap(s) need when collecting the numerals

  def simpleExpCodeGen(exp: ExpNd): String = {

    val result: String = exp match {

      case NoExpNd() => ""

      case BooleanLiteralExpNd(b: String) => b

      case IntLiteralExpNd(i: Long) => i.toString()

      case FloatLiteralExpNd(x: Double) => {

        val fmt = new java.text.DecimalFormat("####0.0########################")
        // 24 Decimal places
        // 0 : Digit will always be show otherwise 0 character placed'
        // # show digit only when it is not 0 otherwise omit
        fmt.format(x).toString()
        // ####0.0########################
      }

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => simpleExpCodeGen(x) + " " + resBiOp(op) + " " + simpleExpCodeGen(y)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + simpleExpCodeGen(x)

      case MemberExpNd(x: ExpNd, name: String) => simpleExpCodeGen(x) + "." + name

      case FetchExpNd(x: ExpNd) => simpleExpCodeGen(x)

      case AsExpNd(x: ExpNd, _) => simpleExpCodeGen(x)

      case NameExpNd(name: NameNd) =>
        var expCode = ""
        name.decl.get.parent match {
          case Some(nd) => expCode = nd.name + "." + name + " "
          case None => {}
        }
        expCode

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => simpleExpCodeGen(operands(0)) + resOpChain(ops(0)) + simpleExpCodeGen(operands(1))

      case PermissionOp(objId) => ""

      case _ => exp.toString()
    }
    result
  }

  //Collect all the name nodes used in the expression, essentially needed before asserting the expressions are defined

  def nameExpCodeGen(expNd: ExpNd, nameExp: ArrayBuffer[String]): List[String] = {

    expNd match {

      case NoExpNd() => {}

      case IntLiteralExpNd(i: Long) => {}

      case FloatLiteralExpNd(x: Double) => {}

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) =>
        nameExpCodeGen(x, nameExp); nameExpCodeGen(y, nameExp)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => nameExpCodeGen(x, nameExp)

      case MemberExpNd(x: ExpNd, name: String) => nameExpCodeGen(x, nameExp)

      case FetchExpNd(x: ExpNd) => nameExpCodeGen(x, nameExp)

      case AsExpNd(x: ExpNd, _) => nameExpCodeGen(x, nameExp)

      case NameExpNd(name: NameNd) => nameExp += name.decl.get.fqn.toString()

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => {
        for (exp <- operands)
          nameExpCodeGen(exp, nameExp)
      }

      case CanReadOp(x) => getNameFromLocSet(x, nameExp)

      case CanWriteOp(x) => getNameFromLocSet(x, nameExp)
      
      case AccessOp(pm) => for(loc <- pm.locSet){
        getNameFromLocSet(loc, nameExp)
      }

      case _ => ""
    }

    nameExp.toList
  }
 
  def getNameFromLocSet(x: LocSetNd, nameExp: ArrayBuffer[String]) {
    x match {
      case ObjectIdLSN(exp: ExpNd) => nameExpCodeGen(exp, nameExp)
      case ArrayLSN(forDecl: ForDecl, offSet: ExpNd, bound: ExpNd, boundInclusive: Boolean, locSet: LocSetNd) => getNameFromLocSet(locSet, nameExp)
    }
  }

  //Generate the expressions only for the locked objects

  def lockExpCodeGen(lock: ExpNd, transContext: TransContext, perVal: String): String = {

    val result: String = lock match {

      case NameExpNd(name: NameNd) => transContext.heap + "[" + transContext.objRef + "," + name.qn.toString() + "]" + perVal + ";" mkString

      case ThisExpNd(str: String) => transContext.heap + "[" + transContext.objRef + "," + str + "]" + perVal + ";" mkString

      case _ => ""
    }
    result
  }

  // Get the names for locked objects, particularly needed when more than one lock appears and need to differentiate different locks

  def getNamefromLockExp(lock: ExpNd): String = {
    lock match {
      case NameExpNd(name: NameNd) => name.toString() + "_Lock"

      case ThisExpNd(str: String) => str + "_lock"

      case _ => ""
    }
  }

  //Collect the amount of permission from invariant - locked objects need the invariant permissions

  def getPerFromInvExp(exp: ExpNd, lock: ExpNd, lockTransContext: TransContext, baseTranContext: TransContext): String = {

    val result: String = exp match {

      case NoExpNd() => ""

      case IntLiteralExpNd(i: Long) => ""

      case FloatLiteralExpNd(x: Double) => ""

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => getPerFromInvExp(x, lock, lockTransContext, baseTranContext) + getPerFromInvExp(y, lock, lockTransContext, baseTranContext) mkString

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + getPerFromInvExp(x, lock, lockTransContext, baseTranContext) mkString

      case MemberExpNd(x: ExpNd, name: String) => getPerFromInvExp(x, lock, lockTransContext, baseTranContext) + "." + name mkString

      case FetchExpNd(x: ExpNd) => getPerFromInvExp(x, lock, lockTransContext, baseTranContext) mkString

      case AsExpNd(x: ExpNd, _) => getPerFromInvExp(x, lock, lockTransContext, baseTranContext) mkString

      case NameExpNd(name: NameNd) => ""

      case CanReadOp(locSet) => {
        lockTransContext.heap + "[" + lockTransContext.objRef + "," + NameManager.getFQN(locSet) + "] > 0.0 " mkString
      }

      case CanWriteOp(locSet) => { //TODO // need to recheck for the nested lock, because it needs tto check whether the thread has enough permission to give or not.
        baseTranContext.heap + "[" + baseTranContext.objRef + "," + NameManager.getFQN(locSet) + "] := " + baseTranContext.heap + "[" + baseTranContext.objRef + "," + NameManager.getFQN(locSet) + "] - 1.0;\n" + lockTransContext.heap + "[" + lockTransContext.objRef + "," + NameManager.getFQN(locSet) + "] == 1.0; \n"
      }

      case PermissionOp(objId) => {
        lockTransContext.heap + "[" + lockTransContext.objRef + "," + NameManager.getFQN(objId) + "]" mkString
      }

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => getPerFromInvExp(operands(0), lock, lockTransContext, baseTranContext) + getPerFromInvExp(operands(1), lock, lockTransContext, baseTranContext)

      case _ => ""

    }
    result
  }

  // Generate only the Boogie expressions for the invariants

  def InvExpCodeGen(exp: ExpNd, transContext: TransContext): String = {

    val result: String = exp match {

      case NoExpNd() => ""

      case IntLiteralExpNd(i: Long) => i.toString

      case FloatLiteralExpNd(x: Double) => {
        val fmt = new java.text.DecimalFormat("####0.0########################")
        // 24 Decimal places
        // 0 : Digit will always be show otherwise 0 character placed'
        // # show digit only when it is not 0 otherwise omit
        fmt.format(x).toString()
        // ####0.0########################
      }

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => InvExpCodeGen(x, transContext) + " " + resBiOp(op) + " " + InvExpCodeGen(y, transContext)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + "(" + InvExpCodeGen(x, transContext) + ")"

      case MemberExpNd(x: ExpNd, name: String) => InvExpCodeGen(x, transContext) + "." + name

      case FetchExpNd(x: ExpNd) => InvExpCodeGen(x, transContext)

      case AsExpNd(x: ExpNd, _) => InvExpCodeGen(x, transContext)

      case NameExpNd(name: NameNd) =>
        var expCode = ""
        name.decl.get.parent match {
          case Some(nd) => expCode = transContext.getHeap() + "[" + transContext.getObjRef() + "," + name.decl.get.fqn.toString() + "] " mkString
          case None => {}
        }
        expCode

      case CanReadOp(locSet) => {
        val transContextPer = new TransContext("Permission", transContext.getObjRef())
        transContextPer.getHeap() + "[" + transContextPer.getObjRef() + "," + NameManager.getFQN(locSet) + "] > 0.0 " mkString
      }

      case CanWriteOp(locSet) => {
        val transContextPer = new TransContext("Permission", transContext.getObjRef())
        transContextPer.getHeap() + "[" + transContextPer.getObjRef() + "," + NameManager.getFQN(locSet) + "] == 1.0 " mkString
      }

      case AccessOp(pmn) => {
        var str : String = "";
        val transContextPer = new TransContext("Permission", transContext.getObjRef())
        val perm_pairs = pmn.pm;
        for ((loc, amnt) <- perm_pairs) {
          if (!(perm_pairs.equals(perm_pairs.lastOption))) {
            str.concat(transContextPer.getHeap() + "[" + transContextPer.getObjRef() + "," + NameManager.getFQN(loc) + "] == " + amnt + " && " mkString)
          } else {
            str.concat(transContextPer.getHeap() + "[" + transContextPer.getObjRef() + "," + NameManager.getFQN(loc) + "] == " + amnt mkString)
          }
        }
        str
      }

      case PermissionOp(objId) => transContext.heap + "[" + transContext.objRef + "," + NameManager.getFQN(objId) + "]" mkString

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => "(" + InvExpCodeGen(operands(0), transContext) + ") " + resOpChain(ops(0)) + " (" + InvExpCodeGen(operands(1), transContext) + ")"

      case _ => exp.toString()

    }
    result
  }
  
  // Invariant Permission Expression Code Generation

  def InvPermExpCodeGen(exp: ExpNd, transContext: TransContext, res: String): String = {

    exp match {

      case NoExpNd() => ""

      case IntLiteralExpNd(i: Long) => ""

      case FloatLiteralExpNd(x: Double) =>""
      
      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => {
        if( (InvPermExpCodeGen(x,transContext,res).equals("")) && !(InvPermExpCodeGen(y,transContext,res).equals(""))) {
          InvPermExpCodeGen(y, transContext,res)
        }
        else if ((InvPermExpCodeGen(y,transContext,res).equals("")) && !(InvPermExpCodeGen(x,transContext,res).equals(""))){
          InvPermExpCodeGen(x, transContext,res)
        }
        else if (!(InvPermExpCodeGen(x,transContext,res).equals("")) &&  !(InvPermExpCodeGen(y,transContext,res).equals(""))) {
        InvPermExpCodeGen(x, transContext,res) + " " + resBiOp(op) + " " + InvPermExpCodeGen(y, transContext,res) 
        }
        else ""
        
      }

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + "(" + InvPermExpCodeGen(x, transContext,res) + ")"

      case MemberExpNd(x: ExpNd, name: String) => InvPermExpCodeGen(x, transContext,res) + "." + name

      case FetchExpNd(x: ExpNd) => InvPermExpCodeGen(x, transContext,res)

      case AsExpNd(x: ExpNd, _) => InvPermExpCodeGen(x, transContext,res)

      case NameExpNd(name: NameNd) => {
        var expCode = ""
        name.decl.get.parent match {
          case Some(nd) => expCode = transContext.getHeap() + "[" + transContext.getObjRef() + "," + name.decl.get.fqn.toString() + "] " mkString
          case None => {}
        }
        expCode
      }
      case CanReadOp(locSet) => {
        val transContextPer = new TransContext("Permission", transContext.getObjRef())
        res.concat(transContextPer.getHeap() + "[" + transContextPer.getObjRef() + "," + NameManager.getFQN(locSet) + "] > 0.0 " mkString)
      }

      case CanWriteOp(locSet) => {
        val transContextPer = new TransContext("Permission", transContext.getObjRef())
        res.concat(transContextPer.getHeap() + "[" + transContextPer.getObjRef() + "," + NameManager.getFQN(locSet) + "] == 1.0 " mkString)
      }

      case AccessOp(pmn) => {
        var str : String = "";
        val transContextPer = new TransContext("Permission", transContext.getObjRef())
        val perm_pairs = pmn.pm;
        for ((loc, amnt) <- perm_pairs) {
          if (!(perm_pairs.equals(perm_pairs.lastOption))) {
            str.concat(transContextPer.getHeap() + "[" + transContextPer.getObjRef() + "," + NameManager.getFQN(loc) + "] == " + amnt + " && " mkString)
          } else {
            str.concat(transContextPer.getHeap() + "[" + transContextPer.getObjRef() + "," + NameManager.getFQN(loc) + "] == " + amnt mkString)
          }
        }
        res.concat(str)
      }

      case PermissionOp(objId) => transContext.heap + "[" + transContext.objRef + "," + NameManager.getFQN(objId) + "]" mkString

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => {
        
        if(InvPermExpCodeGen(operands(0),transContext,res).equals("") && !(InvPermExpCodeGen(operands(1),transContext,res).equals(""))) {
          InvPermExpCodeGen(operands(1), transContext,res)
        }
        else if (InvPermExpCodeGen(operands(1),transContext,res).equals("") && !(InvPermExpCodeGen(operands(0),transContext,res).equals(""))){
          InvPermExpCodeGen(operands(0), transContext,res)
        }
        else if (!(InvPermExpCodeGen(operands(1),transContext,res).equals("")) &&  !(InvPermExpCodeGen(operands(0),transContext,res).equals(""))) {
        "(" + InvPermExpCodeGen(operands(0), transContext,res) + ") " + resOpChain(ops(0)) + " (" + InvPermExpCodeGen(operands(1), transContext,res) + ")"
        }
        else ""
        
      }
        

      case _ => exp.toString()

    }
    res
  }
  
  

  def lockedInvExpCodeGen(exp: ExpNd, transContext: TransContext, nameExp$: ArrayBuffer[String]): List[String] = {

    exp match {

      case NoExpNd() => ""

      case IntLiteralExpNd(i: Long) => i.toString

      case FloatLiteralExpNd(x: Double) => {

        val fmt = new java.text.DecimalFormat("####0.0########################")
        // 24 Decimal places
        // 0 : Digit will always be show otherwise 0 character placed'
        // # show digit only when it is not 0 otherwise omit
        fmt.format(x).toString()
        // ####0.0########################
      }

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => lockedInvExpCodeGen(x, transContext, nameExp$) + "+" + lockedInvExpCodeGen(y, transContext, nameExp$)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => lockedInvExpCodeGen(x, transContext, nameExp$)

      case MemberExpNd(x: ExpNd, name: String) => lockedInvExpCodeGen(x, transContext, nameExp$) + "." + name

      case FetchExpNd(x: ExpNd) => lockedInvExpCodeGen(x, transContext, nameExp$)

      case AsExpNd(x: ExpNd, _) => lockedInvExpCodeGen(x, transContext, nameExp$)

      case NameExpNd(name: NameNd) => ""

      case CanReadOp(locSet) => {
        nameExp$ += transContext.getHeap() + "[" + transContext.getObjRef() + "," + NameManager.getFQN(locSet) + "]" mkString
      }

      case CanWriteOp(locSet) => {
        nameExp$ += transContext.getHeap() + "[" + transContext.getObjRef() + "," + NameManager.getFQN(locSet) + "]" mkString
      }

      case PermissionOp(objId) => transContext.heap + "[" + transContext.objRef + "," + NameManager.getFQN(objId) + "]" mkString

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => lockedInvExpCodeGen(operands(0), transContext, nameExp$) + " + " + lockedInvExpCodeGen(operands(1), transContext, nameExp$)

      case _ => exp.toString()

    }
    nameExp$.toList
  }

  // an overloaded Invariant Expression Code Generation Method, locked objects need different Invariant Code Generation
  def InvExpCodeGen(exp: ExpNd, lockTransContext: TransContext, baseTransContext: TransContext): String = {

    val result: String = exp match {

      case NoExpNd() => ""

      case IntLiteralExpNd(i: Long) => i.toString

      case FloatLiteralExpNd(x: Double) => {

        val fmt = new java.text.DecimalFormat("####0.0########################")
        // 24 Decimal places
        // 0 : Digit will always be show otherwise 0 character placed'
        // # show digit only when it is not 0 otherwise omit
        fmt.format(x).toString()
        // ####0.0########################
      }

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => InvExpCodeGen(x, lockTransContext, baseTransContext) + " " + resBiOp(op) + " " + InvExpCodeGen(y, lockTransContext, baseTransContext)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + InvExpCodeGen(x, lockTransContext, baseTransContext)

      case MemberExpNd(x: ExpNd, name: String) => InvExpCodeGen(x, lockTransContext, baseTransContext) + "." + name

      case FetchExpNd(x: ExpNd) => InvExpCodeGen(x, lockTransContext, baseTransContext)

      case AsExpNd(x: ExpNd, _) => InvExpCodeGen(x, lockTransContext, baseTransContext)

      case NameExpNd(name: NameNd) =>
        var expCode = ""
        name.decl.get.parent match {
          case Some(nd) => expCode = baseTransContext.getHeap() + "[" + baseTransContext.getObjRef() + "," + name.decl.get.fqn.toString() + "] " mkString
          case None => {}
        }
        expCode

      case CanReadOp(locSet) => {
        lockTransContext.getHeap() + "[" + lockTransContext.getObjRef() + "," + NameManager.getFQN(locSet) + "] > 0.0 " mkString
      }

      case CanWriteOp(locSet) => {
        lockTransContext.getHeap() + "[" + lockTransContext.getObjRef() + "," + NameManager.getFQN(locSet) + "] == 1.0 " mkString
      }

      case PermissionOp(objId) => baseTransContext.heap + "[" + baseTransContext.objRef + "," + NameManager.getFQN(objId) + "]" mkString

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => InvExpCodeGen(operands(0), lockTransContext, baseTransContext) + " " + resOpChain(ops(0)) + " " + InvExpCodeGen(operands(1), lockTransContext, baseTransContext)

      case _ => exp.toString()

    }
    result
  }

  // Build the Simple Boogie equivalent expression.

  def buildBoogieExp(exp: ExpNd, buildFor: TransContext): String = {

    val result: String = exp match {

      case NoExpNd() => ""

      case BooleanLiteralExpNd(b) => b.toString()

      case IntLiteralExpNd(i: Long) => i.toString()

      case FloatLiteralExpNd(x: Double) => {

        val fmt = new java.text.DecimalFormat("####0.0########################")
        // 24 Decimal places
        // 0 : Digit will always be show otherwise 0 character placed'
        // # show digit only when it is not 0 otherwise omit
        fmt.format(x).toString()
        // ####0.0########################
      }

      case NameExpNd(name: NameNd) => {
        buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + name.decl.get.fqn.toString() + "]" mkString

      }
      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => buildBoogieExp(x, buildFor) + " " + resBiOp(op) + " " + buildBoogieExp(y, buildFor)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => {
        if (resUnaOp(op) == "'")
          buildBoogieExp(x, buildFor) // Heap Switching
        else
          resUnaOp(op) + buildBoogieExp(x, buildFor)
      }

      case MemberExpNd(x: ExpNd, name: String) => buildBoogieExp(x, buildFor) + "." + name

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => {
        buildBoogieExp(operands(0), buildFor) + " " + resOpChain(ops(0)) + " " + buildBoogieExp(operands(1), buildFor) mkString
      }
      case FetchExpNd(x: ExpNd) => buildBoogieExp(x, buildFor)

      case AsExpNd(x: ExpNd, _) => buildBoogieExp(x, buildFor)

      case _ => exp.toString()
    }
    result
  }

  // Build Post Condition Expression - Switching Heap

  def buildPostCondExp(exp: ExpNd, buildFor: TransContext): String = {

    val result: String = exp match {

      case NoExpNd() => ""

      case BooleanLiteralExpNd(b) => b.toString()

      case IntLiteralExpNd(i: Long) => i.toString()

      case FloatLiteralExpNd(x: Double) => {

        val fmt = new java.text.DecimalFormat("####0.0########################")
        // 24 Decimal places
        // 0 : Digit will always be show otherwise 0 character placed'
        // # show digit only when it is not 0 otherwise omit
        fmt.format(x).toString()
        // ####0.0########################
      }

      case NameExpNd(name: NameNd) => {
        buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + name.decl.get.fqn.toString() + "]" mkString

      }
      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => buildBoogieExp(x, buildFor) + " " + resBiOp(op) + " " + buildBoogieExp(y, buildFor)

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => {
        if (resUnaOp(op) == "'"){
          val tempContext = buildFor
          buildFor.setHeap("Heap")
          buildBoogieExp(x, tempContext)
        }
        else
          resUnaOp(op) + buildBoogieExp(x, buildFor)
      }

      case MemberExpNd(x: ExpNd, name: String) => buildBoogieExp(x, buildFor) + "." + name

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => {
        buildBoogieExp(operands(0), buildFor) + " " + resOpChain(ops(0)) + " " + buildBoogieExp(operands(1), buildFor) mkString
      }
      case FetchExpNd(x: ExpNd) => buildBoogieExp(x, buildFor)

      case AsExpNd(x: ExpNd, _) => buildBoogieExp(x, buildFor)

      case _ => exp.toString()
    }
    result
  }
  
  // Get the expressions to assert the reading permissions

  def assertReadingPermission(exp: ExpNd, buildFor: TransContext): String = { // Permission Greater than '0.0'

    val nameList = new ArrayBuffer[String];
    val nameList_dirty = nameExpCodeGen(exp, nameList)
    val nameList$ = nameList_dirty.distinct
    var result = "";
    if (nameList$.nonEmpty) {
      result = "assert "
      for (name <- nameList$.dropRight(1)) {
        result = result.concat(buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + name + "] > 0.0" + " && ")
      }
      result = result.concat(buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + nameList.last + "] > 0.0 ;")
    }
    result
  }

  //Used for the defindness check to produce the aseertions on writing permission in expressions
  // Permission must be equal to 1.0;

  def buildWritingPerExp(exp: ExpNd, buildFor: TransContext): String = { 

    val result: String = exp match {

      case NoExpNd() => ""

      case IntLiteralExpNd(i: Long) => ""

      case FloatLiteralExpNd(x: Double) => ""

      case NameExpNd(name: NameNd) => {
        buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + name.decl.get.fqn.toString() + "] == 1.0 " mkString

      }
      case MemberExpNd(x: ExpNd, name: String) => buildWritingPerExp(x, buildFor) + "." + name

      case FetchExpNd(x: ExpNd) => buildWritingPerExp(x, buildFor)

      case AsExpNd(x: ExpNd, _) => buildWritingPerExp(x, buildFor)

      case _ => ""
    }
    result
  }
  
  def buildReadingPerExp(exp: ExpNd, buildFor: TransContext): String = { 

    val result: String = exp match {

      case NoExpNd() => ""

      case IntLiteralExpNd(i: Long) => "true"

      case FloatLiteralExpNd(x: Double) => "true"

      case NameExpNd(name: NameNd) => { 
        buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + name.decl.get.fqn.toString() + "] > 0.0 " mkString

      }
      
      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => {
        buildReadingPerExp(x, buildFor)  + "&&" + buildReadingPerExp(y, buildFor) mkString
      }

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => buildReadingPerExp(x, buildFor)
      
      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => {
        buildReadingPerExp(operands(0), buildFor) + "&&" +  buildReadingPerExp(operands(1), buildFor) mkString
      }
      
      case MemberExpNd(x: ExpNd, name: String) => buildReadingPerExp(x, buildFor) + "." + name

      case FetchExpNd(x: ExpNd) => buildReadingPerExp(x, buildFor)

      case AsExpNd(x: ExpNd, _) => buildReadingPerExp(x, buildFor)

      case _ => ""
    }
    result
  }



  
  
  def chainingOpTrans(op: ChainingOperator): String = {
    op match {
      case EqualOp => return "=="
      case NotEqualOp => return "!="
      case LessOp => return "<"
      case LessOrEqualOp => return "<="
      case GreaterOp => return ">"
      case GreaterOrEqualOp => return ">="
    }
  }

  def biOpTrans(op: BinaryOperator): String = {
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

  def resBiOp(op: BinaryOperator) = {
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
 def resUnaOp(op: UnaryOperator) = {
    val result = op match {
      case NegativeOp => "-"
      case NotOp => "!"
      case PrimeOp => "'"
    }
    result
  }
 def resOpChain(op: ChainingOperator): String = {
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