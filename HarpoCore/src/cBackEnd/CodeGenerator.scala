package cBackEnd
import frontEnd.AST._
import java.io.OutputStreamWriter
import CFG._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import cBackEnd.CFG.CFGNd
import contracts.Contracts
import checker.CheckerTypes._
class CodeGenerator {

  //generate code for statements
  var initCode : String = ""
  var methCode : String = ""
  var usedNds = List[CFGNd]()
  var labelMap = new HashMap[CFGNd, String]
  //stores the methCfgNd when funcType==1
  var methCfgNd = new MethImpCfgNd(null, null, null, null)
  //funcType:  0)normal func 1)start of method 2)start of merge 3)Co 4)with 5)call
  var className = ""
  var funcType = 0

  def getCode():String = initCode + "\n" + methCode
  def runGenerateS(curNd: CFGNd) = {

    var lm = new LabelManager
    usedNds = List[CFGNd]()
    generateS(curNd, lm.newLabel, lm)

    methCode += "}\n\n"

  }
  def generateS(curNd: CFGNd, curLabel: String, lm: LabelManager): String = {
    // var curLabel = lm.newLabel4
    val NM = NameManager
    curNd match {
      case StartCfgNd(func_name : String, class_name : String, funcType : Int) =>
        val mFunc = NM.mkName(func_name)
        val mClsName = NM.mkName(class_name)
        this.className = class_name
        this.funcType = funcType
        initCode += "void " + mFunc + "(Continuation *k);\n"
        methCode += "void " + mFunc + "(Continuation *k){" + "\n" + mClsName + " *context = (" + mClsName + "*) k->context ;\n"
        
        if(funcType == 2)
        {
          methCode += "  unlock( & context->procLock ) ;\n"
          methCode += "k->fp = 0;\n"
          methCode += "return;"
        }

      //   case MethImpCfgNd(name: String, paramList: List[ParamDeclNd], guard: ExpNd, methodImplementationList: List[MethodImplementationDeclNd]) =>
      //     this.methCfgNd = curNd.asInstanceOf[MethImpCfgNd]
      //   case StartOfMeth(methCfgNd: MethImpCfgNd) =>
      case _ =>
        val mLabel = curLabel
        methCode += mLabel + ": ;\n"

        if (!usedNds.exists(s => s eq curNd)) {
          usedNds = usedNds ::: List(curNd)
          labelMap += curNd -> mLabel
        } else {
          methCode += "goto " + labelMap.get(curNd).get + ";\n"
          return labelMap.get(curNd).get
        }

    }

    curNd match {
      //   case StartCfgNd(func_name: String,class_name: String,funcType:Int) =>
      case AcceptCfgNd(methodImplementationList: List[MethodImplementationDeclNd]) =>
        var hasGuard = false
        var guardNames = List[String]()
        for (mthImpNd : MethodImplementationDeclNd <- methodImplementationList) {
          if (!mthImpNd.guard.isInstanceOf[NoExpNd]) {
            hasGuard = true
            val mMthImpName = NM.mkName(mthImpNd.name)
            guardNames = guardNames ::: List(mMthImpName)
            methCode += "bool " + mMthImpName + " = " + generateE(mthImpNd.guard) + ";\n"
          }
          //mthImpNd1
        }
        
        if (hasGuard) {
          methCode += "if ((!" + guardNames(0) + ")"
  
          for (gName: String <- guardNames: List[String]) {
            if (gName != guardNames(0)) {
              methCode += "&& (!" + gName + ")"
            }
          }
          methCode+=")\n";
          methCode+="{ // Report this thread is stuck. \n}\n"
        }

        methCode+=" lock( & context->procLock );\n"
        for (mthImpNd : MethodImplementationDeclNd <- methodImplementationList) {
          methCode += "if ("
          if (hasGuard) methCode += guardNames(methodImplementationList.indexOf(mthImpNd)) + "&&"
          methCode += "context->" + NM.mkName(mthImpNd.name) + ".ps.state == 1 ) {\n"
          methCode += "k->fp = &" + NM.mkName(curNd.getNext(methodImplementationList.indexOf(mthImpNd)).asInstanceOf[ContinuationCfgNd].name) + ";\n"
          methCode += "return;\n}\n"
        }
        //        context->fetch.ps.next.fp = &ServerFunc1Fetch ;
        //        context->fetch.ps.next.context = context ;    }
        for (mthImpNd : MethodImplementationDeclNd <- methodImplementationList) {
          if (hasGuard) methCode+="if (" + guardNames(methodImplementationList.indexOf(mthImpNd)) + ") {\n"
          methCode+="context->" + NM.mkName(mthImpNd.name) + ".ps.next.fp = &" + NM.mkName(curNd.getNext(methodImplementationList.indexOf(mthImpNd)).asInstanceOf[ContinuationCfgNd].name) + " ;\n"
          methCode+="context->" + NM.mkName(mthImpNd.name) + ".ps.next.context = context ;\n"
          if (hasGuard) methCode += "}\n"
        }

        methCode+="unlock( & context->procLock ) ;\n"
        methCode+="k->fp = 0 ;\n"
        methCode+="return ;\n"

      //  case MethImpCfgNd(name: String, paramList: List[ParamDeclNd], guard: ExpNd) =>
      //        println("call " + name + ";")
      //        println(generateE(guard))
      //        generateS(curNd.edgeList(0), lm.newLabel, lm)
      case StartOfMeth(methCfgNd: MethImpCfgNd) =>
        val nextLabel = lm.newLabel
        this.methCfgNd = methCfgNd
        
        for (mthImpNd : MethodImplementationDeclNd <- methCfgNd.methodImplementationList) {
          if (!mthImpNd.name.equals(methCfgNd.name)) methCode+="context->" + NM.mkName(mthImpNd.name) + ".ps.next.fp = 0;\n"
        }
        methCode += "context->" + NM.mkName(methCfgNd.name) + ".ps.state = 2;\n"
        
        if (methCfgNd.name.equals("output")) {
          val debugParam = methCfgNd.paramList(0)
          var debugType : checker.CheckerTypes.Type = null
          debugParam.ty.tipe match {
            case Some(tipe) =>
              if (tipe.isInstanceOf[LocationType]) debugType = tipe.asInstanceOf[LocationType].base
              else debugType = tipe
              if (debugType == bool) {
                methCode += "char* s = NULL;\n"
                methCode += "if (context->" + NM.mkName(methCfgNd.name) + "." + NM.mkName(debugParam.name) + " == 1) s = \"true\";\n"
                methCode += "else if (context->" + NM.mkName(methCfgNd.name) + "." + NM.mkName(debugParam.name) + " == 0) s = \"false\";\n"
                methCode += "if (s != NULL) printf(\"%s\\n\", s);\n"
                methCode += "fflush(stdout);\n"
              }
              else {
                methCode += "printf(\"%"
                if (debugType == int8 || debugType == int16 || debugType == int32 || debugType == int64) {
                  methCode += "i\\n\""
                } else if (debugType == real16 || debugType == real32 || debugType == real64) {
                  methCode += "f\\n\""
                }
                methCode += ", context->" + NM.mkName(methCfgNd.name) + "." + NM.mkName(debugParam.name) + ");\n"
                methCode += "fflush(stdout);\n"
              }
            case None => {}
          }
        }
        
        methCode += "unlock( & context->procLock ) ;\n"
        if (!curNd.edgeList.isEmpty) {
          methCode += "goto " + nextLabel + ";\n"
          generateS(curNd.edgeList(0), nextLabel, lm)
        }
        methCode += "k->fp = 0;\n"
        methCode += "return;\n"
      case TryCallCfgNd(recipient: ExpNd, argList: List[ExpNd]) =>
        //assert curNd.getNext(0) match ContinuationCfgNd

        //            Sink_Deposit *procP = context->sink->deposit ; ???????????????
        methCode+="//create procP here!!!!\n"
        
        var recipientName = ""
        var rhs = ""
        recipient match {
          case NameExpNd(name : NameNd) => 
            recipientName = name.toString()
            rhs = "&(context->" + NM.mkName(recipientName) + ")"
          case MemberExpNd(x : ExpNd, mName : String) =>
            recipientName = mName
            x match {
              case NameExpNd(cName : NameNd) =>
                rhs = "&(&context->" + NM.mkName(cName.toString()) + ")->" + NM.mkName(mName)
              case MemberExpNd(x : ExpNd, name : String) =>
                Contracts.unreachable("HANDLE THE MEMBEREXPND")
              case _ =>
                Contracts.unreachable("Invalid type of ExpNd.")
            }
          case _ => Contracts.unreachable("Recipient is a " + recipient)
        }
        val params = NM.getParamMap(recipientName)
        val pName = NM.mkName(params._1 + "_" + recipientName)
        
        methCode += pName + " *procP = " + rhs + ";\n"
        //  println(generateE(recipient.asInstanceOf[MemberExpNd].x) + "!@#!@#!@#")
        methCode+="lock( procP->ps.procLockP ) ;\n"
        methCode+="if(  procP->ps.state == 0 ) {\n"
        methCode+="k->fp = &" + NM.mkName(curNd.getNext(0).asInstanceOf[ContinuationCfgNd].name) + " ;\n"
        methCode+="return;\n"
        methCode+="} else {\n"
        methCode+="Continuation cont ;\n"
        methCode+="cont.context = k->context ;\n"
        methCode+="cont.fp = &" + NM.mkName(curNd.getNext(0).asInstanceOf[ContinuationCfgNd].name) + " ;\n"
        methCode+="Enqueue( &cont, & procP->ps.clientQueue) ;\n"
        methCode+="unlock( procP->ps.procLockP ) ;\n"
        methCode+=" k->fp = 0 ;\n"
        methCode+="return;\n}\n"

      case CallCfgNd(recipient: ExpNd, argList: List[ExpNd]) =>

        //    Sink_Deposit *procP = context->sink->deposit ;
        //    // Precondition. The lock is locked. The procedure is in state 0.
        //    // So this client is a winner.
        //    
        //    // Code to compute the argument expression E goes here
        //    procP->x = /*the value just computed.*/0;
        val nextLabel = lm.newLabel
        methCode += "//create procP here!!!!\n"
        
        var recipientName = ""
        var rhs = ""
        recipient match {
          case NameExpNd(name : NameNd) => 
            recipientName = name.toString()
            rhs = "&(context->" + NM.mkName(recipientName) + ")"
          case MemberExpNd(x : ExpNd, mName : String) =>
            recipientName = mName
            x match {
              case NameExpNd(cName : NameNd) =>
                rhs = "&(&context->" + NM.mkName(cName.toString()) + ")->" + NM.mkName(mName)
              case MemberExpNd(x : ExpNd, name : String) =>
                Contracts.unreachable("HANDLE THE MEMBEREXPND")
              case _ =>
                Contracts.unreachable("Invalid type of ExpNd.")
            }
          case _ => Contracts.unreachable("Recipient is a " + recipient)
        }
        val params = NM.getParamMap(recipientName)
        val pName = NM.mkName(params._1 + "_" + recipientName)

        methCode += pName + " *procP = " + rhs + ";\n"
        
        if (!argList.isEmpty) {
          for (i <- 0 until argList.length) {
        	  var rhs = ""
            params._2(i).paramCategory match {
              case InParamCategory =>
                rhs = generateE(argList(i))
              case OutParamCategory =>
                rhs = "&(" + generateE(argList(i)) + ")"
              case _ =>
                Contracts.unreachable("ObjParamCategory is invalid.")
                
            }
            methCode += "procP->" + NM.mkName(params._2(i).name.toString) + " = " + rhs + ";\n"
          }
        }
        
        generateS(curNd.edgeList(0), nextLabel, lm)
      case IfCfgNd(exp: ExpNd) =>
        val trueLabel = lm.newLabel
        val falseLabel = lm.newLabel
        methCode+="if (" + generateE(exp) + ")" + " goto " + trueLabel + ";\n"
        methCode+="else goto " + falseLabel + ";\n"
        if (curNd.edgeList.length > 0)
          generateS(curNd.edgeList(0), trueLabel, lm)
        if (curNd.edgeList.length > 1)
          generateS(curNd.edgeList(1), falseLabel, lm)
      case AssignmentCfgNd(lhs, rhs) =>
        var i = 0
        val nextLabel = lm.newLabel
        //assert lhs.length == rhs.length
        while (i < lhs.length) {
            methCode += generateE(lhs(i)) + "=" + generateE(rhs(i)) + ";\n"
          i = i + 1
        }
        if (!curNd.edgeList.isEmpty) {
          methCode += "goto " + nextLabel + ";\n"
          generateS(curNd.edgeList(0), nextLabel, lm)
        }
      case WhileCfgNd(guard) =>
        val trueLabel = lm.newLabel
        val falseLabel = lm.newLabel
        methCode+="if (" + generateE(guard) + ")" + " goto " + trueLabel + ";\n"
        methCode+="else goto " + falseLabel + ";\n"
        if (curNd.edgeList.length > 0)
          generateS(curNd.edgeList(0), trueLabel, lm)
        if (curNd.edgeList.length > 1)
          generateS(curNd.edgeList(1), falseLabel, lm)
      case ContinuationCfgNd(name: String) =>
        // Normal Function
        if (funcType == 0) {
          methCode+="k->fp = 0 ;"
          methCode+="return;"
        }
        // Start of Method
        else if (funcType == 1) {
          //  methCfgNd.paramList
          /*if (!methCfgNd.paramList.isEmpty)
            for (param: ParamDeclNd <- methCfgNd.paramList: List[ParamDeclNd]) {
              param.paramCategory match {
                case OutParamCategory =>
                  methCode += "context->" + NM.mkName(methCfgNd.name) + "." + NM.mkName(param.name) + " = context->" + NM.mkName(methCfgNd.name) + "." + NM.mkName(param.name) + ";\n"
                case _ =>
              }
            }*/
          methCode+="lock( & context->procLock ) ;\n"
          methCode+="PutContinuationOnHeap( & context->" + NM.mkName(methCfgNd.name) + ".ps.next ) ;\n"
          methCode+="context->" + NM.mkName(methCfgNd.name) + ".ps.state = 0 ;\n"
          methCode+="context->" + NM.mkName(methCfgNd.name) + ".ps.next.fp = 0;\n"
          methCode+="if( ! isEmpty( & context->" + NM.mkName(methCfgNd.name) + ".ps.clientQueue ) ) {\n"
          methCode+="context->" + NM.mkName(methCfgNd.name) + ".ps.next.fp = & " + NM.mkName(name) + ";\n"
          methCode+="context->" + NM.mkName(methCfgNd.name) + ".ps.next.context = k->context;\n"
          methCode+=" Dequeue(k, & context->" + NM.mkName(methCfgNd.name) + ".ps.clientQueue ) ;\n"
          methCode+="return;\n"
          methCode+="} else {\n"
          methCode+="k->fp = & " + NM.mkName(name) + " ;\n"
          methCode+="return;\n}\n"
        }
        // Start of merge
        else if (funcType == 2) {
          
        }
        // Co function
        else if (funcType == 3) {
          methCode += "bool stillRunning = count(" + "&context->" + NM.mkCounterName(className) + ");\n"
          methCode += "if (stillRunning) {\n"
          methCode += "k->fp = 0;\n"
          methCode += "return;\n"
          methCode += "} else {\n"
          methCode += "k->fp = &" + NM.mkName(name) + ";\n"
          methCode += "k->context = context;\n"
          methCode += "return;\n"
          methCode += "}\n"
        }
        // With function
        else if (funcType == 4) {
          
        }
        // Method call
        else if (funcType == 5) {

          methCode+="procP->ps.state = 1 ;\n"
          methCode+=" Continuation temp = procP->ps.next ;\n"
          methCode+=" procP->ps.next.context = k->context ;\n"
          methCode+="procP->ps.next.fp = &" + NM.mkName(name) + ";\n"
          methCode+="if( temp.fp == 0 ) {\n"
          methCode+=" unlock( procP->ps.procLockP ) ;\n"
          methCode+=" k->fp = 0 ;\n"
          methCode+="return;\n}\n"
          methCode+="else {\n"
          methCode+="*k = temp ;}\n"
        }
      case LocalDeclCfgNd(decl: LocalDeclNd) =>
        methCode += "context->" + NM.mkLocalObjName(decl.name) + " = " + generateE(decl.init) + ";\n"
        
        val nextLabel = lm.newLabel
        
        if (!curNd.edgeList.isEmpty) {
          methCode += "goto " + nextLabel + ";\n"
          generateS(curNd.edgeList(0), nextLabel, lm)
        }
      case _ =>
        val nextLabel = lm.newLabel

        if (!curNd.edgeList.isEmpty) {
          methCode+="goto " + nextLabel + ";\n"
          generateS(curNd.edgeList(0), nextLabel, lm)
        }
    }
    return curLabel
  }

  //generate code for expressions
  def generateE(curNd: ExpNd): String = curNd match {
    case IntLiteralExpNd(i: Long) =>
      //output i.toString()
      return i.toString()
    case FloatLiteralExpNd(x : Double) =>
      return x.toString()
    case NameExpNd(name: NameNd) =>
      //output name
      if (name.toString.equals("true") || name.toString.equals("false")) return name.toString()
      // TODO: make sure this doesn't break other concurrent code gen cases of named expressions
      val decl = name.decl.get
      
      var parentCode = ""
      decl.parent match {
        case Some(parDecl) =>
          if (!className.equals(parDecl.name) && parDecl.isInstanceOf[ClassLikeDeclNd]) parentCode = NameManager.mkSuperName(parDecl.name) + "."
        case None => {}
      }
            
      decl match {
        case LocalDeclNd( isConst : Boolean, ty : TypeNd, init : ExpNd, cmd : CommandNd) =>
          return "context->" + NameManager.mkLocalObjName(name.toString)
        case ParamDeclNd( ty : TypeNd, paramCategory : ParamCategory) =>
          decl.asInstanceOf[ParamDeclNd].paramCategory match {
            case InParamCategory =>
              return "context->" + parentCode + NameManager.mkName(NameManager.getMethodNameByParam(name.toString)) + "." + NameManager.mkName(name.toString)
            case OutParamCategory =>
              return "*(context->" + parentCode + NameManager.mkName(NameManager.getMethodNameByParam(name.toString)) + "." + NameManager.mkName(name.toString) + ")"
            case ObjParamCategory =>
              Contracts.unreachable("ObjParamCategory is invalid.")  
          }
        case _ => return "context->" + parentCode + NameManager.mkName(name.toString)       
      }
      
    case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) =>
      op match {
        // !x\/y
        case ImpliesOp =>
          return "(" + "!" + generateE(x) + "||" + generateE(y) + ")"
        case EquivOp =>
          //**************************
          return "((!" + generateE(x) + "||" + generateE(y) + ") && (!" + generateE(y) + "||" + generateE(x) + "))" 
          //return generateE(x) + "<=>" + generateE(y)
        case IndexOp =>
          return "pow(" + generateE(x) + "," + generateE(y) + ")"
        case _ =>
          return "(" + generateE(x) + biOpTrans(op) + generateE(y) + ")"

      }
    case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) =>
      op match {
        case NegativeOp =>
          return "-" + generateE(x)
        case NotOp =>
          return "!" + generateE(x)
      }
    case MemberExpNd(x: ExpNd, name: String) =>
      //***************************
      return generateE(x) + "." + NameManager.mkName(name)
    case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) =>
      //assert operands.length == ops.length + 1
      var i = 0
      var s = ""
      while (i < ops.length) {
        s = s + "(" + generateE(operands(i)) + relOpTrans(ops(i)) + generateE(operands(i + 1))
        i = i + 1
        if (i < ops.length)
          s = s + ")&&"
        else
          s = s + ")"
      }

      // for the case where with empty ops
      if (i == 0)
        s = generateE(operands(i))
      return s
    case FetchExpNd(x: ExpNd) =>
      return generateE(x)
    case AsExpNd(x: ExpNd, ty: TypeNd) =>
      return generateE(x)
    case _ =>
      return ""

  }

  // generate code for the Class Declarations
  def generateC(curNd: DeclNd) = curNd match {
    case _ => //println()
  }

  def relOpTrans(op: ChainingOperator): String =
    {
      op match {
        case EqualOp => return "=="
        case NotEqualOp => return "!="
        case LessOp => return "<"
        case LessOrEqualOp => return "<="
        case GreaterOp => return ">"
        case GreaterOrEqualOp => return ">="
      }
    }
  def biOpTrans(op: BinaryOperator): String =
    {
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
}