package boogieBackEnd
import frontEnd.AST
import frontEnd.AST._
import util.Format
import util.OutputBuilder;
import contracts.Contracts;

private class ClassCodeGen(val dlNd: DeclNd, var outputBuffer: OutputBuilder) {

  var transContext = new TransContext("Heap", "this")

  def classCodeGen(): OutputBuilder = {

    // Class identifier

    val className = dlNd.name;
    outputBuffer.newLine
    outputBuffer.put("const unique " + dlNd.name + ":className;")

    //Object Identifier

    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
      mem match {
        case ObjDeclNd(isGhost: Boolean, isConst: Boolean, acc: Access, ty: TypeNd, init: InitExpNd) => {
          //See FQN later dlNd.name is class name and mem.name is object name
          val objType: String = TypeCodeGen(ty)
          outputBuffer.newLine
          outputBuffer.put("const unique " + dlNd.name + "." + mem.name + ": Field " + objType + ";")
          outputBuffer.newLine
        }
        case _ => {}
      }

    // Constructor Procedure

    outputBuffer.newLine
    outputBuffer.put("procedure " + dlNd.name + ".constructor" + "(this:Ref)") // dlNd.name is class name
    outputBuffer.newLine
    outputBuffer.put("requires dtype(this) <: " + dlNd.name)
    outputBuffer.newLine
    outputBuffer.put("modifies Heap;")
    outputBuffer.newLine
    outputBuffer.put("modifies Permission;")
    outputBuffer.newLine
    outputBuffer.put("{")
    outputBuffer.newLine
    outputBuffer.put("var Permission : PermissionType where (forall <x> r: Ref, f: Field :: Permission[r,f] == 0.0)")
    outputBuffer.newLine
    outputBuffer.put("var oldHeap:HeapType;")
    outputBuffer.newLine
    outputBuffer.put("havoc Heap;")

    //Class Claim Code Generation

    classClaimCodeGen()

    //Objects initialization Code Generation, It Initializes the Heap

    objectsInitCodeGen()

    //Class Invariant Code

    classInvariantCodeGen()

    //Closing of Constructor Procedure
    outputBuffer.newLine
    outputBuffer.put("}")

    //Thread Procedure

    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
      mem match {
        case ThreadDeclNd(claimList: List[ClaimNd], block: CommandNd) => {
          outputBuffer.newLine
          outputBuffer.put("procedure " + dlNd.name + "." + mem.name + " (this: Ref)")
          outputBuffer.newLine
          outputBuffer.put("dtype(this) <: " + dlNd.name)
          outputBuffer.newLine
          outputBuffer.put("modifies heap;")
          outputBuffer.newLine
          outputBuffer.put("{")
          outputBuffer.newLine
          outputBuffer.put("var Permission: PermissionType where (forall<x> r: Ref, f: Field x :: Permission[r,f] == 0.0)")
          outputBuffer.newLine
          outputBuffer.put("var oldHeap, preHeap, tempHeap: HeapType;")
          outputBuffer.newLine

          for (claim <- claimList) { //Thread Claim Code Generation
            claimCodeGen(claim)
          }

          commandCodeGen(block) //Thread Body Code Generation
        }

        case _ => {}
      }
    outputBuffer
  }

  def commandCodeGen(cmd: CommandNd) {
    cmd match {
      case SkipCmdNd() => { //Do Nothing
      }

      case SeqCommandNd(fstCmd, sndCmd) => {
        commandCodeGen(fstCmd)
        commandCodeGen(sndCmd)
      }

      case LocalDeclCmdNd(decl) => {
        val objType: String = TypeCodeGen(decl.ty)
        val exp: String = new ExpCodeGen().getExpCode(decl.init)
        outputBuffer.newLine
        outputBuffer.put("Heap[this, " + decl.fqn + "] := " + exp + ";")
        outputBuffer.newLine
      }

      case AssignmentCmdNd(lhs, rhs) => {
        val obj = new ExpCodeGen()
        for (l_exp <- lhs.init) {
          outputBuffer.newLine
          outputBuffer.put(obj.getExpCode(l_exp))
          if(!(l_exp.equals(lhs.last))) // Not to put comma for last expression in sequence
          outputBuffer.put(",")
        }
        outputBuffer.put(obj.getExpCode(lhs.last))
        outputBuffer.put(" = ")

        for (r_exp <- rhs) {
          val exp: String = obj.getExpCode(r_exp)
          outputBuffer.put(exp)
          if(!(r_exp.equals(rhs.last))) // Not to put comma for last expression in sequence
          outputBuffer.put(",")
        }
        outputBuffer.put(obj.getExpCode(rhs.last))
        outputBuffer.put(";")
      }

      case CallCmdNd(method, argList) => {
      }

      case IfCmdNd(guard, thenCmd, elseCmd) => {
      }

      case WhileCmdNd(guard, lil, body) => {
        outputBuffer.newLine
        outputBuffer.put("while(" + "true" + ")") // build guard expression
        outputBuffer.newLine
        outputBuffer.put("{")
        commandCodeGen(body)
        outputBuffer.newLine
        outputBuffer.put("}")
      }

      case ForCmdNd(decl, repetitions, lil, body) => {
      }

      case CoForCmdNd(decl, repetitions, claimList, body) => {
      }

      case CoCmdNd(claimList, fstCmd, sndCmd) => {
      }

      case AcceptCmdNd(methodImplementationList) => {
        for (mi <- methodImplementationList) {
          outputBuffer.newLine
          outputBuffer.put("goto " + mi.nameNd + ";")
          outputBuffer.newLine
          outputBuffer.put(mi.nameNd + ":")
          val methDecl = for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
            mem match {
              case MethodDeclNd(acc, paramList, preCndList, postCndList, givesPerList, takesPerList, borrowsPerList) => {
                if (mem.name.toString.equals(mi.nameNd.toString)) {
                  methTakesPerCodeGen(acc, mem.name, paramList, takesPerList)
                  methPreCondCodeGen(acc, mem.name, paramList, preCndList)
                  methodImplementationCodeGen(mi)
                  methPostCondCodeGen(acc, mem.name, paramList, postCndList)
                  methGivesPerCodeGen(acc, mem.name, paramList, givesPerList)
                }

              }
              case _ => "" // Check TypeChecker line 279-280
            }
        }
      }

      case WithCmdNd(lock, tpl, guard, command, gpl) => {

        //guard translation
        outputBuffer.newLine;
        outputBuffer.put("var that : Ref;")
        outputBuffer.newLine
        outputBuffer.put("that := "); build(lock, transContext)
        // takes permission list
        for (tp <- tpl)
          for ((lsn, amnt) <- tp.pmn.pm)
            lsn match {
              case ObjectIdLSN(nameExp) => {
                outputBuffer.newLine
                outputBuffer.put("oldPermission := Permission;")
                outputBuffer.newLine
                outputBuffer.put("if(Permission[this," + lsn.getName() + "] == 0.0")
                outputBuffer.newLine
                outputBuffer.put("{")
                outputBuffer.newLine
                outputBuffer.put("havoc Heap_temp")
                outputBuffer.newLine
                outputBuffer.put("Heap[this," + lsn.getName() + "];")
                outputBuffer.newLine
                outputBuffer.put("Permission[this," + lsn.getName() + "] := " + "Permission[this," + lsn.getName() + "] + " + amnt + ";")
              }
              case _ => contracts.Contracts.toDo("Array Location Set Node")
            }
        commandCodeGen(command)

        //gives permission list

      }

      case AssertCmdNd(assertion) => {
        val expCode = new ExpCodeGen().getExpCode(assertion)
        val parts = expCode.split(" ")
        val per = "\nassert Permission[this," + parts(0) + "] > 0.0 ;"
        val assertCmdCode = per + "\nassert Heap[this," + parts(0) + "] " + parts(1) + ";"
        assertCmdCode
      }

      case AssumeCmdNd(assumption) => {
        val expCode = new ExpCodeGen().getExpCode(assumption)
        val parts = expCode.split(" ")
        val per = "\nassume Permission[this," + parts(0) + "] > 0.0 ;"
        val assertCmdCode = per + "\nassume Heap[this," + parts(0) + "] " + parts(1) + ";"
        assertCmdCode
      }

      case _ => {
        ""
      }
    }
  }

  // Method Implementation inside Thread, Code Generation
  def methodImplementationCodeGen(mi: MethodImplementationDeclNd) {
    outputBuffer.put("//Method Implementatiom")
    //Figure out the need of parameter list
    commandCodeGen(mi.fstCmd)
    commandCodeGen(mi.sndCmd)
  }

  //Method's 'pre' Condition Code Generation
  def methPreCondCodeGen(acc: Access, name: String, params: List[ParamDeclNd], preCnds: List[PreCndNd]) {
    outputBuffer.newLine
    outputBuffer.put("//Pre Condition(s)")
    outputBuffer.newLine
    outputBuffer.put("oldHeap := Heap;")
    outputBuffer.newLine
    outputBuffer.put("havoc Heap")
    for (prc <- preCnds) {
      val exp = prc.condition;
      transContext.set("Heap", "this")
      outputBuffer.newLine
      outputBuffer.put("assume " + build(prc.condition, transContext)) // assume and assert
    }
  }
  
  //Method's 'takes' Specification Code Generation
  def methTakesPerCodeGen(acc: Access, name: String, params: List[ParamDeclNd], takesPers: List[TakesPerNd]) {
    outputBuffer.newLine
    outputBuffer.put("//Taking Permission(s)\n")
    for (tp <- takesPers)
      for ((lsn, expNd) <- tp.pmn.pm)
        lsn match {
          case ObjectIdLSN(nameExp) => {
            val amount: String = new ExpCodeGen().getExpCode(expNd)
            outputBuffer.newLine
            outputBuffer.put("oldPermission := Permission;")
            outputBuffer.newLine
            outputBuffer.put("if(Permission[this," + lsn.getName() + "] == 0.0")
            outputBuffer.newLine
            outputBuffer.put("{")
            outputBuffer.newLine
            outputBuffer.put("havoc Heap_temp")
            outputBuffer.newLine
            outputBuffer.put("Heap[this," + lsn.getName() + "];")
            outputBuffer.newLine
            outputBuffer.put("Permission[this," + lsn.getName() + "] := " + "Permission[this," + amount + ";")
            outputBuffer.newLine
            outputBuffer.put("}")
          }
          case _ => contracts.Contracts.toDo("Array Location Set Node")
        }
  }

  //Method's 'post' Condition Code Generation
  def methPostCondCodeGen(acc: Access, name: String, params: List[ParamDeclNd], postCnds: List[PostCndNd]) {
    outputBuffer.newLine
    outputBuffer.put("//Post Condition(s)\n")
    outputBuffer.newLine
    for (poc <- postCnds) {
      val exp = poc.condition;
      transContext.set("Heap", "this")
      outputBuffer.newLine
      outputBuffer.put("assert " + build(poc.condition, transContext)) // assume and assert
    }
  }
  
  //Method's 'gives' Specification Code Generation
  def methGivesPerCodeGen(acc: Access, name: String, params: List[ParamDeclNd], givesPers: List[GivesPerNd]) {
    outputBuffer.newLine
    outputBuffer.put("//Giving Permissions(s)\n")
    for (tp <- givesPers)
      for ((lsn, amnt) <- tp.pmn.pm)
        lsn match {
          case ObjectIdLSN(nameExp) => {
            transContext.set("Permission", "this")
            outputBuffer.newLine
            outputBuffer.put("assert "); build(nameExp, transContext); outputBuffer.put(" := "); build(nameExp, transContext); outputBuffer.put(" - "); outputBuffer.put(amnt + ";");
          }
          case _ => contracts.Contracts.toDo("Array Location Set Node")
        }
  }

  //Class Claim Code Generation
  
  def classClaimCodeGen() {
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ClaimNd(pmn) =>
          val perm_pairs = pmn.pm;
          var result = "";
          for ((loc, amount) <- perm_pairs) {
            var name = "";
            loc match {
              case ObjectIdLSN(nameExp) => {
                name = dlNd.name + "." + nameExp.name.toString();
              }
              case _ => contracts.Contracts.toDo("Location Set Node with Array")
            }
            var amnt: String = "";
            amount match {
              case IntLiteralExpNd(i: Long) => amnt = i.toString()
              case FloatLiteralExpNd(x: Double) => amnt = x.toString()
              case _ => contracts.Contracts.toDo("Permission Type Left")
            }
            outputBuffer.newLine
            outputBuffer.put("//Claim")
            outputBuffer.newLine
            outputBuffer.put("Permission[this," + name + "] := Permission[this," +name+ "] + "  + amnt + ";")
          }
        case _ =>
      }
    }
    outputBuffer.newLine
    outputBuffer.put("assert(forall<x> r: Ref, f: Field x :: Permission[r,f] <= 1.0);")
  }

  //Thread Claim Code Generation
  
  def claimCodeGen(claimNd: ClaimNd) {
    val perm_pairs = claimNd.pmn.pm;
    var result = "";
    for ((loc, amount) <- perm_pairs) {
      var name = "";
      loc match {
        case ObjectIdLSN(nen) => {
          name = dlNd.name + "." + nen.name.toString();
        }
        case _ => contracts.Contracts.toDo("Location Set Node with Array")
      }
      var amnt: String = "";
      amount match {
        case IntLiteralExpNd(i: Long) => amnt = i.toString()
        case FloatLiteralExpNd(x: Double) => amnt = x.toString()
        case _ => contracts.Contracts.toDo("Permission Type Left")
      }
      outputBuffer.newLine
      outputBuffer.put("//Claim")
      outputBuffer.newLine
      outputBuffer.put("Permission[this," + name + "] := Permission[this," + name + "] + " + amnt + ";")

    }
  }

  def classInvariantCodeGen() {
    outputBuffer.newLine
    outputBuffer.put("//Class Invariant")
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ClassInvNd(expNd) => {
          expNd match {
            case CanReadOp(loc) => {
              outputBuffer.newLine
              outputBuffer.setError("Don't have enough permission, permission is \'0\' in all cases when it's unable to read a value", mem.coord)
              outputBuffer.put("assert Permission[this," + dlNd.name + "." + loc.getName().toString() + "] > 0.0;");
              outputBuffer.newLine
              outputBuffer.clearError
            }
            case CanWriteOp(loc) => {
              outputBuffer.newLine
              outputBuffer.setError("Don't have anough permission to write, permission must be greater of equal to \'1\' to perform write", mem.coord)
              outputBuffer.put("assert Permission[this," + dlNd.name + "." + loc.getName().toString() + "] == 1.0;");
              outputBuffer.newLine
              outputBuffer.clearError
            }
            case PermissionOp(loc) => contracts.Contracts.toDo("Permission Operator: To get amount of permission")
            case _ => {
              transContext.set("Permission", "this")
              outputBuffer.newLine
              outputBuffer.setError("Bad Invariant Expression", mem.coord)
              outputBuffer.put("assert " + build(expNd, transContext)); // build -> object
              outputBuffer.newLine
              outputBuffer.clearError
            }
          }
        }
        case _ => "" // Unreachable not working, will fix it later
      }
    }
  }

  def build(expNd: ExpNd, buildFor: TransContext): String = {
    //expNd, parameter(which heap to use) H1,H2 , Structure combining array heap, permission map, -> array permission, field permission, this translating in context of which object
    // process inv wrt object(locked)
    // Output Builder

    val result: String = expNd match {

      case NoExpNd() => ""

      case IntLiteralExpNd(i: Long) => i.toString()

      case FloatLiteralExpNd(x: Double) => x.toString()

      case NameExpNd(name: NameNd) => {
        buildFor.getHeap() + "[" + buildFor.getObjRef() + "," + dlNd.name +"."+name.qn.toString() + "]"
      }
      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => List(build(x, buildFor), " ", resBiOp(op), " ", build(y, buildFor)) mkString

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + build(x, buildFor)

      case MemberExpNd(x: ExpNd, name: String) => build(x, buildFor) + "." + name

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => {
        var chainExp = ""
        for (op <- ops)
          chainExp += (build(operands(ops.indexOf(op)), buildFor) + chainingOpTrans(op) + build(operands(ops.indexOf(op) + 1), buildFor))
        chainExp;
      }
      case FetchExpNd(x: ExpNd) => build(x, buildFor)

      case AsExpNd(x: ExpNd, _) => build(x, buildFor) // TODO insert type conversions?

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

  private def objectsInitCodeGen() {
    var objDecl, objInits = ""
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ObjDeclNd(isGhost: Boolean, isConst: Boolean, acc: Access, ty: TypeNd, init: InitExpNd) => {
          objDecl = getObjDeclCode(isConst, acc, ty, dlNd.name, mem.name)
          val objName = dlNd.name + "." + mem.name
          val objType: String = TypeCodeGen(ty)
          val exp: String = new ExpCodeGen().getExpCode(init, objName.toString())
          outputBuffer.newLine
          outputBuffer.put("Heap[this, " + objName + "] := " + exp + ";") 
          // TODO When it will be that? 
          // building the expression not required for this subroutine, This subroutine contains the initialization of objects
          outputBuffer.newLine
        }
        case _ => ""
      }
    }
  }
  private def getObjDeclCode(isConst: Boolean, acc: Access, ty: TypeNd, className: String, objName: String): String = {
    val objQN = className + "." + objName
    val objType: String = TypeCodeGen(ty)
    val objCode = "\nconst unique " + objQN + ":" + "Field " + objType + ";"
    return objCode
  }

}