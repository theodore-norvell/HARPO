package boogieBackEnd
import frontEnd.AST
import frontEnd.AST._
import util.Format
import util.OutputBuilder;

private class ClassCodeGen(val dlNd: DeclNd, var outputBuffer: OutputBuilder) {
  
  var transContext = new TransContext("Heap", "this")

  def getClassCode(): String = {

    var ClassCode = ""

    var objInits = "";

    val className = dlNd.name;
    outputBuffer.newLine
    outputBuffer.setError("Bad Class Declaration", dlNd.coord)
    outputBuffer.put("const unique " + dlNd.name + ":className;")
    outputBuffer.newLine
    outputBuffer.clearError

    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ObjDeclNd(isGhost: Boolean, isConst: Boolean, acc: Access, ty: TypeNd, init: InitExpNd) => {
          //See FQN later dlNd.name is class name and mem.name is object name
          val objType: String = TypeCodeGen(ty)
          outputBuffer.newLine
          outputBuffer.put("const unique" + dlNd.name + "." + mem.name + ": Field" + objType + ";")
          outputBuffer.newLine
        }
        case _ => ""
      }
    }
    outputBuffer.newLine
    outputBuffer.put("procedure" + dlNd.name + ".constructor" + "(this:Ref)") // dlNd.name is class name
    outputBuffer.newLine
    outputBuffer.put("requires dtype(this) <:" + dlNd.name + ".constructor")
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
    //Generate Class Claim Code
    genClassClaim()
    //Generate Objects initialization Code
    genObjectsInit()
    //Generate Invariant Code
    genClassInvariant()
    outputBuffer.newLine
    outputBuffer.put("}")
    
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case MethodDeclNd(acc, paramList, preCndList, postCndList, givesPerList, takesPerList, borrowsPerList) => {
         
      var takes = ""
      var pre = ""
      var body = ""
      var post = ""
      var gives = ""

      //takes
      for (tp <- takesPerList) {
        for ((lsn , amnt) <- tp.pmn.pm) {
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
          return ""
        }
      }  
          outputBuffer.newLine
          outputBuffer.put("\\Method Declaration")
          outputBuffer.newLine
          outputBuffer.put("oldPermission := Permission;")
          /* Takes Specification Clause*/
          outputBuffer.newLine
          //outputBuffer.put("if(Permission[this," + + ")")
          outputBuffer.newLine
                   
          /* PreCondition */
          
          outputBuffer.newLine         
          
          /* Thread Body */
          outputBuffer.newLine
          
          /* Post Condition */
          outputBuffer.newLine
          
          /* Gives Specification Clause */
          outputBuffer.newLine   
          
        }

        case ThreadDeclNd(claimList: List[ClaimNd], block: CommandNd) => {
          
          outputBuffer.newLine
          outputBuffer.setError("",mem.coord)
          outputBuffer.put("procedure" + dlNd.name + "." + mem.name + "(this: Ref)")
          outputBuffer.newLine
          outputBuffer.put("dtype(this) <: "+ mem.name)
          outputBuffer.newLine
          outputBuffer.put("modifies heap")
          outputBuffer.clearError
          outputBuffer.newLine
          outputBuffer.put("{")
          outputBuffer.newLine
          outputBuffer.put("var Permission: PermissionType where (forall<x> r: Ref, f: Field x :: Permission[r,f] == 0.0)")
          outputBuffer.newLine
          outputBuffer.put("var oldHeap, preHeap, tempHeap: HeapType;")
          outputBuffer.newLine
          outputBuffer.put("var that: Ref")
          outputBuffer.newLine
          outputBuffer.put("that := this;")
          outputBuffer.newLine
          outputBuffer.put("while(true)")
          outputBuffer.newLine
          outputBuffer.put("{")
          outputBuffer.newLine
          outputBuffer.put("goto increment;") // hard coded goto label, decide which method to go on
          outputBuffer.newLine
          outputBuffer.put("increment:")
          
          /* Thread Claim */
          val thr = new ThreadCodeGen(mem);
          val ThrCode = thr.getThreadCode(dlNd.name, claimList, block)
          val threadName = mem.name.replace("#", "")
          val thrDecl = "\nprocedure " + dlNd.name + "." + threadName + "(this:Ref)"
          val thrClaim = "\nmodifies Heap;" // {add claim}
          val thrStaticBody = "\nvar Permission : PermissionType where (forall <a> r:Ref, f : Field a :: Permission[r,f] == 0.0  ) ; \nvar oldHeap, tmpHeap : HeapType ;"
          val thrDynamicBody = getCommandCode(block, dlNd.name)
          val thrBody = "\n{" + thrStaticBody + "\n" + objInits + thrDynamicBody + "\n}"
          val thrCode = thrDecl + thrClaim + thrBody
          ClassCode += thrCode
        }

        case _ => { null }
      }
    }
    return ClassCode;
  }

  def getCommandCode(cmd: CommandNd, objName: String): String = {
    val result: String = cmd match {
      case AssertCmdNd(assertion) => {
        val exp = new ExpCodeGen().getExpCode(assertion)
        val expCode = new ExpCodeGen().getExpCode(assertion)
        val parts = expCode.split(" ")
        val per = "\nassert Permission[this," + parts(0) + "] > 0.0 ;"
        val assertCmdCode = per + "\nassert Heap[this," + parts(0) + "] " + parts(1) + ";"
        assertCmdCode
      }
      case _ => ""
    }
    return result
  }

  def getMethodCode(acc: Access, name: String, params: List[ParamDeclNd], preCnds: List[PreCndNd], postCnds: List[PostCndNd], givesPers: List[GivesPerNd], takesPers: List[TakesPerNd], borrowsPers: List[BorrowsPerNd]): String =
    {
      var methCode = "";
      methCode = Format.func0(name + ":")
      var takes = ""
      var pre = ""
      var body = ""
      var post = ""
      var gives = ""

      //takes

      for (tp <- takesPers) {
        for (lsn <- tp.pmn.lsn) {
          lsn match {
            case ObjectIdLSN(nameExp) => {
              takes = Format.func2("oldPermission := Permission;")
              takes += Format.func2("if(Permission")
            }
            case _ => contracts.Contracts.toDo("Array Location Set Node")
          }

          return ""
        }

      }

      //Pre

      //Body

      //Post

      //Gives

      return "o"
    }

  def genClassClaim() {
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ClaimNd(pmn) =>
          val perm_pairs = pmn.pm;
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
            outputBuffer.put("//Class Claim")
            outputBuffer.newLine
            outputBuffer.put("Permission[this," + name + "] == " + amnt + ";")
            
          }
        case _ =>
      }
    }
    outputBuffer.newLine
    outputBuffer.put("assert(forall<x> r: Ref, f: Field x :: Permission[r,f] <= 1.0)")
  }

  def genClassInvariant() {
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
            case _ =>  {
              transContext.set("Permission", "this")
              outputBuffer.newLine
              outputBuffer.setError("Bad Invariant Expression", mem.coord)
              outputBuffer.put("assert " + build(expNd,transContext)); // build -> object
              outputBuffer.newLine
              outputBuffer.clearError
            }
          }
        }
        case _ => "" // Unreachable not working, will fix it later
      }
    }
  }

  def build(expNd: ExpNd, buildFor : TransContext): String = {
    //expNd, parameter(which heap to use) H1,H2 , Structure combining array heap, permission map, -> array permission, field permission, this translating in context of which object
    // process inv wrt object(locked)
    // Output Builder

    val result: String = expNd match {

      case NoExpNd() => ""

      case IntLiteralExpNd(i: Long) => i.toString()

      case FloatLiteralExpNd(x: Double) => x.toString()

      case NameExpNd(name: NameNd) => {
        buildFor.getHeap()+"["+buildFor.getObjRef()+","+name.qn.toString()+"]"
        }
      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => List(build(x,buildFor), " ", resBiOp(op), " ", build(y,buildFor)) mkString

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => resUnaOp(op) + build(x,buildFor)

      case MemberExpNd(x: ExpNd, name: String) => build(x,buildFor) + "." + name

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => {
        var chainExp = ""
        for (op <- ops)
          chainExp += (build(operands(ops.indexOf(op)),buildFor) + chainingOpTrans(op) + build(operands(ops.indexOf(op) + 1),buildFor))
        chainExp;
      }
      case FetchExpNd(x: ExpNd) => build(x,buildFor)

      case AsExpNd(x: ExpNd, _) => build(x,buildFor) // TODO insert type conversions?

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

  private def genObjectsInit() {
    var objDecl, objInits = ""
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ObjDeclNd(isGhost: Boolean, isConst: Boolean, acc: Access, ty: TypeNd, init: InitExpNd) => {
          objDecl = getObjDeclCode(isConst, acc, ty, dlNd.name, mem.name)
          val objName = dlNd.name + "." + mem.name
          val objType: String = TypeCodeGen(ty)
          val exp: String = new ExpCodeGen().getExpCode(init, objName.toString())
          outputBuffer.newLine
          outputBuffer.setError("Bad Initialization of Object" ,mem.coord)
          outputBuffer.put( "Heap[this, " + objName + "] := " + exp + ";" ) // TODO When it will be that?
          outputBuffer.newLine
          outputBuffer.clearError
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