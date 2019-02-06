package boogieBackEnd
import frontEnd.AST 
import frontEnd.AST._
import util.Format

private class ClassCodeGen(val dlNd: DeclNd) {

  def getClassCode(): String = {
    
    val clsName = dlNd.name;
    
    var ClassCode = ""
    var objDecls = "";
    var objInits = "";
    var classClaim = "";
    var classInvariant = "";
    
    val className = dlNd.name;
    val classConstant = "\nconst unique " + clsName + ":className;";
         ClassCode += Format.func0(classConstant);
    
         
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ObjDeclNd(isGhost: Boolean, isConst: Boolean, acc: Access, ty: TypeNd, init: InitExpNd) => {
          ClassCode += Format.func0(getObjDeclCode(isConst, acc, ty, dlNd.name, mem.name))
          }
        case _ => "" 
     }
    }     
    val constructorProc = 
        Format.func0("procedure " + className + " (this:Ref)") + 
        Format.func1("requires dtype(this) <:"+ clsName + ".constructor")+
        Format.func1("modifies Heap;")+
        Format.func1("modifies Permission;")+
        Format.func1("{" )+
        Format.func2("var Permission : PermissionType where (forall <x> r: Ref, f: Field :: Permission[r,f] == 0.0)")+
        Format.func2("var oldHeap:HeapType;")+
        Format.func2("havoc Heap;")+
        Format.func2(getObjectsInit())+
        Format.func2(getClassClaim()) +
        Format.func2(getClassInvariant()) +
        Format.func1("}")
            
        ClassCode += constructorProc;
    
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        
        case MethodDeclNd(acc, paramList, preCndList, postCndList, givesPerList, takesPerList, borrowsPerList) => {
          val methCodeGen = new MethCodeGen(mem)
          val methDeclCode = methCodeGen.getMethDeclCode(acc, dlNd.name, paramList, preCndList, postCndList, givesPerList, takesPerList, borrowsPerList)
          
        
        }

        case ThreadDeclNd(claimList: List[ClaimNd], block: CommandNd) => {
          val staticThrVars = """
                      var oldHeap, preHeap, Heap_tmp: HeapType;
                      var oldArrayHeap, preArrayHeap, ArrayHeap_tmp : ArrayHeapType;
                      var Permission, oldPermission, prePermission : PermissionType;
                      var ArrayPermission, oldArrayPermission, preArrayPermission : ArrayPermissionType;
                      
                      //intial Permissions
                      oldPermission := Permission;
                      havoc Permission;
                      assume (forall<x> r:Ref, f: Field x :: Permission[r, f] == 0.0);
                      
                      //array initial permission
                      oldArrayPermission := ArrayPermission;
                      havoc ArrayPermission;
                      assume (forall<x> r:ArrayRef x, f : int:: ArrayPermission[r, f] == 0.0); 
          """
          val thr = new ThreadCodeGen(mem);
          val ThrCode = thr.getThreadCode(dlNd.name, claimList, block)
          val threadName = mem.name.replace("#", "")
          val thrDecl = "\nprocedure " + dlNd.name + "." + threadName + "(this:Ref)"
          val thrClaim = "\nmodifies Heap;" // {add claim}
          val thrStaticBody = "\nvar Permission : PermissionType where (forall <a> r:Ref, f : Field a :: Permission[r,f] == 0.0  ) ; \nvar oldHeap, tmpHeap : HeapType ;"
          val thrDynamicBody = getCommandCode(block, dlNd.name)
          val thrBody = "\n{" + thrStaticBody + "\n" + objInits + thrDynamicBody + "\n}"
          val thrCode = thrDecl + staticThrVars + thrClaim + thrBody
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
    var methCode ="";
    methCode = Format.func0(name + ":")
    var takes = ""
    var pre = ""
    var body = ""
    var post = ""
    var gives = ""
    
    //takes
    
    for(tp <- takesPers)
    {
      for(lsn <- tp.pmn.lsn )
      {
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
  
   def getClassClaim(): String = {
      var classClaims= Format.func2("//Class Claim")
      for (mem <- dlNd.asInstanceOf[ClassLike].directMembers){ 
        mem match {
        case ClaimNd(pmn) =>
          val perm_pairs = pmn.pm;
          var result = "";
          for ((loc,amount) <- perm_pairs)
          {
            var name = "" ;
            loc match {
              case ObjectIdLSN(nen) => {
                name = dlNd.name + "." + nen.name.toString();
              }
              case _ => contracts.Contracts.toDo("Location Set Node with Array")
            }
            var amnt : String = "";
            amount match {
              case IntLiteralExpNd(i : Long) => amnt = i.toString()
              case FloatLiteralExpNd(x : Double) => amnt = x.toString()
              case _ => contracts.Contracts.toDo("Permission Type Left")
            }
            classClaims += Format.func2("Permission[this," + name + "] == " + amnt + ";") 
          }
        case _ =>
        }
      }
      classClaims += Format.func2("assert(forall<x> r: Ref, f: Field x :: Permission[r,f] <= 1.0)")
      
      classClaims
    }
    
    
    def getClassInvariant(): String = {
      var classInvariant = Format.func2("//Class Invariant")
      for (mem <- dlNd.asInstanceOf[ClassLike].directMembers){ 
       mem match {
        case ClassInvNd(expNd) => {
          expNd match {
            case CanReadOp(loc) => classInvariant += Format.func2("assert Permission[this," + dlNd.name + "." + loc.getName().toString() + "] > 0.0;"); 
            
            case CanWriteOp(loc) => classInvariant += Format.func2("assert Permission[this," + dlNd.name + "." + loc.getName().toString() + "] == 1.0;");
            
            case PermissionOp(loc) => contracts.Contracts.toDo("Permission Operator: To get amount of permission")
                  
            case _ => classInvariant += Format.func2("assert " + build(expNd))
            }
          }
        case _ => "" // Unreachable not working, will fix it later
       }
    }
      classInvariant
    }


    def build(expNd : ExpNd) : String = { //expNd, parameter(which heap to use) H1,H2 , Structure combining array heap, permission map, -> array permission, field permission, this translating in context of which object
     // process inv wrt object(locked)
      // Output Builder
    
     val result : String = expNd match {
      
      case NoExpNd() => ""
      
      case IntLiteralExpNd( i : Long ) => i.toString()
     
      case FloatLiteralExpNd( x : Double ) => x.toString()
      
      case NameExpNd( name : NameNd ) => "Heap[this, " + name.qn.toString() + "]" //'that' case
        
      case BinaryOpExpNd( op : BinaryOperator, x : ExpNd, y : ExpNd ) => List( build(x), " ", resBiOp(op), " " ,build(y)) mkString
        
      case UnaryOpExpNd( op : UnaryOperator, x : ExpNd ) => resUnaOp(op) + build(x)
      
      case MemberExpNd( x : ExpNd, name : String ) => build(x) + "." + name
      
      case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd]) => {
        var chainExp = ""
          for(op <- ops)
             chainExp += (build(operands(ops.indexOf(op))) + chainingOpTrans(op) + build(operands(ops.indexOf(op)+1)))
        chainExp;
      }
      case FetchExpNd( x : ExpNd) => build(x)
    
      case AsExpNd( x : ExpNd, _ ) => build(x)   // TODO insert type conversions?
      
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
   
  private def resBiOp(op:BinaryOperator)={
    val result = op match{
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
  private def resUnaOp(op:UnaryOperator)={
    val result = op match {
        case NegativeOp => "-"
        case NotOp => "!"
    }
    result
  }
  private def resOpChain(op: ChainingOperator): String = {
    val result:String = op match {
      case EqualOp => "=="
      case NotEqualOp => "!="
      case LessOp => "<"
      case LessOrEqualOp => "<="
      case GreaterOp => ">"
      case GreaterOrEqualOp => ">="
    }
    result
  }
  
  
  private def getObjectsInit(): String = {
    var objDecl, objInits = ""
    var result = ""
     for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ObjDeclNd(isGhost: Boolean, isConst: Boolean, acc: Access, ty: TypeNd, init: InitExpNd) => {
           objDecl = getObjDeclCode(isConst, acc, ty, dlNd.name, mem.name)
              val objName =  dlNd.name + "." + mem.name
              val objType: String = TypeCodeGen(ty)
              val exp: String = new ExpCodeGen().getExpCode(init, objName.toString()) 
              objInits = "Heap[this, " + objName + "] := " + exp + ";" // TODO When it will be that?
              result += Format.func2(objInits)
           }
          case _ => ""
         }
        }
  result
  }
  private def getObjDeclCode(isConst: Boolean, acc: Access, ty: TypeNd, className: String, objName: String): String = {
    val objQN = className + "." + objName
    val objType: String = TypeCodeGen(ty)
    val objCode = "\nconst unique " + objQN + ":" + "Field " + objType + ";"
    return objCode
  }
  
}