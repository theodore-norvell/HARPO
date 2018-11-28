package boogieBackEnd
import frontEnd.AST
import frontEnd.AST._
private class ClassCodeGen(val dlNd: DeclNd) {

  def getClassCode(): String = {
    val objName="";
    var boogieClassCode = ""
    var objDecls = "";
    var objInits= "";
    val code = "\ntype className; \nfunction dtype(Ref) returns (className); \nconst unique " + dlNd.name + ":className;";
    boogieClassCode += code;
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ClaimNd(pmn) => {
          //TODO
          
        }
        case ClassInvNd(pmn) => {
          //TODO
        }
        case ObjDeclNd(isGhost:Boolean,isConst: Boolean, acc: Access, ty: TypeNd, init: InitExpNd) => {
          val objCodeGen= new ObjCodeGen(mem)
          objDecls += objCodeGen.getObjDeclCode(isConst, acc, ty, dlNd.name)
          if(init != null) {objInits += objCodeGen.getObjInitCode(isConst, acc, ty, init, dlNd.name + "." + mem.name)} else {objInits = ""}
          boogieClassCode += objDecls;
        }
        
        case MethodDeclNd(acc, paramList, preCndList, postCndList, givesPerList,takesPerList,borrowsPerList) => {
          val methCodeGen = new MethCodeGen(mem)
          val methDeclCode = methCodeGen.getMethDeclCode(acc,dlNd.name, paramList, preCndList, postCndList, givesPerList,takesPerList,borrowsPerList)         
        }
        
        case ThreadDeclNd(claimList: List[ClaimNd],block: CommandNd) => {
          val thr = new ThreadCodeGen(mem);
          val ThrCode = thr.getThreadCode(dlNd.name, claimList, block)
          val threadName = mem.name.replace("#", "")
          val thrDecl = "\nprocedure " + dlNd.name + "." + threadName + "(this:Ref)"
          val thrClaim = "\nmodifies Heap;" // {add claim}
          val thrStaticBody = "\nvar Permission : PermissionType where (forall <a> r:Ref, f : Field a :: Permission[r,f] == 0.0  ) ; \nvar oldHeap, tmpHeap : HeapType ;"
          val thrDynamicBody = getCommandCode(block, dlNd.name)
          val thrBody = "\n{" + thrStaticBody + "\n"+ objInits + thrDynamicBody + "\n}"
          val thrCode = thrDecl + thrClaim + thrBody
          boogieClassCode += thrCode
        }
          

        case _ => { null }
      }
    }
    return boogieClassCode;
  }

  def getCommandCode(cmd: CommandNd, objName: String): String = {
    val result: String = cmd match {
      case AssertCmdNd(assertion) => {
        val exp= new ExpCodeGen().getExpCode(assertion)
        val expCode = new ExpCodeGen().getExpCode(assertion)
        val parts= expCode.split(" ")
        val per = "\nassert Permission[this," + parts(0) +"] > 0.0 ;"
        val assertCmdCode = per + "\nassert Heap[this," + parts(0) + "] " + parts(1) + ";"
        assertCmdCode
      }   
      case _ => ""
    }
    return result
  }

}