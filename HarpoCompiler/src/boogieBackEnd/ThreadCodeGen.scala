package boogieBackEnd
import frontEnd.AST._
class ThreadCodeGen(dlNd: DeclNd) {

  def getThreadCode(name: String, claimList: List[ClaimNd], block: CommandNd) : String = {
    val threadName = name.replace("#", "")
    val thrDecl = "\nprocedure " + dlNd.name + "." + threadName + "(this:Ref)"
    val thrStaticBody = "\nvar Permission : PermissionType where (forall <a> r:Ref, f : Field a :: Permission[r,f] == 0.0  ) ; \nvar oldHeap, tmpHeap : HeapType ;"
    var thrClaim = "";
    var claim = "";
    for (claimNd <- claimList) {
      claim = getClaimCode(claimNd)
      thrClaim +=claim;
    }
//    val thrDynamicBody = getCommandCode(block, dlNd.name)
//    val thrBody = "\n{" + thrStaticBody + "\n" + objInits + thrDynamicBody + "\n}"
    val thrCode = thrDecl + thrClaim //+ thrBody
    thrCode
  }

  def getClaimCode (claimNd: ClaimNd): String = {
    return "\nmodifies Heap;" // {add claim Translation}
  }

}