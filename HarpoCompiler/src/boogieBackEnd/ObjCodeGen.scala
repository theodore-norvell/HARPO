package boogieBackEnd
import frontEnd.AST._
private class ObjCodeGen(dlNd: DeclNd) {

  def getInitObjCode(isConst: Boolean, acc: Access, ty: TypeNd, exp: InitExpNd, fqn: String): String = {
    val objName =  fqn
    val objType: String = TypeCodeGen(ty)
    val objInit: String = new ExpCodeGen().getExpCode(exp, objName) //pass buffer to childs , append
    var objCode=""
    if(exp!=null){
    objCode = "\nPermission[this, " + objName + "] := 1.0;"
    objCode += "\nassert Permission[this, " + objName + "] == 1.0;"
    objCode += "\nHeap[this, " + objName + "] := " + objInit + ";"
    }
    return objCode
  }
  def getDeclObjCode(isConst: Boolean, acc: Access, ty: TypeNd, className: String): String = {
    val objName = dlNd.name
    val objType: String = TypeCodeGen(ty)
    val objCode = "\nconst unique " + className +"."+objName + ":" + "Field " + objType + ";"
    return objCode
  }

}