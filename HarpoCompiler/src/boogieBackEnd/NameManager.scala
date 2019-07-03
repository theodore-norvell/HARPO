package boogieBackEnd
import frontEnd.AST._;
import frontEnd.FQN;

private class NameManager() {
  
  def getFQN(exp: ExpNd) : String = {
     exp match {
      case exp@NameExpNd(nameNd) => exp.name.decl.get.fqn.names.toString();
      case _ => "NoName"
    }
  }
  def getFQN(lsn: LocSetNd) : String = {
    lsn match {
      case ObjectIdLSN(exp: ExpNd) => getFQN(exp)
      case ArrayLSN(exp: ArrayExpNd) => getFQN(exp.objId)
    }
  }
}