package boogieBackEnd
import frontEnd.AST._;
import frontEnd.FQN;

object NameManager {

  def getFQN(exp: ExpNd): String = {
    exp match {
      case exp @ NameExpNd(nameNd) => nameNd.decl.get.fqn.toString();
      case _ => "NoName"
    }
  }

  def getFQN(locSet: LocSetNd): String = {
    locSet match {
      case ObjectIdLSN(exp: ExpNd) => getFQN(exp)
      case ArrayLSN(forVar, offSet, bound, inclusiveBound, locSet) => getFQN(locSet)   
      case _ => "NoName"
    }
  }
}