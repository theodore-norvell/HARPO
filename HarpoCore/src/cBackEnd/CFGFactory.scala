package cBackEnd
import frontEnd.AST._
import CFG._
class CFGFactory {
  def makeStartCfgNd(func_name: String,class_name: String,funcType:Int): CFGNd = new StartCfgNd(func_name: String,class_name: String,funcType:Int)
  def makeAssignmentCfgNd(lhs: Seq[ExpNd], rhs: Seq[ExpNd]): CFGNd = new AssignmentCfgNd(lhs, rhs)
  def makeIfCfgNd(exp: ExpNd): CFGNd = new IfCfgNd(exp)
  def makeEndIfCfgNd(): CFGNd = new EndIfCfgNd() 
  def makeWhileCfgNd(exp: ExpNd): CFGNd = new WhileCfgNd(exp)
  def makeCoCfgNd(): CFGNd = new CoCfgNd()
  def makeAcceptCfgNd(methodImplementationList: List[MethodImplementationDeclNd]): CFGNd = new AcceptCfgNd(methodImplementationList: List[MethodImplementationDeclNd])
  def makeMethImpCfgNd(name: String, paramList: List[ParamDeclNd], guard: ExpNd,methodImplementationList: List[MethodImplementationDeclNd])
     = new MethImpCfgNd(name, paramList, guard,methodImplementationList: List[MethodImplementationDeclNd])
  def makeMethImpThenCfgNd() = new MethImpThenCfgNd()
  def makeWithCfgNd(lock: ExpNd, guard: ExpNd) = new WithCfgNd(lock, guard)
  def makeForCfgNd(name: String, repetitions: ExpNd) = new ForCfgNd(name, repetitions)
  def makeCallCfgNd(recipient: ExpNd, argList: List[ExpNd]) = new CallCfgNd(recipient, argList)
  def makeTryCallCfgNd(recipient: ExpNd, argList: List[ExpNd]) = new TryCallCfgNd(recipient: ExpNd, argList: List[ExpNd])
  //def makeLocalDeclCfgNd(isConst: Boolean, name: String, ty: TypeNd, init: ExpNd) = new LocalDeclCfgNd(isConst, name, ty, init)
  def makeLocalDeclCfgNd( decl : LocalDeclNd) = new LocalDeclCfgNd(decl)
  def makeCoForNd(name: String, repetitions: ExpNd) = new CoForNd(name: String, repetitions: ExpNd)
  def makeMergeCfgNd() = new MergeCfgNd()
  def makeEndSeqCfgNd() = new EndSeqCfgNd()
  def makeContinuationCfgNd(name: String) = new ContinuationCfgNd(name: String)
  def makeEndOfThreadCfgNd() = new EndOfThreadCfgNd()
  def makeStartOfMeth(methCfgNd:MethImpCfgNd) = new StartOfMeth(methCfgNd:MethImpCfgNd)
}