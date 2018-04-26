package cBackEnd
import frontEnd.AST._
import CFG._

class CFGBuilder {
  var factory = new CFGFactory
  
  //prepare the startNd and the endThreadNd for this thread
  def run(cmd: CommandNd,startNd:CFGNd):CFGNd={
       var endThreadNd = factory.makeEndOfThreadCfgNd()
       var lastNd = build(cmd,startNd)
     lastNd.setNext(endThreadNd)

      return endThreadNd
   
  }
  def build(cmd: CommandNd, lastNd: CFGNd): CFGNd ={
  
    cmd match {
  
    case IfCmdNd(guard, thenCmd, elseCmd) =>
      var ifNd = factory.makeIfCfgNd(guard)
      var mergeNd = factory.makeEndIfCfgNd()
      // var mergeNd = factory.makeMergeCfgNd()
      lastNd.setNext(ifNd)
      var result = build(thenCmd, ifNd)
      result.setNext(mergeNd)
      result = build(elseCmd, ifNd)
      result.setNext(mergeNd)
      return mergeNd

      
    // assume SkipCmdNd is the end of current thread
    case SkipCmdNd() => 
   
      return lastNd
    case SeqCommandNd(fstCmd, sndCmd) =>
      var result = build(fstCmd, lastNd)
      return build(sndCmd, result)

    case AssignmentCmdNd(lhs, rhs) =>
      var assignmentNd = factory.makeAssignmentCfgNd(lhs, rhs)
      lastNd.setNext(assignmentNd)
      return assignmentNd

    case WhileCmdNd(guard, body) =>
      var whileNd = factory.makeWhileCfgNd(guard)
      lastNd.setNext(whileNd)
      var result = build(body, whileNd)
      result.setNext(whileNd)
      return whileNd

    case CoCmdNd(fstCmd, sndCmd) =>
      var coNd = factory.makeCoCfgNd()
      var mergeNd = factory.makeMergeCfgNd()
      lastNd.setNext(coNd)
      var result = build(fstCmd, coNd)
      result.setNext(mergeNd)
      result = build(sndCmd, coNd)
      result.setNext(mergeNd)
      return mergeNd

    case AcceptCmdNd(methodImplementationList) =>
      var acceptNd = factory.makeAcceptCfgNd(methodImplementationList)
      var mergeNd = factory.makeMergeCfgNd()
      lastNd.setNext(acceptNd)
     
      for (mthImpNd: MethodImplementationDeclNd <- methodImplementationList: List[MethodImplementationDeclNd]) {
        
        var mthNd = factory.makeMethImpCfgNd(mthImpNd.name, mthImpNd.paramList, mthImpNd.guard,methodImplementationList)
        acceptNd.setNext(mthNd)
        var result = build(mthImpNd.fstCmd, mthNd)
    //    var mthThenNd = factory.makeMethImpThenCfgNd()
    //    result.setNext(mthThenNd)
    //    result = build(mthImpNd.sndCmd, result)
        result.setNext(mergeNd)
      }
      return mergeNd

    case WithCmdNd(lock, guard, command) =>
      var withNd = factory.makeWithCfgNd(lock, guard)
      var endNd = factory.makeEndSeqCfgNd()
      lastNd.setNext(withNd)
      var result = build(command, withNd)
      lastNd.setNext(result)
      return endNd

    case ForCmdNd(forDecl, repetitions, body) =>
      var forNd = factory.makeForCfgNd(forDecl.fvd.name, repetitions)
      lastNd.setNext(forNd)
      var result = build(body, forNd)
      result.setNext(forNd)
      return forNd

    case CallCmdNd(recipient, argList) =>
      var callNd = factory.makeCallCfgNd(recipient, argList)
      lastNd.setNext(callNd)
      return callNd

    // case LocalDeclCmdNd(isConst: Boolean, name: String, ty: TypeNd, init: ExpNd, stmt: CommandNd) =>
    case LocalDeclCmdNd(decl: LocalDeclNd) =>
      //      var locDecNd = factory.makeLocalDeclCfgNd(isConst, name, ty, init)
      var locDecNd = factory.makeLocalDeclCfgNd(decl)
      lastNd.setNext(locDecNd)
      var cmdNd = build(decl.cmd, locDecNd)
      //    var result = build(stmt, locDecNd)
      //    return result
      return cmdNd

    case CoForCmdNd(forDecl: ForDecl, repetitions: ExpNd, body: CommandNd) =>
      var coForNd = factory.makeCoForNd(forDecl.fvd.name, repetitions)
      var endNd = factory.makeEndSeqCfgNd()
      lastNd.setNext(coForNd)
      var result = build(body, coForNd)
      result.setNext(endNd)
      return endNd

    //   case _ => return lastNd // unfinished

    }
  }
}