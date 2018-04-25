package cBackEnd

import frontEnd.AST._
import scala.collection.mutable.ArrayBuffer

trait CFG {

  abstract class CFGNd {
    val nodeNumber = CFG.nextNodeNumber() ;
    var edgeList = List[CFGNd]()
    var isVisited = false
    def setNext(nd: CFGNd) = {
      edgeList = edgeList ::: List(nd)
    }
    def getNext(i: Int): CFGNd = {
      return edgeList(i)
    }
    def replaceNext(nd0: CFGNd, nd1: CFGNd) // 0 replaced by 1
    {
      if(edgeList.exists(nd0=>true))
      {   var i = edgeList.indexOf(nd0)
      edgeList = edgeList.splitAt(i)._1 ::: List(nd1) ::: edgeList.splitAt(i)._2
//      edgeList = edgeList.filter(nd0 => true)
        edgeList =((ArrayBuffer() ++ edgeList) - nd0).toList}

    }
    def visited(): Boolean =
      {
        return isVisited
      }
    def setVisit() =
      {
        isVisited = true
      }
    
    def printCfg() : String =
    {
      var str = ""
      
      str += nodeNumber + " : " + this + "\n"
      if(! edgeList.isEmpty ) {
          str += "    --> "
          for (node <- edgeList) {
              str += node.nodeNumber + "  " }
          str += "\n" }
      
      return str
    }
  }
  case class MergeCfgNd() extends CFGNd

  case class EndSeqCfgNd() extends CFGNd

  //funcType:  0)normal func 1)start of method 2)start of merge 3)Co 4)with 5)call
  case class StartCfgNd(func_name: String,class_name: String,funcType:Int) extends CFGNd

  // case class IdleNd() extends CFGNd

  case class AssignmentCfgNd(lhs: Seq[ExpNd], rhs: Seq[ExpNd]) extends CFGNd

  case class IfCfgNd(exp: ExpNd) extends CFGNd

  case class EndIfCfgNd() extends CFGNd

  case class WhileCfgNd(exp: ExpNd) extends CFGNd

  case class CoCfgNd() extends CFGNd

  case class AcceptCfgNd(methodImplementationList: List[MethodImplementationDeclNd]) extends CFGNd

  case class MethImpCfgNd(name: String, paramList: List[ParamDeclNd], guard: ExpNd, methodImplementationList: List[MethodImplementationDeclNd]) extends CFGNd
  
  case class StartOfMeth(methCfgNd:MethImpCfgNd) extends CFGNd
  
  case class MethImpThenCfgNd() extends CFGNd

  case class WithCfgNd(lock: ExpNd, guard: ExpNd) extends CFGNd

  case class ForCfgNd(name: String, repetitions: ExpNd) extends CFGNd

  case class CallCfgNd(recipient: ExpNd, argList: List[ExpNd]) extends CFGNd
  
  case class TryCallCfgNd(recipient: ExpNd, argList: List[ExpNd]) extends CFGNd
  //case class LocalDeclCfgNd(isConst: Boolean, name: String, ty: TypeNd, init: ExpNd) extends CFGNd
  case class LocalDeclCfgNd(decl: LocalDeclNd) extends CFGNd

  case class CoForNd(name: String, repetitions: ExpNd) extends CFGNd

  case class ContinuationCfgNd(name: String) extends CFGNd

  case class EndOfThreadCfgNd() extends CFGNd
}

object CFG extends CFG {
    private var nodeCounter = -1 
    
    def nextNodeNumber() = { nodeCounter += 1 ; nodeCounter }
    
    def printCFG( nd : CFGNd ) : String = {
        val stringBuilder = new StringBuilder()
        var toPrint = List(nd) // Nodes discovered, but not yet printed.
        var vs = Set(nd)  // The set of all nodes that are on or have been on the toPrint list. 
        while( ! toPrint.isEmpty ) {
            val v = toPrint.head 
            toPrint = toPrint.tail
            stringBuilder ++= v.printCfg()
            for( u <- v.edgeList ) {
                if( ! vs.contains( u ) ) {
                    toPrint ++= List(u)
                    vs ++= Set(u) 
                }
            }
        }
        stringBuilder.toString()
    }
}