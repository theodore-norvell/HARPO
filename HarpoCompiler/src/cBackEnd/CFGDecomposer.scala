package cBackEnd
import frontEnd.AST._
import CFG._
import scala.collection.mutable.HashSet
class CFGDivider {
  var factory = new CFGFactory
  var usedNds = List[CFGNd]()
  var className = ""
  var identNumber = 0
  def divide(curNd: CFGNd, lastNd: CFGNd, startList: List[CFGNd]): List[CFGNd] =
    {
      //      if (curNd.edgeList.length == 0) {
      //        curNd.setNext(factory.makeContinuationCfgNd("null"))
      //        return startList
      //      }

      curNd match {
        // multi-branch decomposition
        case AcceptCfgNd(methodImplementationList: List[MethodImplementationDeclNd]) =>
          // need a name for the continuation node
          var resList = startList
          for (g <- curNd.edgeList) {
            //need a name here

            resList = divide(g, curNd, resList)
          }
          return resList

        // instead of cutting after acceptNd, i cut the CFG after Method Implementation node 
        // to make sure the acceptNode can access to the guard statements
        case StartCfgNd(func_name: String, class_name: String, funcType: Int) =>
          className = class_name // set class name from the first startNd
          return divide(curNd.getNext(0), curNd, startList ::: List(curNd))
        case MethImpCfgNd(name, paramList, guard, methodImplementationList: List[MethodImplementationDeclNd]) =>

          //          var tempNd = curNd.getNext(0)
          lastNd.replaceNext(curNd, factory.makeContinuationCfgNd("FuncMeth_" + className + name + identNumber))
          //          curNd.replaceNext(tempNd, factory.makeContinuationCfgNd("Func_"+className + name + startList.size))
          //need a name here
          var startNd = factory.makeStartCfgNd("FuncMeth_" + className + name + identNumber, className, 1)
          identNumber += 1
          //          startNd.setNext(tempNd)
          //          startNd.setNext(curNd.getNext(0))
          var startMeth = factory.makeStartOfMeth(curNd.asInstanceOf[MethImpCfgNd])
          startNd.setNext(startMeth)
          startMeth.setNext(curNd.getNext(0))
          return divide(startMeth, startNd, startList ::: List(startNd))

        case CoCfgNd() =>
          // need a name for the continuation node
          lastNd.replaceNext(curNd, factory.makeContinuationCfgNd("FuncCo_" + className + identNumber))
          var resList = startList
          for (g <- curNd.edgeList) {
            //need a name here
            var startNd = factory.makeStartCfgNd("FuncCo_" + className + identNumber, className, 3)
            identNumber += 1
            startNd.setNext(g)
            resList = divide(g, startNd, resList ::: List(startNd))
          }
          return resList
        //loop decomposition
        case ForCfgNd(exp, guard) =>
          if (!usedNds.exists(s => s == curNd.edgeList(0))) {
            usedNds = usedNds ::: List(curNd.edgeList(0))
            return divide(curNd.getNext(0), curNd, startList)
          } else {
            return divide(curNd.getNext(1), curNd, startList)
          }

        case WhileCfgNd(exp) =>
          if (!usedNds.exists(s => s == curNd.edgeList(0))) {
            usedNds = usedNds ::: List(curNd.edgeList(0))
            return divide(curNd.getNext(0), curNd, startList)
          } else {
            return divide(curNd.getNext(1), curNd, startList)
          }

        //          if (curNd.visited())
        //            divide(curNd.getNext(0), curNd, startList)
        //          else {
        //            curNd.setVisit();
        //            divide(curNd.getNext(1), curNd, startList)
        //          }

        // linear decomposition 
        case MergeCfgNd() | EndSeqCfgNd() =>
          if (!usedNds.exists(s => s eq curNd.edgeList(0))) {

            usedNds = usedNds ::: List(curNd.edgeList(0))
            var continuationNd = factory.makeContinuationCfgNd("FuncMerge_" + className + identNumber)
            lastNd.replaceNext(curNd, continuationNd)
            //need a name here
            var startNd = factory.makeStartCfgNd("FuncMerge_" + className + identNumber, className, 2)
            identNumber += 1
            startNd.setNext(curNd.getNext(0))

            curNd.setNext(continuationNd)
            divide(curNd.getNext(0), startNd, startList ::: List(startNd))

          } else {
            lastNd.replaceNext(curNd, curNd.getNext(1))
            return startList
          }
        case WithCfgNd(lock, guard) =>
          lastNd.replaceNext(curNd, factory.makeContinuationCfgNd("FuncWith_" + className + identNumber))
          //need a name here
          var startNd = factory.makeStartCfgNd("FuncWith_" + className + identNumber, className, 4)
          identNumber += 1
          startNd.setNext(curNd)
          divide(curNd, startNd, startList ::: List(startNd))

        case CallCfgNd(recipient, argList) =>
          var tryCallNd = factory.makeTryCallCfgNd(recipient, argList)
          lastNd.replaceNext(curNd, tryCallNd)
          tryCallNd.setNext(factory.makeContinuationCfgNd("FuncCall_" + className + identNumber + "A"))
          //need a name here
          var startNd1 = factory.makeStartCfgNd("FuncCall_" + className + identNumber + "A", className, 5)
          startNd1.setNext(curNd)
          var temp = curNd.getNext(0)
          curNd.replaceNext(temp, factory.makeContinuationCfgNd("FuncCall_" + className + identNumber + "B"))
          var startNd2 = factory.makeStartCfgNd("FuncCall_" + className + identNumber + "B", className, 0)
          identNumber += 1
          startNd2.setNext(temp)
          divide(temp, startNd2, startList ::: List(startNd1) ::: List(startNd2))

        // other case
        case EndOfThreadCfgNd() =>
          curNd.setNext(factory.makeContinuationCfgNd("null"))
            // println("@@@@@@@@@@@@@@@@" + startList.size)
          return startList
        case _ =>
          var resList = startList
          //          for (g <- curNd.edgeList)
          //            if (!usedNds.exists(s => s == g)) {
          //              usedNds = usedNds ::: List(g)
          //              resList = divide(g, curNd, resList)
          //            }
          for (g <- curNd.edgeList)
            //   if (!usedNds.exists(s => s == g)) {
            //   usedNds = usedNds ::: List(g)
            resList = divide(g, curNd, resList)
          // }
          return resList

      }
    }
}