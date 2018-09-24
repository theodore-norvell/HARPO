package cBackEnd

import java.io._
import java.io.OutputStreamWriter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import frontEnd.ErrorRecorder
import frontEnd.AST
import frontEnd.AST._
import cBackEnd.CFG._
import java.net.URL
import scala.io.Source

/**
 * @author nhacnguyen
 */
class CBackEnd(val dl : frontEnd.AST.DeclList) 
{
private val cfgBuilder = new CFGBuilder()
private val cfgFactory = new CFGFactory()
private val cfgDivider = new CFGDivider
private val codeGenerator = new CodeGenerator()
  
    
def getCCode() : String = {
  val code = genHeaderCode + genDeclCode + genConcurentCode
  NameManager.clear()
  return code
}
    
private def genHeaderCode() : String = {
  val headerUrl : URL = this.getClass().getResource("/cBackEnd/harpoPrelude.h")
  val header = new File(headerUrl.toURI())
  val contents = Source.fromFile(header)
  val str = try contents.mkString finally contents.close()
  
  str
}
    
private def genConcurentCode() : String = {
  for (dlNd : DeclNd <- dl.decls ) {
    dlNd match {
      case dlNd : ClassDeclNd =>
        for (dllNd : DeclNd <- dlNd.directMembers ) {
          dllNd match {
            case ThreadDeclNd(thrClaim: ClaimNd,block : CommandNd) =>
              val threadName = dllNd.name.replace("#", "")
              var startNd = cfgFactory.makeStartCfgNd("FuncThread_" + dlNd.name + threadName, dlNd.name, 0)
              cfgBuilder.run(block, startNd)
              //print first graph here
              println("\n________________________________________")
              println("## Printing original CFG ###############\n")
              println(CFG.printCFG(startNd))
              
              var resList = cfgDivider.divide(startNd, startNd, List())
              //print 2nd graph here
              for (node : CFGNd <- resList) {
                println("________________________________________")
                println("## Printing decomposed CFG #############\n")
                println(CFG.printCFG(node))
              }
              
              for (stNd : CFGNd <- resList){
                codeGenerator.runGenerateS(stNd)
              }
              case _ =>
            }
          } 
      case _ =>{}
    }
  }
  val concurentCode = codeGenerator.getCode()
  concurentCode
}
    
private def genDeclCode() = { 
  var globalObjCode = ""
  var initializeCode = ""
  var nameTbl = HashMap[String, HashMap[String, String]]()
  var cCode = ""
        
  for (dlNd<-dl.decls){
    dlNd match{
      case ObjDeclNd(isGhost, isConst, acc, ty, init) =>{
        if (dlNd.name != "true" && dlNd.name != "false"){
            val code = ObjCodeGen(dlNd)
            init match {                   
              case NewInitExpNd( ty : TypeNd, args : List[ExpNd] ) =>
                globalObjCode = code._1 + ";\n"
                initializeCode += code._2           
              case ArrayInitExpNd( forDecl : ForDecl, bound : ExpNd, a : InitExpNd ) =>
                globalObjCode = code._1 + ";\n"
                initializeCode += code._2        
              case IfInitExpNd( guard : ExpNd, a : InitExpNd, b : InitExpNd ) =>
                globalObjCode = code._1 + ";\n"
                initializeCode += code._2            
              case _ => globalObjCode = code._1 + " = " + code._2 + ";\n"
            }
            cCode += globalObjCode
        }
      }
      case IntfDeclNd() =>{
        nameTbl(dlNd.name) = HashMap[String, String]()
        val code = IntfCodeGen(dlNd, nameTbl)
        cCode += code
      }
          
      case ClassDeclNd() =>{
        val code = ClassCodeGen(dlNd, nameTbl)
        cCode += code
      }
      
      case _ => {}
    }
  }
  initializeCode = "void initialize(){\n" + initializeCode + "}\n"     
  cCode + initializeCode + "\n"
}
}