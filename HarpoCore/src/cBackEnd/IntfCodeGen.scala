package cBackEnd

import frontEnd._
import frontEnd.AST._
import java.io._
import checker.Checker
import checker.CheckerTypes._
import scala.collection.mutable.HashMap

/**
 * @author nhacnguyen
 */
object IntfCodeGen {
  def apply(intf : DeclNd, table : HashMap[String, HashMap[String, String]]) : String = {
    val icg = new IntfCodeGen()
    icg.build(intf, table)
  }
}

class IntfCodeGen {
  def build(intfNd : DeclNd, table : HashMap[String, HashMap[String, String]]) : String = {
    val NM = NameManager
    var constantList = List[String]()
    var procCode = ""
    var constructorCode = ""
    var intfCode = ""
    intfNd match {
      case IntfDeclNd() => {
        var nameTbl = HashMap[String, String]()
        intfCode = "typedef struct " + NM.mkName(intfNd.name) + "{\n"
        
        for (sup <- intfNd.asInstanceOf[ClassLike].superTypes) {
          intfCode += NM.mkName(sup.toString()) + " " + NM.mkSuperName(sup.toString()) + ";\n"
          constructorCode += NM.mkClassConstructorName(sup.toString()) + "(&" + NM.mkClassParaName(intfNd.name) + "->" + NM.mkSuperName(sup.toString()) + ");\n"
          nameTbl = nameTbl ++ table(sup.toString())
        }
        
        for (mem <- intfNd.asInstanceOf[ClassLike].members.declarations) {
          mem match {
            case MethodDeclNd(acc : Access, params : List[ParamDeclNd]) =>
              procCode += MethCodeGen(mem, intfNd.name) + "\n"
              constructorCode += "InitProcStruct(&(" + NM.mkClassParaName(intfNd.name) + "->" + NM.mkName(mem.name) + ".ps));\n"
              intfCode += NM.mkMethodName(mem.name, intfNd.name) + "* " + NM.mkName(mem.name) + ";\n"
              nameTbl(mem.name) = NM.mkMethodName(mem.name, intfNd.name)
              NM.putParamMap(mem.name, intfNd.name, params)
            case ObjDeclNd(isConst : Boolean, acc : Access, ty : TypeNd, init : InitExpNd) =>
              val objCode = ObjCodeGen(mem)
              intfCode += objCode._1 + ";\n"
              nameTbl(mem.name) = NM.mkIntfObjName(mem.name, intfNd.name)
              
              if (init.isInstanceOf[NewInitExpNd]) {
                if (isConst) constantList = constantList :+ ExpCodeGen(init, mem.name, intfNd.name)
                else constructorCode += ExpCodeGen(init, mem.name, intfNd.name)
              }
              else {
                if (isConst) constantList = constantList :+ NM.mkClassParaName(intfNd.name) + "->" + NM.mkName(mem.name) + " = " + ExpCodeGen(init, mem.name, intfNd.name)
                else constructorCode += NM.mkClassParaName(intfNd.name) + "->" + NM.mkName(mem.name) + " = " + ExpCodeGen(init, mem.name, intfNd.name) + ";\n"
              }
            case _ =>{}  
            }
          }
        intfCode += "} " + NM.mkName(intfNd.name) + ";\n"
        
        if (!constantList.isEmpty) {
          constructorCode = NM.mkName(intfNd.name) + " initConst = {" + (constantList mkString ",") + "};\nmemcpy(" + NM.mkClassParaName(intfNd.name) + ",&initConst,sizeof(" + NM.mkName(intfNd.name) + "));\n" + constructorCode
        }
        
        constructorCode = "void " + NM.mkClassConstructorName(intfNd.name) + "(" + NM.mkName(intfNd.name) + " *" + NM.mkClassParaName(intfNd.name) + "){\n" + constructorCode + "}\n"
      }
      case _ => {}
    }
    procCode + intfCode + constructorCode
  }
}