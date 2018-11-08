package cBackEnd

import frontEnd._

import frontEnd.AST._
import java.io._
import checker.Checker
import checker.CheckerTypes._
import frontEnd.AST.PreCndNd;
import frontEnd.AST.PostCndNd;
/**
 * @author nhacnguyen
 */
object MethCodeGen {
  def apply(methNd : DeclNd, clsName : String) : String = {
    val mcg = new MethCodeGen()
    mcg.build(methNd, clsName)
  }
}

class MethCodeGen {
  def build(methNd : DeclNd, clsName : String) : String = {
    var code : String = ""
    methNd match {
      case MethodDeclNd( acc : Access, params : List[ParamDeclNd],preCndList: List[PreCndNd], postCndList: List[PostCndNd], givesPerList: List[GivesPerNd], takesPerList: List[TakesPerNd], borrowsPerList: List[BorrowsPerNd]) =>{
        val NM = NameManager
        val methName = NM.mkMethodName(methNd.name, clsName)
        code = "typedef struct " + methName + "{\nProcStruct ps;\n"
        for (mem <- params){
          mem match {
            case ParamDeclNd(isGhost: Boolean, ty : TypeNd, paramCategory : ParamCategory) => {
              val objType : (String, String) = TypeCodeGen(ty)
              val objName : String = paramCategory match {
                case InParamCategory => NM.mkName(mem.name)
                case OutParamCategory => (if (objType._2 != "") //method's parameter is an array type
                    NM.mkName(mem.name) else "*" + NM.mkName(mem.name.toString))
                case _ => ""              
              }
            code += objType._1 + " " + objName + objType._2 + ";\n"
            }
          } 
        }
       code += "}" + methName + ";\n"
      }
      case _ => {}
    }
    code
  }  
}