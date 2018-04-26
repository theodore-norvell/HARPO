package cBackEnd

import frontEnd._
import frontEnd.AST._
import java.io._
import checker.Checker
import checker.CheckerTypes._
import contracts.Contracts

/**
 * @author TJ
 */
object ParamCodeGen {
  def apply(paramNd : DeclNd) : String = {
    val pcg = new ParamCodeGen()
    pcg.build(paramNd)
  }
}

class ParamCodeGen {
  def build(paramNd : DeclNd) : String = {
    val NM = NameManager
    val param = paramNd.asInstanceOf[ParamDeclNd]
    var paramStr = ""
    var paramName = ""
    val paramType = TypeCodeGen(param.ty)
    paramNd.asInstanceOf[ParamDeclNd].paramCategory match {
      case InParamCategory =>
        paramName = paramNd.name
      case OutParamCategory =>
        paramName = "*" + paramNd.name
      case _ =>
        Contracts.unreachable("ObjParamCategory is invalid!")
    }
    paramStr = paramType._1 + " " + NM.mkName(paramName) + " " + paramType._2 + ";\n"
    paramStr
  }
}