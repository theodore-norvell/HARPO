package cBackEnd

import frontEnd._
import frontEnd.AST._
import java.io._
import checker.Checker
import checker.CheckerTypes._



/**
 * @author nhacnguyen
 */
object ObjCodeGen {
  def apply(objNd : DeclNd) : (String, String) = {
    val ocg = new ObjCodeGen()
    ocg.build(objNd)
  }
}

class ObjCodeGen {
 
 /*
  * genObjDeclCode() : generates C codes for ObjDeclNd from AST.
  * Input : ObjDeclNd
  * Ouput : a tuple (str1 : String,str2 : String) where str1 is the declaration of the object,
  * str2 is the initialized code of the object.
  */
  def build(objNd : DeclNd) : (String, String) = {
    val NM = NameManager
    objNd match {
      case ObjDeclNd(isGhost:Boolean,isConst : Boolean, acc : Access, ty : TypeNd, init : InitExpNd) =>
        val objName : String = objNd.name
        val objType : (String, String) = TypeCodeGen(ty)
        val objInit = ExpCodeGen(init, objName)
        
        (List((if (isConst) "const " else "") + objType._1, NM.mkName(objName), objType._2) mkString " ", objInit)
      case _ => ("", "")
    }
      
  }

//  private def resInitExp(init : InitExpNd,name : String) : String = {
//    val NM = NameManager
//    val result : String = init match{
//          case ValueInitExpNd( exp : ExpNd ) => ExpCodeGen.build(exp)                       
//          case NewInitExpNd( ty : TypeNd, args : List[ExpNd] ) =>
//            val argsLs = args.map(ExpCodeGen.build(_))
//            List(NM.mkClassConstructorName(ty.toString) + "(",argsLs mkString ",",");\n") mkString " "
//            
//          case ArrayInitExpNd( forDecl : ForDecl, bound : ExpNd, a : InitExpNd ) =>
//            val n = name + "[" + decl.name + "]"
//            var initExp = resInitExp(a,name + "[" + decl.name + "]");
//            if (!List("if ","for ").exists(initExp.contains)) initExp = name + "[" + decl.name + "]" + " = " + initExp
//            List("for (",decl.name," = 0;",decl.name,"<",ExpCodeGen.build(bound),";",decl.name," ++ )\n",initExp,"\n") mkString " "
//            
//          case IfInitExpNd( guard : ExpNd, a : InitExpNd, b : InitExpNd ) =>
//            var initExpA = resInitExp(a,name);
//            var initExpB = resInitExp(b,name);
//            if (!List("if ","for ").exists(initExpA.contains)) initExpA = name + " = " + initExpA
//            if (!List("if ","for ").exists(initExpB.contains)) initExpB = name + " = " + initExpB
//            List("if (",ExpCodeGen.build(guard),")","{",initExpA,"}\n","else {",initExpB,"}\n") mkString " "
//            
//          case WidenInitExpNd( a : InitExpNd ) =>resInitExp(a,name)
//          case _ => init.toString()
//        }
//    result
//  }
  
}