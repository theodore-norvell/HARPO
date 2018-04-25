package cBackEnd

import frontEnd._
import frontEnd.AST._
import java.io._
import checker.Checker
import checker.CheckerTypes._
/**
 * @author nhacnguyen
 */
object ExpCodeGen {
  def apply(init : InitExpNd, name : String) : String = {
    val ecg = new ExpCodeGen()
    ecg.build(init, name)
  }
  
  def apply(init : InitExpNd, name : String, cName : String) : String = {
    val ecg = new ExpCodeGen()
    ecg.currentClass = cName
    ecg.build(init, name)
  }
  def apply(exp : ExpNd) : String = {
    val ecg = new ExpCodeGen()
    ecg.build(exp)
  }
}

class ExpCodeGen {
  var currentClass : String = ""
  
  def build(init : InitExpNd, name : String) : String = {
    val NM = NameManager
    val mName = NM.mkName(name)
    val result : String = init match{
      case ValueInitExpNd( exp : ExpNd ) => build(exp)                       
      case NewInitExpNd( ty : TypeNd, args : List[ExpNd] ) =>
        var param = ""
        if (!currentClass.equals("")) param = NM.mkClassParaName(currentClass) + "->"
        val argsLs = List("&" + param + mName) ++ args.map(build(_))
        List(NM.mkClassConstructorName(ty.toString) + "(", argsLs mkString ",", ");\n") mkString " "
 
      case ArrayInitExpNd( forDecl : ForDecl, bound : ExpNd, a : InitExpNd ) =>
        val mDeclName = NM.mkName(forDecl.fvd.name) //TODO Why not the fqn? TSN 2015 Aug 23
        val n = mName + "[" + mDeclName + "]"
        var initExp = build(a, mName + "[" + mDeclName + "]");
        //TODO What is the point of the next line? TSN 2015 Aug 23
        if (!List("if ", "for ").exists(initExp.contains)) initExp = mName + "[" + mDeclName + "]" + " = " + initExp
        List("for (", mDeclName, " = 0;", mDeclName, "<", build(bound), ";", mDeclName, " ++ )\n", initExp, "\n") mkString " "
            
      case IfInitExpNd( guard : ExpNd, a : InitExpNd, b : InitExpNd ) =>
        var initExpA = build(a, name);
        var initExpB = build(b, name);
        if (!List("if ", "for ").exists(initExpA.contains)) initExpA = mName + " = " + initExpA
        if (!List("if ", "for ").exists(initExpB.contains)) initExpB = mName + " = " + initExpB
        List("if (", build(guard), ")", "{", initExpA, "}\n", "else {", initExpB, "}\n") mkString " "
            
      case WidenInitExpNd( a : InitExpNd ) => build(a, name)
      case _ => init.toString()
    }
    result
  }
  def build(exNd : ExpNd) : String = {
    val NM = NameManager
    val result : String = exNd match {
      case NoExpNd() => ""
      
      case IntLiteralExpNd( i : Long ) => i.toString()
     
      case FloatLiteralExpNd( x : Double ) => x.toString()
      
      case NameExpNd( name : NameNd ) =>
        var parentCode = ""
        name.decl.get.parent match {
          case Some(parDecl) =>
            if (!currentClass.equals(parDecl.name) && !currentClass.equals("") && parDecl.isInstanceOf[ClassLikeDeclNd]) parentCode = NameManager.mkClassParaName(currentClass) + "->" + NameManager.mkSuperName(parDecl.name) + "."
          case None => {}
        }
        parentCode + NM.mkName(name.toString())
      
      case BinaryOpExpNd( op : BinaryOperator, x : ExpNd, y : ExpNd ) => List("(", build(x), resBiOp(op), build(y), ")") mkString "" 
        
      case UnaryOpExpNd( op : UnaryOperator, x : ExpNd ) => resUnaOp(op) + build(x)
      
      //TODO case Conversion(x : ExpNd ) =>""
      
      case MemberExpNd( x : ExpNd, name : String ) => build(x) + "." + NM.mkName(name)
      
      //TODO case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd]) =>""
      
      case FetchExpNd( x : ExpNd) => build(x)
    
      case AsExpNd( x : ExpNd, _ ) => build(x)   // TODO insert type conversions?
      
      case _ => exNd.toString()
    }
    result
  }
  private def resBiOp(op:BinaryOperator)={
    val result = op match{
      //case ImpliesOp()=>
      case EquivOp=> "=="
      case OrOp=> "||"
      case AndOp=> "&&"
      case AddOp=> "+"
      case SubOp=> "-"
      case MulOp=> "*"
      case SlashDivOp=> "/"
      //case WordDivOp()=>
      //case RemOp()=>
      //case IndexOp()=>
      case _ => op.toString()
    }
    result
  }
  private def resUnaOp(op:UnaryOperator)={
    val result = op match {
        case NegativeOp => "-"
        case NotOp => "!"
    }
    result
  }
  private def resOpChain(op: ChainingOperator): String = {
    val result:String = op match {
      case EqualOp => "=="
      case NotEqualOp => "!="
      case LessOp => "<"
      case LessOrEqualOp => "<="
      case GreaterOp => ">"
      case GreaterOrEqualOp => ">="
    }
    result
  }

}
