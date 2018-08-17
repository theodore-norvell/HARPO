package cBackEnd

import frontEnd.AST
import frontEnd.AST._
import scala.collection.mutable.HashMap

/**
 * @author nhacnguyen
 */
class ExpEvaluator(var cTable:HashMap[String,String]) {
  def setConstTable(t:HashMap[String,String])=this.cTable=t
  
  def getInitValue(init:InitExpNd,ty:String):String={
    init match {
      case ValueInitExpNd( exp : ExpNd )=>
        ty match {
          case "int8"|"int16"|"int32"|"int64"=> getIntValue(exp).toString()
          case "real32"|"real64" => getRealValue(exp).toString()
          case "bool" => getBoolValue(exp).toString()
          case _ => "" 
        }
      
      case NewInitExpNd( ty : TypeNd, args : List[ExpNd] )=>
        val argLs=args.map(getIntValue(_))
        ty.toString()+"("+(argLs mkString ",")+")"
      
      
      case ArrayInitExpNd( forDecl : ForDecl, bound : ExpNd, a : InitExpNd )=>
        var arr=List[String]()
        val bnd=getIntValue(bound).toInt
        val name=forDecl.fvd.name 
        for (i<-0 until bnd) {
          cTable(name)=i.toString()
          arr=arr:+getInitValue(a,ty)
        }
        cTable.remove(name)
        "{"+(arr mkString ",")+"}"
        
      case IfInitExpNd( guard : ExpNd, a : InitExpNd, b : InitExpNd )=>
        val g=getBoolValue(guard)
        if (g) getInitValue(a,ty) else getInitValue(b,ty)       
      
      case _ => ""
    }
  }
  def getRealValue(exp:ExpNd):Double={
    val result:Double= exp match {
      //case NoExpNd()(coord : AST.Coord) extends ExpNd( coord )
  
      case IntLiteralExpNd( i : Long )=> i
      
      case FloatLiteralExpNd( x : Double )=> x
      
      case NameExpNd( name : NameNd )=>cTable(name.toString()).toDouble
      
      case BinaryOpExpNd( op : BinaryOperator, x : ExpNd, y : ExpNd )=>
        op match {
          //case ImpliesOp()
          //case EquivOp()
          //case OrOp()
          //case AndOp()
          case AddOp=>getRealValue(x)+getRealValue(y)
          case SubOp=>getRealValue(x)-getRealValue(y)
          case MulOp=>getRealValue(x)*getRealValue(y)
          case SlashDivOp=>getRealValue(x)/getRealValue(y)
          //case WordDivOp()
          //case RemOp()
          //case IndexOp()
          case _ => 0
        }
      
      case UnaryOpExpNd( op : UnaryOperator, x : ExpNd )=>
        op match{
          case NegativeOp=> -getRealValue(x)
          case NotOp=>0
        }
      
      //case Conversion(x : ExpNd )     
      //case MemberExpNd( x : ExpNd, name : String )      
      //case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd])     
      case _ => 0
    }
    result
  }
  
   def getIntValue(exp:ExpNd):Long={
    val result:Long= exp match {
      
      //case NoExpNd()(coord : AST.Coord) extends ExpNd( coord ) 
      case IntLiteralExpNd( i : Long )=> i    
      //case FloatLiteralExpNd( x : Double )=> x
      
      case NameExpNd( name : NameNd )=>
        //println(cTable+name.toString())
        cTable(name.toString()).toLong
      
      case BinaryOpExpNd( op : BinaryOperator, x : ExpNd, y : ExpNd )=>
        op match {
          //case ImpliesOp()
          //case EquivOp()
          //case OrOp()
          //case AndOp()
          case AddOp=>getIntValue(x)+getIntValue(y)
          case SubOp=>getIntValue(x)-getIntValue(y)
          case MulOp=>getIntValue(x)*getIntValue(y)
          case SlashDivOp=>getIntValue(x)/getIntValue(y)
          //case WordDivOp
          //case RemOp()
          //case IndexOp()
          case _ => 0
        }
      
      case UnaryOpExpNd( op : UnaryOperator, x : ExpNd )=>
        op match{
          case NegativeOp=> -getIntValue(x)
          case NotOp=>0
        }
      
      //case Conversion(x : ExpNd )      
      //case MemberExpNd( x : ExpNd, name : String )  
      //case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd]
      case _ => 0
    }
    result
  }
   
   def getBoolValue(exp:ExpNd):Boolean={
    val result:Boolean= exp match {
      
      //case NoExpNd()(coord : AST.Coord) extends ExpNd( coord ) 
      case IntLiteralExpNd( i : Long )=> if (i>0) true else false 
      //case FloatLiteralExpNd( x : Double )=> x
      
      case NameExpNd( name : NameNd )=>cTable(name.toString()).toBoolean
      
      case BinaryOpExpNd( op : BinaryOperator, x : ExpNd, y : ExpNd )=>
        op match {
          //case ImpliesOp()
          //case EquivOp()
          case OrOp=>getBoolValue(x) || getBoolValue(y)
          case AndOp=>getBoolValue(x) && getBoolValue(y)
          //case AddOp()=>getIntValue(x)+getIntValue(x)
          //case SubOp()=>getIntValue(x)+getIntValue(x)
          //case MulOp()=>getIntValue(x)+getIntValue(x)
          //case SlashDivOp()=>getIntValue(x)+getIntValue(x)
          //case WordDivOp
          //case RemOp()
          //case IndexOp()
          case _ => false
        }
      
      case UnaryOpExpNd( op : UnaryOperator, x : ExpNd )=>
        op match{ 
          case NotOp=> !getBoolValue(x)
          case _ => false
        }
      
      //case Conversion(x : ExpNd )      
      //case MemberExpNd( x : ExpNd, name : String )  
      //case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd]
      case _ => false
    }
    result
  }
}
