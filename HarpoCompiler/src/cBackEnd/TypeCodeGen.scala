package cBackEnd

import frontEnd._

import frontEnd.AST._
import java.io._
import checker.Checker
import checker.CheckerTypes._
import contracts.Contracts._ 

/**
 * @author nhacnguyen
 */
object TypeCodeGen {
    
  private def build(ty:TypeNd):(String, String) = {
    check(ty.tipe.isInstanceOf[Some[Type]])
    val baseType = extractBaseType(ty.tipe.get)
    
    var codeType : String = 
        if (baseType.isInstanceOf[PrimitiveType] ) {
            val primType = baseType.asInstanceOf[PrimitiveType]
            check( BuilderTypes.hToCTypeMap.contains( primType ) )
            BuilderTypes.hToCTypeMap(primType) }
        else if( baseType.isInstanceOf[ClassIntfType] ) {
            NameManager.mkName(ty.toString()) }
        else {
            unreachable("Bad base type in TypeCodeGen." ) ; }
    
    return (codeType, resBound(ty.tipe.get))
  }
    
  private def extractBaseType(t : Type) : Type = {
      t match{
        case ClassIntfType( decl : AST.ClassLikeDeclNd ) => t

        case PrimitiveType( name : QN ) => t
        
        //case UniversalType( typeVar : TypeVar, body : Type )
        
        //case AppliedType( typeFunc : Type, typeArg : Type)
        
        case ArrayType( base : Type, bound : AST.ExpNd ) =>  extractBaseType(base)
        
        //case TopType()
      
        //case MethodType( paramList : List[Parameter] )
        
        //case TypeVar( name : QN, bound : Type )
        
        case LocationType( base : PrimitiveType ) => extractBaseType(base)
        
        case _ => unreachable("Unexpected type in extractBaseType.")
    }
  }
  
  private def resBound(t : Type) : String = {
    // TODO. Can this possibly work for multidimensional arrays?
    t match {
      case ArrayType(base : Type, bound : AST.ExpNd) => "[" + ExpCodeGen(bound) + "]"
      case _ => ""
    }
  }
  
  def apply(ty : TypeNd) : (String, String) = {
    build(ty)
  }
}