package boogieBackEnd

import frontEnd.AST._
import java.io._
import checker.Checker
import checker.CheckerTypes._
import contracts.Contracts._ 

import frontEnd._

import frontEnd.AST._
import java.io._
import checker.Checker
import checker.CheckerTypes._
import contracts.Contracts._ 

/**
 * @author Inaam Ahmed
 */
object TypeCodeGen {
    
  private def build(ty:TypeNd): String = {
    check(ty.tipe.isInstanceOf[Some[Type]])
    val baseType = extractBaseType(ty.tipe.get)
    
    var codeType : String = 
        if (baseType.isInstanceOf[PrimitiveType] ) {
            val primType = baseType.asInstanceOf[PrimitiveType]
            check( BuilderTypes.hToBoogieTypeMap.contains( primType ) )
            BuilderTypes.hToBoogieTypeMap(primType) }
        else {
            unreachable("Bad base type in TypeCodeGen." ) ; }
    
    return codeType
  }
    
  private def extractBaseType(t : Type) : Type = {
      t match{
        case ClassIntfType( decl : AST.ClassLikeDeclNd ) => t

        case PrimitiveType( name : QN ) => t
        
        case ArrayType( base : Type, bound : AST.ExpNd ) =>  extractBaseType(base)
        
        case LocationType( base : PrimitiveType ) => extractBaseType(base)
        
        case _ => unreachable("Unexpected type in extractBaseType.")
    }
  }
  
  def apply(ty : TypeNd) : String = {
    build(ty)
  }
}