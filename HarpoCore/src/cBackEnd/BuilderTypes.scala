package cBackEnd

import frontEnd.AST.ExpNd
import scala.collection.mutable.HashMap
import checker.CheckerTypes.PrimitiveType

object BuilderTypes {
  // primitive type conversion for the two ExpNds in BiOpration
  val indexMap = 
HashMap("::Int8"->0,"::Int16"->1,"::Int32"->2,"::Int64"->3,"::Real16"->4,"::Real32"->5,"::Real64"->6,"::Bool"->7)
  val hToCTypeMap = 
HashMap(checker.CheckerTypes.int8->"Int8",
		checker.CheckerTypes.int16->"Int16",
		checker.CheckerTypes.int32->"Int32",
		checker.CheckerTypes.int64->"Int64",
		checker.CheckerTypes.real16->"float",
		checker.CheckerTypes.real32->"float",
    checker.CheckerTypes.real64->"double",
		checker.CheckerTypes.bool->"bool")
  
  val biOpConvTable =
    Array(Array("int8", "int16", "int32", "int64", "real16", "real32", "real64", "-"),
      Array("int16", "int16", "int32", "int64", "real32", "real32", "real64", "-"),
      Array("int32", "int32", "int32", "int64", "real64", "real64", "real64", "-"),
      Array("int64", "int64", "int64", "int64", "-", "-", "-", "-"),
      Array("real16", "real32", "real64", "-", "real16", "real32", "real64", "-"),
      Array("real32", "real32", "real64", "-", "real32", "real32", "real64", "-"),
      Array("real64", "real64", "real64", "-", "real64", "real64", "real64", "-"),
      Array("-", "-", "-", "-", "-", "-", "-", "bool"))
  def biOpTypeConvert(x:ExpNd,y:ExpNd)= 
  { 
    //assert x.isPrimitive && y.isPrimitive
// indexMap.get(x.tipe.asInstanceOf[PrimitiveType].name)
    
   
    
  
  }

}