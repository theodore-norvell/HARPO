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

  private def build(ty: TypeNd): String = {
    check(ty.tipe.isInstanceOf[Some[Type]])
    val baseType = extractBaseType(ty.tipe.get)
    var codeType: String =
      if (baseType.isInstanceOf[PrimitiveType]) {
        val primitiveType = baseType.asInstanceOf[PrimitiveType]
        check(BuilderTypes.hToBoogieTypeMap.contains(primitiveType))
        BuilderTypes.hToBoogieTypeMap(primitiveType)
      } else {
        unreachable("Bad base type in TypeCodeGen.");
      }
    return codeType
  }

  private def extractBaseType(tipe: Type): Type = {
    tipe match {
      case ClassIntfType(decl: AST.ClassLikeDeclNd) => tipe

      case PrimitiveType(name: QN) => tipe

      case ArrayType(base: Type, bound: AST.ExpNd) => extractBaseType(base)

      case LocationType(base: PrimitiveType) => extractBaseType(base)

      case ArrayLocationType(baseType: ArrayType) => extractBaseType(baseType)

      case _ => unreachable("Unexpected type in extractBaseType.")
    }
  }

  def apply(ty: TypeNd): String = {
    build(ty)
  }
}