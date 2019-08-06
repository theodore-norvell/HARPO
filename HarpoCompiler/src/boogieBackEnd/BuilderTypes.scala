package boogieBackEnd
import scala.collection.mutable.HashMap
import checker.CheckerTypes.PrimitiveType

object BuilderTypes {
  val hToBoogieTypeMap =
    HashMap(
              checker.CheckerTypes.int8 -> "int",
              checker.CheckerTypes.int16 -> "int",
              checker.CheckerTypes.int32 -> "int",
              checker.CheckerTypes.int64 -> "int",
              checker.CheckerTypes.real16 -> "real",
              checker.CheckerTypes.real32 -> "real",
              checker.CheckerTypes.real64 -> "real",
              checker.CheckerTypes.bool -> "bool"
           )
}

//Out of range integers for the Boogie, we should not just convert the int1,16,32,64 into int in Boogie