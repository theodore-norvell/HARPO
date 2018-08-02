package boogieBackEnd
import java.net.URL
import scala.io.Source
import java.io.OutputStreamWriter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import frontEnd.ErrorRecorder
import frontEnd.AST
import frontEnd.AST._
import java.net.URL
import scala.io.Source


class BoogieBackEnd(val masterDeclList : frontEnd.AST.DeclList) {
private def getBoogieCode() : String = {
  println(masterDeclList.toString());
  val boogieCode = genBoogiePrelude() + genDeclCode( masterDeclList )
  println("getBoogieCode Running")
  println(boogieCode);
  return boogieCode.toString();
  
}   
private def genBoogiePrelude() : String = {
  val filename = "BoogiePrelude.txt"
  val boogiePrelude = Source.fromFile(filename).getLines.mkString;
  println(boogiePrelude)
  return boogiePrelude
}

private def genDeclCode( dl : DeclList) : String = {
  var globalObjCode = ""
  var initializeCode = ""
  var nameTbl = HashMap[String, HashMap[String, String]]()
  var boogieCode = ""
  
  for(dNd: DeclNd <- dl.decls){ // declarations set
    dNd match{
       case ClassDeclNd() =>
       {
//        val code = "type className; \n function dtype(Ref) returns (className); \n const unique " + dNd.Name + " :className\n";
//        boogieCode += code
       }
       case ObjDeclNd( isConst, acc, ty, init) =>
       {
        val code = "\n const unique " + dNd.fqn + " :Field "+ ty; //inject name of Node
        boogieCode += code
        //Assign initialization expression
       }    
//      case AssertCmdNd() =>
//       {
//        
//       }
//      case ThreadDeclNd( block ) =>
//        {
//          ()
//        }
      
      case _ => {}
    }
  }
  
  //initializeCode = "void initialize(){\n" + initializeCode + "}\n"     
  //cCode + initializeCode + "\n"
  return boogieCode
  
}

}