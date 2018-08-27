package boogieBackEnd
import java.net.URL
import scala.io.Source
import java.io.OutputStreamWriter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import frontEnd.ErrorRecorder
import frontEnd.AST
import frontEnd.AST._
import scala.io.Source
import checker.CheckerTypes.PrimitiveType;
import java.io.File

class BoogieBackEnd(val masterDeclList : frontEnd.AST.DeclList) {

	def getBoogieCode():String = {
			println(masterDeclList.toString());
			val boogieCode = getBoogiePrelude() + genDeclCode( masterDeclList )
			return boogieCode.toString();
	}   
	private def getBoogiePrelude():String = {      
        	val preludeUrl : URL = this.getClass().getResource("/boogieBackEnd/BoogiePrelude.txt")
          val prelude = new File(preludeUrl.toURI())
          val contents = Source.fromFile(prelude)
          val boogiePrelude = try contents.mkString finally contents.close()
          return boogiePrelude
}

	private def genDeclCode( dl : DeclList):String = {
			    var globalObjCode = ""
					var initializeCode = ""
					var nameTbl = HashMap[String, HashMap[String, String]]()
					var boogieCode = ""
						for(dlNd : DeclNd <- dl.decls) {
						dlNd match{          
						case ClassDeclNd() => {
						  val classCode=new ClassCodeGen();
						  boogieCode += classCode.getClassCode(dlNd)
						}
						case IntfDeclNd() => {
						  val intfCode=new IntfCodeGen();
						  boogieCode += intfCode.getIntfCode(dlNd)
						}
						case _ => val code = "No Declarations Found"
						}
					} 
			return boogieCode;
  }
}