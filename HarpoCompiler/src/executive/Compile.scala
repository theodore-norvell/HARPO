package executive
import java.io.File;
import java.io.PrintWriter;
import java.net.URL;
import scala.io.Source;
import sys.process._;

object Compile extends App {
  println("\n\n\nVerifier start")
  var verify = new HarpoToBoogieTranslator()
  verify.addFile("HarpoSourceCode.harpo",getHarpoSource())
  verify.runTranslator()
  var boogieScript:String = verify.getBoogieOutput()
  println("Boogie Script\n\n\n"+boogieScript)
  val writer = new PrintWriter(new File("BoogieOutputScript.bpl"))
  writer.write(boogieScript)
  writer.close()
  
  val command = "cmd.exe /c Boogie \"BoogieOutputScript.bpl\"";
  /*
   * without c process will hang
   * ! will give back result
   * !! gives back output in result
   */
  val result = Process(command).!! 
  println(" ================== Boogie Verifier Results Begin====================== \n\n")
  println(result)
  println(" \n\n================== Boogie Verifier Results End======================")
  
 
  println("\n\n\nVerifier end")
  
  def getHarpoSource():String = {      
        	val sourceUrl : URL = this.getClass().getResource("/ioSourceFiles/HarpoSourceCode.text")
          val harpoSource = new File(sourceUrl.toURI())
          val contents = Source.fromFile(harpoSource)
          val sourceString = try contents.mkString finally contents.close()
          return sourceString
}
}


