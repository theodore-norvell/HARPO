package executive
import java.io.File;
import java.io.PrintWriter;
import java.net.URL;
import scala.io.Source;
import sys.process._;
import scala.sys.process.ProcessBuilder;
import util.OutputBuilder;

object Compile extends App {
  println("\n\n\nVerifier start")
  var verify = new HarpoToBoogieTranslator()
  verify.addFile("HarpoSourceCode.harpo",getHarpoSource())
  var outputBuffer : OutputBuilder = new OutputBuilder;
  var transBuffer : OutputBuilder = new OutputBuilder;
  transBuffer = verify.runHarpoToBoogieTrans(outputBuffer)
  transBuffer.newLine
  val text: String = transBuffer.result().mkString("\n")
  println(text)
  val writer = new PrintWriter(new File("BoogieOutputScript.bpl"))
		  writer.write(text)
      writer.close()
  
 // println("Boogie Script\n\n\n"+ boogieScript)
  val command = "cmd.exe /c boogie \"BoogieOutputScript.bpl\"";
  /*
   * without c process will hang
   * ! will give back result
   * !! gives back output in result
   */
  var log : String = "";
  
  var result : String = "";
  
  val logger = ProcessLogger( (out) => result = out, (err) => log = err )
  
  val output = Process( command ).! ( logger ) 

  val outputDes = Process(command).!!
  
  println(" ================== Boogie Verifier Results Begin====================== \n\n")
  println(output)
  println(result)
  println(outputDes)
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


