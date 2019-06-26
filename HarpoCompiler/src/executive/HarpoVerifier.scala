package executive
import java.io.File;
import java.io.PrintWriter;
import java.net.URL;

import scala.collection.mutable.ArrayBuffer
import scala.io.Source;
import sys.process._;
import scala.sys.process.ProcessBuilder;
import util.OutputBuilder;

import boogieBackEnd.VerificationReport;
import boogieBackEnd.BoogieErrorParser;

object HarpoVerifier extends App {

  def getHarpoSource(): String = {
    val sourceUrl: URL = this.getClass().getResource("/ioSourceFiles/HarpoSourceCode.text")
    val harpoSource = new File(sourceUrl.toURI())
    val contents = Source.fromFile(harpoSource)
    val sourceString = try contents.mkString finally contents.close()
    return sourceString
  }
  
  
  val verbose : Boolean = true
  println( "\n\n\nVerifier start" )
  var translator = new HarpoToBoogieTranslator()
  translator.addFile( "HarpoSourceCode.harpo", getHarpoSource() )
  val errorRecorder = translator.translateAndVerify( verbose )
    
  errorRecorder.printErrors( System.out )
}


