package executive
import java.io.File;
import java.io.PrintWriter;
import java.net.URL;
import scala.io.Source;

object Compile extends App {
  println("\n\n\nVerifier start")
  var verify = new HarpoToBoogieTranslator()
  verify.addFile("mathClass.harpo",getHarpoSource())
  verify.runTranslator()
  var boogieScript:String = verify.getBoogieOutput()
  println("Boogie Script\n\n\n"+boogieScript)
  val writer = new PrintWriter(new File("BoogieOutputScript.txt"))
  writer.write(boogieScript)
  writer.close()
  println("\n\n\nVerifier end") 
  
  def getHarpoSource():String = {      
        	val sourceUrl : URL = this.getClass().getResource("/ioSourceFiles/mathClass-harpo.text")
          val harpoSource = new File(sourceUrl.toURI())
          val contents = Source.fromFile(harpoSource)
          val sourceString = try contents.mkString finally contents.close()
          return sourceString
}
}


