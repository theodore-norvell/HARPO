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
    val verbose : Boolean = true
    println( "\n\n\nVerifier start" )
    var translator = new HarpoToBoogieTranslator()
    translator.addFile( "HarpoSourceCode.harpo", getHarpoSource() )
    val ( errorRecorder, transBuffer ) = translator.runHarpoToBoogieTrans( verbose )

    if ( errorRecorder.getFatalCount() == 0 ) {
        transBuffer.newLine
        val text : String = transBuffer.result().mkString( "\n" )
        val vr : VerificationReport = translator.runVerifer( text, verbose )
        // TODO process the report.
    }

    errorRecorder.printErrors( System.out )
    
    println( "\n\n\nVerifier end" )

    def getHarpoSource() : String = {
        val sourceUrl : URL = this.getClass().getResource( "/ioSourceFiles/HarpoSourceCode.text" )
        val harpoSource = new File( sourceUrl.toURI() )
        val contents = Source.fromFile( harpoSource )
        val sourceString = try contents.mkString finally contents.close()
        return sourceString
    }
}


