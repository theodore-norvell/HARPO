package executive

import java.io.File;
import java.io.PrintWriter;
import java.io.StringReader 
import scala.collection.mutable.ArrayBuffer 
import scala.sys.process.Process;
import scala.sys.process.ProcessLogger;

import frontEnd._
import parser._ 
import checker.Checker ;
import boogieBackEnd._
import util.OutputBuilder;

class HarpoToBoogieTranslator {
    
    private var files = new ArrayBuffer[(String,String)] 
    
    def addFile( fileName : String, fileText : String ) {
        files += ((fileName, fileText))
    }
    
    /** Translate the give files to Boogie.
     *  Postcondition: If the number of fatal errors is 0, the outputbuilder will not be null.
     */
    def runHarpoToBoogieTrans( verbose : Boolean ) : (ErrorRecorder, OutputBuilder) = {
        val errorRecorder = new StandardErrorRecorder()
        if( verbose ) println("Translator is running")
        val masterDeclList  = new frontEnd.AST.DeclList() 
        
        // Parsing pass
        for( (fileName, fileText) <- files ) {
                val reader  = new StringReader(fileText) 
                val p : HarpoParser = new HarpoParser( reader )
                p.setFileName(fileName);
                val builder = new frontEnd.Builder(errorRecorder) ;
                p.setBuilder( builder )
                // Run the parser.
    
                val dl : frontEnd.AST.DeclList =
                    try {
                        p.Start().asInstanceOf[frontEnd.AST.DeclList] }
                    catch{
                        case ex : ParseException => {    
                            val coord = if( ex.currentToken != null ) AST.Coord( fileName, ex.currentToken.beginLine, ex.currentToken.beginColumn) 
                                        else AST.Coord( fileName) ;
                            errorRecorder.reportFatal(ex.getMessage(), coord)
                            null }
                        case ex : TokenMgrError => {    
                            val coord = AST.Coord( fileName )//TODO add line and column number
                            errorRecorder.reportFatal(ex.getMessage(), coord)
                            null } }
                if( verbose ) println(fileName, fileText, dl)
                if( dl != null ) {
                    for( decl <- dl.decls ) {
                        masterDeclList.addDeclaration(decl) 
                        } }
        }
        if( verbose ) {
            println("================== Begin: Master Declaration List ====================== \n\n")
            println( masterDeclList.format(80) )
            println(" ================== End: Master Declaration List ======================\n\n")
        }
        
        // Checker passes
        if( errorRecorder.getFatalCount() == 0) {
            if( verbose ) println("Checker Start");
            val checker = new Checker( errorRecorder )
            try { checker.runChecker( masterDeclList ) }
            // The only exception that the checker should throw is a CompilerBailOutException.
            catch{ case e : CompilerBailOutException => () }
        }
        
        // The boogie code generator
        if( errorRecorder.getFatalCount() == 0) {
            if( verbose )  {
                println("Fatal Errors= ", errorRecorder.getFatalCount());
                println(" ================== Begin: Master Declaration List after Checking ====================== \n\n")
                println( masterDeclList.format(80) )
                println(" ================== End: Master Declaration List after Checking ======================\n\n")
            }
            val boogieCodeGen=new BoogieBackEnd()
            val transOutBuffer = boogieCodeGen.getBoogieCode(masterDeclList );
            (errorRecorder, transOutBuffer) ;
        }
        else {
            (errorRecorder, null) 
        }
    }

    def runVerifer( text : String, verbose : Boolean ) : VerificationReport = {
        if ( verbose ) {
            println( " ================== Boogie to Verify======================" )
            println( text )
            println( " ================== End of Boogie to Verify======================" )
        }
        val writer = new PrintWriter( new File( "BoogieOutputScript.bpl" ) )
        writer.write( text )
        writer.close()

        val command = "boogie BoogieOutputScript.bpl";
        val stdOut : ArrayBuffer[String] = new ArrayBuffer[String]
        val stdErr : ArrayBuffer[String] = new ArrayBuffer[String]
        val logger = ProcessLogger( ( line ) => stdOut += line, ( line ) => stdErr += line )
        if ( verbose ) { println( "Running boogie verifier" ) }
        val exitValue = try {
                Process( command ).!( logger )
            } catch {
                case e : Throwable => 
                    println( "Boogie execution bombed: Error is ", e.getMessage() )
                    println( "PATH is " + System.getenv("PATH") )
                    throw e ;
            }
        if ( verbose ) {
            println( "Exit value is " + exitValue ) 
            println( " ================== Boogie Verifier Standard Output Begin======================" )
            println( stdOut.mkString( "\n" ) )
            println( " ================== Boogie Verifier Standard Error Begin======================" )
            println( stdErr.mkString( "\n" ) )
            println( "================== Boogie Verifier Results End======================" )
        }    
        val parser = new BoogieErrorParser
        parser.parseBoogieOutput( stdOut )
    }

}
