package executive
import scala.collection.mutable.ArrayBuffer;
import java.io.PrintWriter;
import java.io.File;
import frontEnd._
import parser._ 
import checker.Checker ;
import scala.collection.mutable.ArrayBuffer 
import java.io.StringReader 
import boogieBackEnd.BoogieBackEnd
import util.OutputBuilder;

class HarpoToBoogieTranslator {
    
    private var errorRecorder = new StandardErrorRecorder()
    
    private var boogieOutput = "No Output"
    
        
    def getErrorReport() : ErrorReport = errorRecorder 
    
    def getBoogieOutput(outputBuffer:OutputBuilder) : OutputBuilder = {
      outputBuffer
    }
    
    private var files = new ArrayBuffer[(String,String)] 
    
    def addFile( fileName : String, fileText : String ) {
        files += ((fileName, fileText))
    }
    
    def runHarpoToBoogieTrans( outputBuffer: OutputBuilder) : OutputBuilder ={
      
        var transOutBuffer = new OutputBuilder 
        errorRecorder = new StandardErrorRecorder()
        println("Translator is running")
        boogieOutput = ""
        val masterDeclList  = new frontEnd.AST.DeclList() 
        
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
            	  println(fileName,fileText,dl)
                if( dl != null ) {
                    for( decl <- dl.decls ) {
                        masterDeclList.addDeclaration(decl) 
                        } }
        }

        println("================== Begin: Master Declaration List ====================== \n\n")
        println( masterDeclList.format(80) )
        println(" ================== End: Master Declaration List ======================\n\n")
        if( errorRecorder.getFatalCount() == 0) {
             println("Checker Start");
            val checker = new Checker( errorRecorder )
            try { checker.runChecker( masterDeclList ) }
            // The only exception that the checker should throw is a CompilerBailOutException.
            catch{ case e : CompilerBailOutException => () } }
        errorRecorder.printErrors( Console.out ) ;
        // The boogie code generator
        if( errorRecorder.getFatalCount() == 0) {
           println("Fetal Errors= ", errorRecorder.getFatalCount());
           println(" ================== Begin: Master Declaration List after Checking ====================== \n\n")
           println( masterDeclList.format(80) )
           println(" ================== End: Master Declaration List after Checking ======================\n\n")
           val boogieCodeGen=new BoogieBackEnd(masterDeclList, outputBuffer)
           transOutBuffer = boogieCodeGen.getBoogieCode();
           }
        transOutBuffer
    }
}