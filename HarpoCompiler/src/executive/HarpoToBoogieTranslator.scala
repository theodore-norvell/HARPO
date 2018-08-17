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

class HarpoToBoogieTranslator {
    
    private var errorRecorder = new StandardErrorRecorder()
    
    private var boogieOutput = "No Output"
        
    def getErrorReport() : ErrorReport = errorRecorder 
    
    def getBoogieOutput() : String = boogieOutput
    
    private var files = new ArrayBuffer[(String,String)] 
    
    def addFile( fileName : String, fileText : String ) {
        files += ((fileName, fileText))
    }
    
    def runTranslator( ) {
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
        println("Master Declaration List : ")
        println( masterDeclList.format(80) )
        if( errorRecorder.getFatalCount() == 0) {
             println("I reached here");
            val checker = new Checker( errorRecorder )
            try { checker.runChecker( masterDeclList ) }
            // The only exception that the checker should throw is a CompilerBailOutException.
            catch{ case e : CompilerBailOutException => () } }
        errorRecorder.printErrors( Console.out ) ;
        // The boogie code generator
        if( errorRecorder.getFatalCount() == 0) {
            val boogieCodeGen=new BoogieBackEnd(masterDeclList)
           boogieOutput= boogieCodeGen.getBoogieCode();
            }
    }
}