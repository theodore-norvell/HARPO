package executive

import frontEnd._
import parser._ 
import checker.Checker ;
import scala.collection.mutable.ArrayBuffer 

import java.io.StringReader 
import cBackEnd.CBackEnd

class HarpoToCCompiler {
    private var errorRecorder = new StandardErrorRecorder()
    
    private var cOutput = ""
        
    def getErrorReport() : ErrorReport = errorRecorder 
    
    def getCOutput() : String = cOutput
    
    private var files = new ArrayBuffer[(String,String)] 
    
    private var fileContents = new ArrayBuffer[String]
    
    def addFile( fileName : String, fileText : String ) {
        files += ((fileName, fileText))
    }

	  def runCompiler( ) {
        errorRecorder = new StandardErrorRecorder()
        cOutput = ""
        
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
                if( dl != null ) {
                    for( decl <- dl.decls ) {
                        masterDeclList.addDeclaration(decl) } }
        }
            
        // Run the checker 
            
        if( masterDeclList != null && errorRecorder.getFatalCount() == 0) {
            val checker = new Checker( errorRecorder )
            try { checker.runChecker( masterDeclList ) }
            // The only exception that the checker should throw is a CompilerBailOutException.
            catch{ case e : CompilerBailOutException => () } }
        
        // To do. Add the C code generator
        if( masterDeclList != null && errorRecorder.getFatalCount() == 0) {
            val cCodeGen=new CBackEnd(masterDeclList)
            cOutput=cCodeGen.getCCode() }
	}
}




