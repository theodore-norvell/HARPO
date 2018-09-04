package verifierTests
import org.scalatest.FlatSpec
import org.scalatest.TestData
import org.scalatest.BeforeAndAfterEach
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.Assertions._
import java.io.Reader
import java.io.StringReader
import java.io.OutputStreamWriter
import frontEnd.ErrorRecorder
import frontEnd.StandardErrorRecorder
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.Range
import frontEnd.AST
import parser.HarpoParser
import parser.ParseException
import parser.TokenMgrError
import checker.Checker 
import frontEnd.CompilerBailOutException
import boogieBackEnd.BoogieBackEnd

class BufferSystemTestsBase extends FlatSpec{
 
  var tranSource: String = ""
  def tryWithBoogieBackEnd( str : String, expectedFatalErrors : Int = 0, expectedWarningErrors : Int = 0, runChecker : Boolean = true) : String = 
    { 
    val boogieBuffer = tryWith( str, expectedFatalErrors, expectedWarningErrors, runChecker,true)
    boogieBuffer
    }

  def tryWith( str : String, expectedFatalErrors : Int, expectedWarningErrors : Int, runChecker : Boolean, runBoogieBackEnd : Boolean ): String = 
  {    
    // Output the input
    println("---------------Harpo Source Begins----------------")
    println(str) ;
    println("---------------Harpo Source Ends----------------")
    
    // Build the builder and the parser
    val errorRecorder = new StandardErrorRecorder()
    val reader  = new StringReader(str) 
    val p : HarpoParser = new HarpoParser( reader )
    val builder = new frontEnd.Builder(errorRecorder) ;
    p.setBuilder( builder )
    
    
    // Run the parser, return AST
    val dl : frontEnd.AST.DeclList =
        try { p.Start().asInstanceOf[frontEnd.AST.DeclList] }
        catch{ case ex : ParseException => {    
                   val coord = if( ex.currentToken != null ) AST.Coord( "fileName", ex.currentToken.beginLine, ex.currentToken.beginColumn) 
                               else AST.Coord( "fileName") ;
                   errorRecorder.reportFatal(ex.getMessage(), coord)
                   null }
               case ex : TokenMgrError => {    
                   val coord = AST.Coord( "fileName") 
                   errorRecorder.reportFatal(ex.getMessage(), coord)
                   null }
                }
        
  
    // Output all errors
    if( errorRecorder.getTotalErrorCount() > 0 ) {
        println("-----------------------------") ;
        for( i <- 0 until errorRecorder.getTotalErrorCount() )
            println( errorRecorder.getErrorCoord(i) + " " + errorRecorder.getErrorText(i)) ;
    }
    
    
    // Output the Declaration List Tree
    if( dl != null ) {
        println("----The AST after parsing-------------\n")
        println( dl.format( 80 ) )
    }
    
    
    // Run the Checker, return attributed syntax tree
    if( runChecker ) {
        assert(errorRecorder.getFatalCount() == 0, "1st pass error prevents checker from running.")
        val checker = new Checker( errorRecorder )
        try {
            checker.runChecker( dl )
        }
        // The only exception that the checker should throw is a Bail.
        catch{ case e : CompilerBailOutException =>
                    println("----The checker has bailed---") ; }
        
    
    // Output the tree.
    println("----The AST after checking---") ;
    println( dl.format( 80 ) )
    println
    println("---------------------------------------") ;
    
    
    // Output all errors
    if( errorRecorder.getTotalErrorCount() > 0 ) {
        println("-----------------------------")
           for( i <- 0 until errorRecorder.getTotalErrorCount() )
                println( errorRecorder.getErrorCoord(i) + " " + errorRecorder.getErrorText(i) ) ; }
    }
    
    
    // Run Boogie BackEnd
    if( runBoogieBackEnd ) {
        assert(errorRecorder.getFatalCount() == 0, "Checking error prevents Boogie back end from running.")
        if( dl != null ) {
            println("-----------Boogie Code generated-------------\n")
            val boogieCodeGen=new BoogieBackEnd(dl)
            tranSource = boogieCodeGen.getBoogieCode()   // Comparsion
        }
    }
    tranSource
  }

}