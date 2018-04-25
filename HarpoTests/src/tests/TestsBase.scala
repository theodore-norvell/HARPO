package tests

import org.scalatest.FlatSpec
import org.scalatest.TestData
import org.scalatest.BeforeAndAfterEach
import org.scalatest.junit.JUnitRunner
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
import cBackEnd.CBackEnd

class TestsBase extends FlatSpec with BeforeAndAfterEach {
    
    override def beforeEach(td: TestData) {   
        println(">>>>>>>>>>>>>Starting " + td.name+" >>>>>>>>>>>>>>>>>>" )
    }

    override def afterEach(td: TestData) { 
        println("<<<<<<<<<<<<<Finished " + td.name + " <<<<<<<<<<<<<<<<<")
    }
    
  def tryWithParser( str : String, expectedFatalErrors : Int = 0, expectedWarningErrors : Int = 0, runChecker : Boolean = false ) =
    tryWith( str, expectedFatalErrors, expectedWarningErrors, false, false ) 
   
  def tryWithChecker( str : String, expectedFatalErrors : Int = 0, expectedWarningErrors : Int = 0 ) =
      tryWith( str, expectedFatalErrors, expectedWarningErrors, true, false)
 
  def tryWithCBackEnd( str : String, expectedFatalErrors : Int = 0, expectedWarningErrors : Int = 0, runChecker : Boolean = true) =
      tryWith( str, expectedFatalErrors, expectedWarningErrors, runChecker,true)
      
  def tryWith( str : String, expectedFatalErrors : Int, expectedWarningErrors : Int, runChecker : Boolean, runCBackEnd : Boolean )  : StandardErrorRecorder = {

    // Output the input
    println( str ) ;
    // Build the builder and the parser
    val errorRecorder = new StandardErrorRecorder()
    val reader  = new StringReader(str) 
    val p : HarpoParser = new HarpoParser( reader )
    val builder = new frontEnd.Builder(errorRecorder) ;
    p.setBuilder( builder )
    
    // Run the parser.
    
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
    
    // Output the tree.
    if( dl != null ) {
        println("----The AST after parsing-------------\n")
        println( dl.format( 80 ) )
    }
    println
    println("---------------------------------------") ;
    
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

    if( runCBackEnd ) {
        assert(errorRecorder.getFatalCount() == 0, "Checking error prevents C back end from running.")
        if( dl != null ) {
            println("-----------C Code  generated-------------\n")
            val cCodeGen=new CBackEnd(dl)
            println(cCodeGen.getCCode())       
        }
    }
    
    assertResult(expectedFatalErrors) ( errorRecorder.getFatalCount()) 
    assertResult( expectedWarningErrors )( errorRecorder.getWarningCount() )
    errorRecorder
  }

}