package verifierTests

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
import boogieBackEnd.BoogieBackEnd
import util.OutputBuilder;
import java.io.File;
import java.io.PrintWriter;
import java.io.IOException ;
import sys.process._;
import scala.sys.process.ProcessBuilder;
import executive.HarpoToBoogieTranslator;
import util.OutputBuilder
import executive.HarpoToBoogieTranslator
import parser.HarpoParser
import frontEnd.StandardErrorRecorder
import frontEnd.AST
import frontEnd.ErrorRecorder
import scala.collection.script.Message

class TestsBase extends FlatSpec with BeforeAndAfterEach {

    override def beforeEach( td : TestData ) {
        println( ">>>>>>>>>>>>>Starting " + td.name + " >>>>>>>>>>>>>>>>>>" )
    }

    override def afterEach( td : TestData ) {
        println( "<<<<<<<<<<<<<Finished " + td.name + " <<<<<<<<<<<<<<<<<" )
    }

    def tryWithParser( str : String, expectedFatalErrors : Int = 0, expectedWarningErrors : Int = 0 ) =
        tryWith( str, expectedFatalErrors, expectedWarningErrors, false )

    def tryWithChecker( str : String, expectedFatalErrors : Int = 0, expectedWarningErrors : Int = 0 ) =
        tryWith( str, expectedFatalErrors, expectedWarningErrors, true )

    def tryWithBoogieBackEnd( str : String, expectedFatalErrors : Int = 0, expectedWarningErrors : Int = 0 ) : String = {

        println( "\n\n\nVerifier start" )
        val verify = new HarpoToBoogieTranslator()
        verify.addFile( "HarpoSourceCode.harpo", str )
        val ( errorRecorder, transBuffer ) : (ErrorRecorder, OutputBuilder) = verify.runHarpoToBoogieTrans( true )

        assertResult( expectedFatalErrors )( errorRecorder.getFatalCount() )
        assertResult( expectedWarningErrors )( errorRecorder.getWarningCount() )

        if ( errorRecorder.getFatalCount() == 0 ) {
            transBuffer.newLine
            val text : String = transBuffer.result().mkString( "\n" )
            println( text )
            val writer = new PrintWriter( new File( "BoogieOutputScript.bpl" ) )
            writer.write( text )
            writer.close()

            val command = "boogie BoogieOutputScript.bpl";
            /*
           	* without c process will hang
           	* ! will give back result
           	* !! gives back output in result
           	*/
            val standardOut : ArrayBuffer[String] = new ArrayBuffer[String]() ;
            val standardErr : ArrayBuffer[String] = new ArrayBuffer[String]() ;
            val logger = ProcessLogger( ( out ) => standardOut += out, ( err ) => standardErr += err )
            try {
                val resultCode = Process( command ).!( logger )
                
                println( " ================== Boogie Verifier Standard Output Begin======================" )
                println( standardOut.mkString("\n") )
                println( "================== Boogie Verifier Standard Error Begin======================" )
                println( standardErr.mkString("\n")  )
                println( " ================== Boogie Verifier Results End======================" )
            } catch {
                case e : IOException => 
                    println( "Boogie execution bombed: Error is ", e.getMessage() )
                    println( "PATH is " + System.getenv("PATH") )
                    throw e ;
            }
            text 
        } else {
            "Fatal errors"
        }
    }

    def tryWith( str : String, expectedFatalErrors : Int, expectedWarningErrors : Int, runChecker : Boolean ) : StandardErrorRecorder = {

        // Build the builder and the parser
        val errorRecorder = new StandardErrorRecorder()
        val reader = new StringReader( str )
        val p : HarpoParser = new HarpoParser( reader )
        val builder = new frontEnd.Builder( errorRecorder );
        p.setBuilder( builder )

        // Run the parser.

        val dl : frontEnd.AST.DeclList =
            try { p.Start().asInstanceOf[frontEnd.AST.DeclList] }
            catch {
                case ex : ParseException => {
                    val coord = if ( ex.currentToken != null ) AST.Coord( "fileName", ex.currentToken.beginLine, ex.currentToken.beginColumn )
                    else AST.Coord( "fileName" );
                    errorRecorder.reportFatal( ex.getMessage(), coord )
                    null
                }
                case ex : TokenMgrError => {
                    val coord = AST.Coord( "fileName" )
                    errorRecorder.reportFatal( ex.getMessage(), coord )
                    null
                }
            }

        // Output all errors
        if ( errorRecorder.getTotalErrorCount() > 0 ) {
            println( "-----------------------------" );
            for ( i <- 0 until errorRecorder.getTotalErrorCount() )
                println( errorRecorder.getErrorCoord( i ) + " " + errorRecorder.getErrorText( i ) );
        }

        if ( runChecker ) {
            assert( errorRecorder.getFatalCount() == 0, "1st pass error prevents checker from running." )
            val checker = new Checker( errorRecorder )
            try {
                checker.runChecker( dl )
            } // The only exception that the checker should throw is a Bail.
            catch {
                case e : CompilerBailOutException =>
                    println( "----The checker has bailed---" );
            }

            // Output the tree.
            println( "----The AST after checking---" );
            println( dl.format( 80 ) )
            println
            println( "---------------------------------------" );

            // Output all errors
            if ( errorRecorder.getTotalErrorCount() > 0 ) {
                println( "-----------------------------" )
                for ( i <- 0 until errorRecorder.getTotalErrorCount() )
                    println( errorRecorder.getErrorCoord( i ) + " " + errorRecorder.getErrorText( i ) );
            }
        }
        assertResult( expectedFatalErrors )( errorRecorder.getFatalCount() )
        assertResult( expectedWarningErrors )( errorRecorder.getWarningCount() )
        errorRecorder
    }
}
