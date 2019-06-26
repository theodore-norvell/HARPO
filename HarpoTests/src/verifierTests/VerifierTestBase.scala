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
import java.io.File;
import java.io.PrintWriter;
import java.io.IOException ;

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.Range
import scala.collection.script.Message

import boogieBackEnd.BoogieBackEnd
import boogieBackEnd.VerificationReport
import checker.Checker
import frontEnd.AST
import frontEnd.ErrorRecorder
import frontEnd.StandardErrorRecorder
import frontEnd.CompilerBailOutException
import frontEnd.StandardErrorRecorder
import frontEnd.AST
import frontEnd.ErrorRecorder
import parser.HarpoParser
import parser.ParseException
import parser.TokenMgrError
import util.OutputBuilder;
import executive.HarpoToBoogieTranslator;
import util.OutputBuilder
import executive.HarpoToBoogieTranslator
import parser.HarpoParser

class VerifierTestBase extends FlatSpec with BeforeAndAfterEach {

    override def beforeEach( td : TestData ) {
        println( ">>>>>>>>>>>>>Starting " + td.name + " >>>>>>>>>>>>>>>>>>" )
    }

    override def afterEach( td : TestData ) {
        println( "<<<<<<<<<<<<<Finished " + td.name + " <<<<<<<<<<<<<<<<<" )
    }

    def tryWithBoogieBackEnd( str : String, expectedFatalErrors : Int = 0, expectedWarningErrors : Int = 0 ) : String = {

        println( "\n\n\nTranslation start" )
        val translator = new HarpoToBoogieTranslator()
        translator.addFile( "HarpoSourceCode.harpo", str )
        val ( errorRecorder, transBuffer ) : (ErrorRecorder, OutputBuilder) = translator.runHarpoToBoogieTrans( true )

        assertResult( expectedFatalErrors )( errorRecorder.getFatalCount() )
        assertResult( expectedWarningErrors )( errorRecorder.getWarningCount() )

        if ( errorRecorder.getFatalCount() == 0 ) {
            transBuffer.newLine
            val text : String = transBuffer.result().mkString( "\n" )
            text 
        } else {
            "Fatal errors"
        }
    }
    
    def translateAndVerify(
        str : String,
        expectedFatalErrors : Int = 0,
        expectedWarningErrors : Int = 0,
        expectedVerificationErrors : Int = 0 )
    : ErrorRecorder = {
        val translator = new HarpoToBoogieTranslator()
        translator.addFile( "HarpoSourceCode.harpo", str )
        translator.translateAndVerify( true ) 
    }
}
