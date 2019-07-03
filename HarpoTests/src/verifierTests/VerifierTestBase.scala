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

  override def beforeEach(td: TestData) {
    println(">>>>>>>>>>>>>Starting " + td.name + " >>>>>>>>>>>>>>>>>>")
  }

  override def afterEach(td: TestData) {
    println("<<<<<<<<<<<<<Finished " + td.name + " <<<<<<<<<<<<<<<<<")
  }

  def tryWithParser(str: String, expectedFatalErrors: Int = 0, expectedWarningErrors: Int = 0) =
    tryWith(str, expectedFatalErrors, expectedWarningErrors, false)

  def tryWithChecker(str: String, expectedFatalErrors: Int = 0, expectedWarningErrors: Int = 0) =
    tryWith(str, expectedFatalErrors, expectedWarningErrors, true)

  def tryWithBoogieBackEnd(str: String, expectedFatalErrors: Int = 0, expectedWarningErrors: Int = 0): String = {

    println("\n\n\nVerifier start")
    val verify = new HarpoToBoogieTranslator()
    verify.addFile("HarpoSourceCode.harpo", str)
    val (errorRecorder, transBuffer): (ErrorRecorder, OutputBuilder) = verify.runHarpoToBoogieTrans(true)

    assertResult(expectedFatalErrors)(errorRecorder.getFatalCount())
    assertResult(expectedWarningErrors)(errorRecorder.getWarningCount())
    transBuffer.newLine
    var text: String = transBuffer.result().mkString("\n")
    val ret = if (errorRecorder.getFatalCount() == 0) {
      transBuffer.newLine
      val vr: VerificationReport = verify.runVerifer(text, true)
      for (ve <- vr.verificationErrorList) {
        println("Boogie Output Line Number (" + ve.lineNumber + ")" + " has verification error: " + ve.boogieMessage + " due to ")
        val map: Option[Tuple2[String, AST.Coord]] = transBuffer.getError(ve.lineNumber)
        if(map.nonEmpty)
        println("HARPO Source Map : " + map.toString())
      }
      for (oe <- vr.otherErrorList) {
        println("Boogie Output Line Number (" + oe.lineNumber + ")" + " has syntax error: " + oe.boogieMessage)
        val map: Option[Tuple2[String, AST.Coord]] = transBuffer.getError(oe.lineNumber)
        if(map.nonEmpty)
        println("HARPO Source Map : " + map.toString())
      }

      text
    } else {
      "Fatal errors"
    }
    text
  }

  def tryWith(str: String, expectedFatalErrors: Int, expectedWarningErrors: Int, runChecker: Boolean): StandardErrorRecorder = {

    // Build the builder and the parser
    val errorRecorder = new StandardErrorRecorder()
    val reader = new StringReader(str)
    val p: HarpoParser = new HarpoParser(reader)
    val builder = new frontEnd.Builder(errorRecorder);
    p.setBuilder(builder)

    // Run the parser.

    val dl: frontEnd.AST.DeclList =
      try { p.Start().asInstanceOf[frontEnd.AST.DeclList] }
      catch {
        case ex: ParseException => {
          val coord = if (ex.currentToken != null) AST.Coord("fileName", ex.currentToken.beginLine, ex.currentToken.beginColumn)
          else AST.Coord("fileName");
          errorRecorder.reportFatal(ex.getMessage(), coord)
          null
        }
        case ex: TokenMgrError => {
          val coord = AST.Coord("fileName")
          errorRecorder.reportFatal(ex.getMessage(), coord)
          null
        }
      }

    // Output all errors
    if (errorRecorder.getTotalErrorCount() > 0) {
      println("-----------------------------");
      for (i <- 0 until errorRecorder.getTotalErrorCount())
        println(errorRecorder.getErrorCoord(i) + " " + errorRecorder.getErrorText(i));
    }
    
     println("----The AST after Parsing---");
      println(dl.format(80))
      println
      println("---------------------------------------");

    if (runChecker) {
      assert(errorRecorder.getFatalCount() == 0, "1st pass error prevents checker from running.")
      val checker = new Checker(errorRecorder)
      try {
        checker.runChecker(dl)
      } // The only exception that the checker should throw is a Bail.
      catch {
        case e: CompilerBailOutException =>
          println("----The checker has bailed---");
      }

      // Output the tree.
      println("----The AST after checking---");
      println(dl.format(80))
      println
      println("---------------------------------------");

      // Output all errors
      if (errorRecorder.getTotalErrorCount() > 0) {
        println("-----------------------------")
        for (i <- 0 until errorRecorder.getTotalErrorCount())
          println(errorRecorder.getErrorCoord(i) + " " + errorRecorder.getErrorText(i));
      }
    }
    assertResult(expectedFatalErrors)(errorRecorder.getFatalCount())
    assertResult(expectedWarningErrors)(errorRecorder.getWarningCount())
    errorRecorder
  }
}
