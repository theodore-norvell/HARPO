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
import sys.process._;
import scala.sys.process.ProcessBuilder;
import executive.HarpoToBoogieTranslator;

class TestsBase extends FlatSpec with BeforeAndAfterEach {

  var outputBuffer: OutputBuilder = new OutputBuilder;

  override def beforeEach(td: TestData) {
    println(">>>>>>>>>>>>>Starting " + td.name + " >>>>>>>>>>>>>>>>>>")
  }

  override def afterEach(td: TestData) {
    println("<<<<<<<<<<<<<Finished " + td.name + " <<<<<<<<<<<<<<<<<")
  }

  def tryWithParser(str: String, expectedFatalErrors: Int = 0, expectedWarningErrors: Int = 0) =
    tryWith(str, expectedFatalErrors, expectedWarningErrors, false, false)

  def tryWithChecker(str: String, expectedFatalErrors: Int = 0, expectedWarningErrors: Int = 0) =
    tryWith(str, expectedFatalErrors, expectedWarningErrors, true, false)

  def tryWithBoogieBackEnd(str: String, expectedFatalErrors: Int = 0, expectedWarningErrors: Int = 0, runChecker: Boolean = true) = {
    tryWith(str, expectedFatalErrors, expectedWarningErrors, runChecker, true)

    println("\n\n\nVerifier start")
    var verify = new HarpoToBoogieTranslator()
    verify.addFile("HarpoSourceCode.harpo", str)
    var outputBuffer: OutputBuilder = new OutputBuilder;
    var transBuffer: OutputBuilder = new OutputBuilder;
    transBuffer = verify.runHarpoToBoogieTrans(outputBuffer)
    transBuffer.newLine
    val text: String = transBuffer.result().mkString("\n")
    println(text)
    val writer = new PrintWriter(new File("BoogieOutputScript.bpl"))
    writer.write(text)
    writer.close()

    // println("Boogie Script\n\n\n"+ boogieScript)
    val command = "cmd.exe /c boogie \"BoogieOutputScript.bpl\"";
    /*
   * without c process will hang
   * ! will give back result
   * !! gives back output in result
   */
    var log: String = "";

    var result: String = "";

    val logger = ProcessLogger((out) => result = out, (err) => log = err)

    val output = Process(command).!(logger)

    val outputDes = Process(command).!!

    println(" ================== Boogie Verifier Results Begin====================== \n\n")
    println(output)
    println(result)
    println(outputDes)
    println(" \n\n================== Boogie Verifier Results End======================")

    println("\n\n\nVerifier end")
    transBuffer

  }

  def tryWith(str: String, expectedFatalErrors: Int, expectedWarningErrors: Int, runChecker: Boolean, runBoogieBackEnd: Boolean): StandardErrorRecorder = {

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

    if (runBoogieBackEnd) {
      assert(errorRecorder.getFatalCount() == 0, "Checking error prevents Boogie back end from running.")
      if (dl != null) {
        println("-----------Boogie Code  generated-------------\n")

        val boogieCodeGen = new BoogieBackEnd(dl, outputBuffer)
        val boogieOutputBuffer = boogieCodeGen.getBoogieCode();
        boogieOutputBuffer.newLine
        val code: String = boogieOutputBuffer.result().mkString("\n")
        println(code)
        val writer = new PrintWriter(new File("BoogieOutputScript.bpl"))
        writer.write(code)
        writer.close()

        // println("Boogie Script\n\n\n"+ boogieScript)
        val command = "cmd.exe /c boogie \"BoogieOutputScript.bpl\"";
        /*
   			* without c process will hang
   			* ! will give back result
   			* !! gives back output in result
   			*/
        var log: String = "";

        var result: String = "";

        val logger = ProcessLogger((out) => result = out, (err) => log = err)

        val output = Process(command).!(logger)

        val outputDes = Process(command).!!

        println(" ================== Boogie Verifier Results Begin====================== \n\n")
        println(output)
        println(result)
        println(outputDes)
        println(" \n\n================== Boogie Verifier Results End======================")
        println("\n\n\nVerifier end")
      }
    }
    assertResult(expectedFatalErrors)(errorRecorder.getFatalCount())
    assertResult(expectedWarningErrors)(errorRecorder.getWarningCount())
    errorRecorder
  }
}