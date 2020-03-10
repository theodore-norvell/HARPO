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
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

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
  
  var TestNum = 0;
  override def beforeEach(td: TestData) {
    println(">>>>>>>>>>>>>Starting " + td.name + " >>>>>>>>>>>>>>>>>>")
  }

  override def afterEach(td: TestData) {
    println("<<<<<<<<<<<<<Finished " + td.name + " <<<<<<<<<<<<<<<<<")
  }

  def tryWithBoogieBackEnd(str: String, expectedFatalErrors: Int = 0, expectedWarningErrors: Int = 0): String = {

    println("\n\n\nTranslation start")
    val translator = new HarpoToBoogieTranslator()
    translator.addFile("HarpoSourceCode.harpo", str)
    val (errorRecorder, transBuffer): (ErrorRecorder, OutputBuilder) = translator.runHarpoToBoogieTrans(true)

    assertResult(expectedFatalErrors)(errorRecorder.getFatalCount())
    assertResult(expectedWarningErrors)(errorRecorder.getWarningCount())

    if (errorRecorder.getFatalCount() == 0) {
      transBuffer.newLine
      val text: String = transBuffer.resultAsString()
      text
    } else {
      "Fatal errors"
    }
  }

  def translateAndVerify(
    fileName: String,
    HarpoStr: String,
    expectedFatalErrors: Int = 0,
    expectedWarningErrors: Int = 0,
    expectedVerificationErrors: Int = 0): (StandardErrorRecorder, OutputBuilder) = {
    val translator = new HarpoToBoogieTranslator()
    translator.addFile("HarpoSourceCode.harpo", HarpoStr)
    val (errorRecorder, builder) = translator.translateAndVerify(true)
    println(builder.resultAsStringWithLineNum())
    writeTestIntoTextFile(fileName, HarpoStr, builder.resultAsStringWithLineNum(), errorRecorder)
    assertResult(expectedFatalErrors)(errorRecorder.getFatalCount())
    assertResult(expectedWarningErrors)(errorRecorder.getWarningCount())
    assertResult(expectedVerificationErrors)(errorRecorder.getVerificationCount())
    println(errorRecorder.printErrors(System.out))
    (errorRecorder, builder)
  }
  //Write Complete Test into Text File
  def writeTestIntoTextFile(fileName: String, harpoStr: String, boogieStr: String, errors: StandardErrorRecorder) {
    println(s"See ------------- ${fileName}[${TestNum}].txt")
    val writer = new PrintWriter( new File( s"${fileName}[${TestNum}].txt") )
    writer.write(s"-------------------HARPO Code in File: ${fileName}[${TestNum}].txt---------------------------\n")
    writer.write(harpoStr + "\n")
    writer.write("--------------------Boogie Code--------------------------\n")
    writer.write(boogieStr + "\n")
    writer.write("--------------------Errors Detected--------------------------\n")
    writer.write(errors.getErrors() + "\n")
    writer.close()
    TestNum = TestNum+1;
  }
}