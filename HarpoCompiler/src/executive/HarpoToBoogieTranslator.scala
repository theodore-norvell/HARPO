package executive
import scala.collection.mutable.ArrayBuffer;
import java.io.PrintWriter;
import java.io.File;
import frontEnd._
import parser._
import checker.Checker;
import scala.collection.mutable.ArrayBuffer
import java.io.StringReader
import boogieBackEnd.BoogieBackEnd
import util.OutputBuilder;
import boogieBackEnd.VerificationReport;
import boogieBackEnd.BoogieErrorParser;
import scala.sys.process.ProcessLogger;
import scala.sys.process.Process;


class HarpoToBoogieTranslator {

  private var errorRecorder = new StandardErrorRecorder()

  private var boogieOutput = "No Output"

  def getErrorReport(): ErrorReport = errorRecorder

  private var files = new ArrayBuffer[(String, String)]

  def addFile(fileName: String, fileText: String) {
    files += ((fileName, fileText))
  }

  def runHarpoToBoogieTrans(verbose: Boolean) = {

    var transOutBuffer = new OutputBuilder

    errorRecorder = new StandardErrorRecorder()

    println("Translator is running")

    val masterDeclList = new frontEnd.AST.DeclList()

    for ((fileName, fileText) <- files) {
      val reader = new StringReader(fileText)
      val p: HarpoParser = new HarpoParser(reader)
      p.setFileName(fileName);
      val builder = new frontEnd.Builder(errorRecorder);
      p.setBuilder(builder)
      // Run the parser.

      val dl: frontEnd.AST.DeclList =
        try {
          p.Start().asInstanceOf[frontEnd.AST.DeclList]
        } catch {
          case ex: ParseException => {
            val coord = if (ex.currentToken != null) AST.Coord(fileName, ex.currentToken.beginLine, ex.currentToken.beginColumn)
            else AST.Coord(fileName);
            errorRecorder.reportFatal(ex.getMessage(), coord)
            null
          }
          case ex: TokenMgrError => {
            val coord = AST.Coord(fileName) //TODO add line and column number
            errorRecorder.reportFatal(ex.getMessage(), coord)
            null
          }
        }

      if (dl != null) {
        for (decl <- dl.decls) {
          masterDeclList.addDeclaration(decl)
        }
      }
    }
    println(masterDeclList.format(100))
    if (errorRecorder.getFatalCount() == 0) {
      println("Checker Start");
      val checker = new Checker(errorRecorder)
      try { checker.runChecker(masterDeclList) }
      // The only exception that the checker should throw is a CompilerBailOutException.
      catch { case e: CompilerBailOutException => () }
    }
    errorRecorder.printErrors(Console.out);
    // The boogie code generator
    if (errorRecorder.getFatalCount() == 0) {
      println("Fetal Errors= ", errorRecorder.getFatalCount());
      println()
      val boogieCodeGen = new BoogieBackEnd(masterDeclList, transOutBuffer)
      transOutBuffer = boogieCodeGen.getBoogieCode();
    }
    println(masterDeclList.format(100))
    (errorRecorder, transOutBuffer)
  }

  def runVerifer(text: String, verbose: Boolean): VerificationReport = {

    val writer = new PrintWriter(new File("BoogieOutputScript.bpl"))
    writer.write(text)
    writer.close()
    val command = "boogie BoogieOutputScript.bpl";
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

    val ep: BoogieErrorParser = new BoogieErrorParser;

    ep.parseBoogieOutput(outputDes)

  }

}