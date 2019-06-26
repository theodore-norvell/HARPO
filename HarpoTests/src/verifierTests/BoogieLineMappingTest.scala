package verifierTests
import scala.sys.process._
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.io._
import org.junit.runner.RunWith
import java.io.OutputStreamWriter
import scala.collection.mutable.ArrayBuffer

import boogieBackEnd.VerificationReport
import executive.HarpoToBoogieTranslator;
import frontEnd.AST.Coord 
import frontEnd.StandardErrorRecorder
import util.OutputBuilder

@RunWith(classOf[JUnitRunner])
class BoogieLineMappingTest extends VerifierTestBase {
  def runBoogie(stringBoogie: String) = {

  }

  behavior of "The Boogie back end";

  val harpo0 = """
          (class Alice()

              (thread
                  assert 1 = 2 
              thread)
          class)

"""
  val hasSyntaxError = """procedure F(n: int) returns (r: int)
  ensures 100 < n ==> r == n - 10;  // This postcondition is easy to check by hand
  ensures n <= 100 ==> r == 91;     // Do you believe this one is true?
{
  if (100 < n) {
    r := n - 10;
  } else {
    call r := F(n + 11);
    call r := F(r);
  } /* Mising close brace */
"""
  val hasNoError = """procedure F(n: int) returns (r: int)
  ensures 100 < n ==> r == n - 10;  // This postcondition is easy to check by hand
  ensures n <= 100 ==> r == 91;     // Do you believe this one is true?
{
  if (100 < n) {
    r := n - 10;
  } else {
    call r := F(n + 11);
    call r := F(r);
  }
}
"""
  val hasVerificationError = new OutputBuilder() 
  hasVerificationError.put( "procedure F(n: int) returns (r: int)" ) ; hasVerificationError.newLine  // 1
  val errorCoord_40_12 = new Coord("filex.harpo", 40, 12) 
  hasVerificationError.setError( "", errorCoord_40_12 ) 
  hasVerificationError.put( "ensures 100 < n ==> r == n - 10;" ) ; hasVerificationError.newLine // 2
  hasVerificationError.put( "ensures n <= 100 ==> r != 91;" ) ; hasVerificationError.newLine // 3
  hasVerificationError.put( "{" ) ; hasVerificationError.newLine // 4
  hasVerificationError.indent
  hasVerificationError.put( "if (100 < n) {" ) ; hasVerificationError.newLine // 5
  hasVerificationError.indent
  hasVerificationError.put( "r := n - 10;" ) ; hasVerificationError.newLine //6
  hasVerificationError.dedent
  hasVerificationError.put( "} else {" ) ; hasVerificationError.newLine // 7
  hasVerificationError.indent
  hasVerificationError.put( "call r := F(n + 11);" ) ; hasVerificationError.newLine // 8
  hasVerificationError.put( "call r := F(r);" ) ; hasVerificationError.newLine // 9
  hasVerificationError.dedent
  
  val errorCoord = new Coord("filex.harpo", 42, 99) 
  hasVerificationError.setError( "oops", errorCoord ) 
  hasVerificationError.put( " } " ) ; hasVerificationError.newLine //10 
  hasVerificationError.dedent 
  hasVerificationError.put( "}" ) ; hasVerificationError.newLine // 11
  
  it should "deal with boogie syntax errors" in {
    var translator = new HarpoToBoogieTranslator()
    val vr: VerificationReport = translator.runVerifier(hasSyntaxError, true)
    
    assertResult(1)(vr.otherErrorCount)
    assertResult(0)(vr.verificationErrorCount)
    val err = vr.getOtherError(0);
    assertResult(11)(err.lineNumber)
  }

  it should "deal with boogie verification errors" in {
    var translator = new HarpoToBoogieTranslator()
    val vr: VerificationReport = translator.runVerifier(hasVerificationError.resultAsString(), true)
    
    assertResult(0)(vr.otherErrorCount)
    assertResult(1)(vr.verificationErrorCount)
    
    val err = vr.getVerificationError(0)
    assertResult(10)(err.lineNumber)
    assertResult(1)(err.associatedLineNumbers.length)
    assertResult(3)(err.associatedLineNumbers(0))
    
    // Now check that these are translated correctly to Harpo line numbers.
    val er = new StandardErrorRecorder()
    vr.reportErrors( er, hasVerificationError )
    
    assertResult( 1 )( er.getTotalErrorCount() )
    assertResult( 1 )( er.getVerificationCount() )
    assertResult( errorCoord )( er.getVerificationCoord( 0 ) ) 
    val message = "oops\nAssociated line File: filex.harpo line: 40 column: 12"  
    assertResult( message )( er.getVerificationText( 0 ) )
  }

  it should "deal with boogie with no errors" in {
    var translator = new HarpoToBoogieTranslator()
    val vr: VerificationReport = translator.runVerifier(hasNoError, true)
    assertResult(0)(vr.otherErrorCount)
    assertResult(0)(vr.verificationErrorCount)
  }

  it should "map boogie line numbers to Harpo line numbers" in {
    val errors = translateAndVerify(harpo0, 0, 0, 1)
    assertResult( 5 )( errors.getVerificationCoord( 1 ).line ) 
    val expectedErrorMessage = "TODO" 
    assertResult( expectedErrorMessage )( errors.getVerificationText( 1 ) ) 
  }
}
