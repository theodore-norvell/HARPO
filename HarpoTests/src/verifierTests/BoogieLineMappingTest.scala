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
import executive.HarpoToBoogieTranslator ;
import frontEnd.StandardErrorRecorder

@RunWith( classOf[JUnitRunner] )
class BoogieLineMappingTest extends VerifierTestBase {
    def runBoogie( stringBoogie : String ) = {
        
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
    val hasVerificationError = """procedure F(n: int) returns (r: int)
  ensures 100 < n ==> r == n - 10;  // This postcondition is easy to check by hand
  ensures n <= 100 ==> r != 91;     // Do you believe this one is true?
{
  if (100 < n) {
    r := n - 10;
  } else {
    call r := F(n + 11);
    call r := F(r);
  } 
}"""
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
}"""

    it should "deal with boogie syntax errors" in {
        var translator = new HarpoToBoogieTranslator()
        val vr : VerificationReport = translator.runVerifer( hasSyntaxError, true ) 
        assertResult( 1 )( vr.otherErrorCount )
        assertResult( 0 )( vr.verificationErrorCount )
        val err = vr.getOtherError(0) ;
        assertResult( 11 )( err.lineNumber )
    }

    it should "deal with boogie verification errors" in {
        var translator = new HarpoToBoogieTranslator()
        val vr : VerificationReport = translator.runVerifer( hasVerificationError, true ) 
        assertResult( 0 )( vr.otherErrorCount )
        assertResult( 1 )( vr.verificationErrorCount )
        val err = vr.getVerificationError(0) ;
        assertResult( 10 )( err.lineNumber )
        assertResult( 1 )( err.associatedLineNumbers.length ) 
        assertResult( 3 )( err.associatedLineNumbers(0) )
    }

    it should "deal with boogie with no errors" in {
        var translator = new HarpoToBoogieTranslator()
        val vr : VerificationReport = translator.runVerifer( hasNoError, true ) 
        assertResult( 0 )( vr.otherErrorCount )
        assertResult( 0 )( vr.verificationErrorCount )
    }
    
    it should "map boogie line numbers to Harpo line numbers" in {
        val boogieSource = tryWithBoogieBackEnd( harpo0 )
        assert( false, "todo more" )
    }
}
