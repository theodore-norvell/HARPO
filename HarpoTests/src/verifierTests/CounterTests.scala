/* 
 * These tests contains all the variations of the counter class
 * @Author: Inaam Ahmed
 * */

package verifierTests
import scala.sys.process._
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.io._
import org.junit.runner.RunWith
import java.io.OutputStreamWriter
import scala.collection.mutable.ArrayBuffer
import parser.HarpoParser
import frontEnd.StandardErrorRecorder
import frontEnd.AST
import frontEnd.AST.DeclNd
import frontEnd.AST.ThreadDeclNd
import frontEnd.AST.CommandNd
import frontEnd.AST.ClassDeclNd
import frontEnd.AST.ClassLike

@RunWith(classOf[JUnitRunner])
class CounterTests extends VerifierTestBase {
  
  behavior of "The Boogie back end with Harpo 'Counter' class";
  it should "generate Boogie code for Counter class with lock" in {
    
    val str = """ 
                (class Counter()	                
                  claim count@0.5
	                invariant canRead(count) /\ count >_ 0
	
	                proc increment()
	                  takes count@0.5
	                  pre count>_0
	                  post count'>0
	                  gives count@0.5
	                obj count: Int32 := 0
	                (thread (*t0*)
		                (while true
		                  do
			                  (accept increment()
				                  (with this
                            do
					                  count := count+1;
					                with) 
				                accept)
		                while)
	                thread)
             class)"""

    val BoogieSource = tryWithBoogieBackEnd(str)
    
    println(BoogieSource)
    
  }
  
  
  it should "generate Boogie code for Counter class without lock" in {
        
    val str = """ 
                (class Counter()	                
                  claim count@0.5
	                invariant canRead(count) /\ count >_ 0
	
	                proc increment()
	                  takes count@0.5
	                  pre count>_0
	                  post count'>0
	                  gives count@0.5
	                obj count: Int32 := 0
	                (thread (*t0*)
		                (while true
		                  do
			                  (accept increment()
					                  count := count+1; 
				                accept)
		                while)
	                thread)
             class)"""

    val BoogieSource = tryWithBoogieBackEnd(str)
    
    println(BoogieSource)
  }
  
    it should "generate Boogie code for Counter class without object invariant" in {
        
    val str = """ 
                (class Counter()	                
                  claim count@0.5

	                proc increment()
	                  takes count@0.5
	                  pre count>_0
	                  post count'>0
	                  gives count@0.5
	                obj count: Int32 := 0
	                (thread (*t0*)
		                (while true
		                  do
			                  (accept increment()
					                  count := count+1; 
				                accept)
		                while)
	                thread)
             class)"""

    val BoogieSource = tryWithBoogieBackEnd(str)
    
    println(BoogieSource)
  }
  
    
    it should "generate Boogie code for Counter class without takes and gives" in {
        
    val str = """ 
                (class Counter()	                
                  claim count@0.5

	                proc increment()
	                  pre count>_0
	                  post count'>0
	                obj count: Int32 := 0
	                (thread (*t0*)
		                (while true
		                  do
			                  (accept increment()
                        (with this
                          do
					                  count := count+1;
                          with) 
				                accept)
		                while)
	                thread)
             class)"""

    val BoogieSource = tryWithBoogieBackEnd(str)

  }
}