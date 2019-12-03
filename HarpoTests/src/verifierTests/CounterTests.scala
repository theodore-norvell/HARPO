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
                  (thread (*t1*)
                    increment()
                  thread)
             class)"""

    val (errors, builder) = translateAndVerify(str)
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
                  (thread (*t1*)
                   increment()
                  thread)
             class)"""

    //val (errors, builder) = translateAndVerify(str) 
 }  
//    it should "generate Boogie code for Counter class without class invariant" in {
//        
//    val str = """ 
//                (class Counter()	                
//                  claim count@0.5
//
//	                proc increment()
//	                  takes count@0.5
//	                  pre count>_0
//	                  post count'>0
//	                  gives count@0.5
//	                obj count: Int32 := 0
//	                (thread (*t0*)
//		                (while true
//		                  do
//			                  (accept increment()
//					                  count := count+1; 
//				                accept)
//		                while)
//	                thread)
//             class)"""
//
//    translateAndVerify(str)
//  }
//  
//    
//    it should "generate Boogie code for Counter class without takes and gives" in {
//        
//    val str = """ 
//                (class Counter()	                
//                  claim count@0.5
//
//	                proc increment()
//	                  pre count>_0
//	                  post count'>0
//	                obj count: Int32 := 0
//	                (thread (*t0*)
//		                (while true
//		                  do
//			                  (accept increment()
//                        (with this
//                          do
//					                  count := count+1;
//                          with) 
//				                accept)
//		                while)
//	                thread)
//             class)"""
//
//    translateAndVerify(str)
//
//  }
//    
//    
//  it should "generate Boogie code for Counter class with claim of full permission and gives" in {
//        
//    val str = """ 
//                (class Counter()	                
//                  claim count@1.0
//
//	                proc increment()
//	                  pre count>_0
//	                  post count'>0
//	                obj count: Int32 := 0
//	                (thread (*t0*)
//		                (while true
//		                  do
//			                  (accept increment()
//                        (with this
//                          do
//					                  count := count+1;
//                          with) 
//				                accept)
//		                while)
//	                thread)
//             class)"""
//
//    translateAndVerify(str)
//
//  }
//  
//    it should "generate Boogie code for Counter class with claim of more than maximum allowed permission" in {
//        
//    val str = """ 
//                (class Counter()	                
//                  claim count@1.0
//
//	                proc increment()
//	                  pre count>_0
//	                  post count'>0
//	                obj count: Int32 := 0
//	                (thread (*t0*)
//		                (while true
//		                  do
//			                  (accept increment()
//                        (with this
//                          do
//					                  count := count+1;
//                          with) 
//				                accept)
//		                while)
//	                thread)
//             class)"""
//
//    translateAndVerify(str)
//
//  }
//
//  it should "generate Boogie code for Counter class with invariant of full permission" in {
//        
//    val str = """ 
//                (class Counter()	                
//                  claim count@1.0
//                  invariant canWrite(count)
//	                proc increment()
//	                  pre count>_0
//	                  post count'>0
//	                obj count: Int32 := 0
//	                (thread (*t0*)
//		                (while true
//		                  do
//			                  (accept increment()
//                        (with this
//                          do
//					                  count := count+1;
//                          with) 
//				                accept)
//		                while)
//	                thread)
//             class)"""
//
//    translateAndVerify(str)
//
//  }
//  
//    it should "generate Boogie code for Counter invariant of full permission without claim" in {
//        
//    val str = """ 
//                (class Counter()
//                  invariant canWrite(count)
//	                proc increment()
//	                  pre count>_0
//	                  post count'>0
//	                obj count: Int32 := 0
//	                (thread (*t0*)
//		                (while true
//		                  do
//			                  (accept increment()
//                        (with this
//                          do
//					                  count := count+1;
//                          with) 
//				                accept)
//		                while)
//	                thread)
//             class)"""
//
//    translateAndVerify(str)
//
//  }
//    
//  it should "generate Boogie code for Assignment" in {
//    
//    val str = """
//                (class Another() class)
//                (class bar()	                
//                  claim count@0.5
//	                invariant canRead(count) /\ count >_ 0
//	                obj foo : Another := new Another();
//	                proc increment()
//	                  takes count@0.5
//	                  pre count>_0
//	                  post count'>0
//	                  gives count@0.5
//	                obj count: Int32 := 0
//	                (thread (*t0*)
//		                (while true
//		                  do
//			                  (accept increment()
//				                  (with foo
//                            do
//					                  count := count+1;
//					                with) 
//				                accept)
//		                while)
//	                thread)
//             class)"""
//
//    val (builder, error) = translateAndVerify(str)
//  }
//  
    
}