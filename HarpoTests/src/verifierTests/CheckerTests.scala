package verifierTests

import org.scalatest.FlatSpec 
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.Assertions._
import java.io._
import org.junit.runner.RunWith
import java.io.OutputStreamWriter
import scala.collection.mutable.ArrayBuffer
import parser.HarpoParser
import parser.ParseException
import parser.TokenMgrError
import frontEnd.ErrorRecorder
import frontEnd.AST

@RunWith(classOf[JUnitRunner])
class CheckerTests extends VerifierTestBase {

  "The Checker" should "check an empty file" in {
    val str = ""
    tryWithChecker(str)
  }

  it should "check a 'ghost' object" in {
    val str = """ 
                  (class Test() 
                    ghost obj c: Int32:=0; 
                  class) 
              """
    tryWithChecker(str)
  }
  it should "check an invariant " in {
    val str = """
                  (class Test()
	                  invariant 5>0
                  class)
              """
    tryWithChecker(str)
  }
  
  // Should fail on resolver pass
  it should "check a 'claim' specification of class Test" in {
    val str = """
                 (class Test()
	                  claim x@0.9-0.6,y@0.4
                  class)
              """
    tryWithChecker(str)
  }

  it should "check an 'assert' command" in {
    val str = """ 
                  (class Test()
	                    obj c: Int32:=0;
	                    const x: Int32:=9;
	                    (thread(*t0*)
		                    assert c+2<x-7
	                    thread) 
                  class) 
             """
    tryWithChecker(str)
  }

  it should "check an 'assume' command" in {
    val str = """  
                  (class Test()
	                    obj c: Int32:=0;
	                    const x: Int32:=9;
	                    (thread(*t0*)
		                    assume c+2<x-7
	                    thread) 
                   class)
              """
    tryWithChecker(str)
  }

  // Constructor Arguments of "in" object category

  it should "check a class with 'ghost' constructor args" in {
    val str = """
                  (class Test(ghost obj par1: Int16)
                  class)
              """
    tryWithChecker(str)
  }

  // Class with ghost variable declarations
  
  it should "check 'ghost const' declarations" in {
    val str = """
                (class Test(ghost obj par1: Int16)
	                ghost obj c: Int32 := 0;
	                ghost const x: Int32 := 1;
                class)
               """
    tryWithChecker(str)
  }

  it should "check a class with 'ghost' local declarations/initialization" in {
    val str = """
                  (class Test()
	                  (thread(*t0*)
		                  ghost const a: Int32 := 9;
	                    ghost obj c: Int32:=0;
	                    ghost const x: Int32:=9;
	                  thread) 
                  class)
              """
    tryWithChecker(str)
  }


  it should "check thread(*t0*) with claim specification" in {
    val str = """ 
                (class Test()
	                ghost obj c: Int32:=0;
	                ghost const x: Int32:=9;
	                (thread(*t0*) claim c@0.5, x
	                thread) 
                class)"""
    tryWithChecker(str)
  }
  
  
  it should "parse when Array Initialization" in {
    val str = """ 
  (class Buffer()

	          proc deposit(in value : Real64)

	          proc fetch(out value :  Real64)

	          const size : Int32 := 10 
	          obj buf : Int32[size] := (for i:size do 0 for)
	          obj front : Int32 :=0
	          obj rear : Int32 :=0
	          obj full : Int32 :=0

	          (thread (*t0*) claim {i:{0,..size} do buf[i]},front, rear, full 
		          (while (true)
			          invariant canWrite(front) /\ canWrite(rear) /\ canWrite(full) /\ canWrite({i:{0,..size} do buf[i]})
			          invariant (0 _< front /\ front < size) /\ (0 _< rear /\ rear < size) /\ (0 _< full /\ full < size)
			          invariant ((front + full) mod size = rear)
                do
				          (accept deposit(in value : Real64) when (full < size)
				               buf[rear] := value
				               rear := (rear+1) mod size
				               full := full+1
				            |
                       fetch(out value: Real64) when (0 < full)
				               value := buf[front]
				               front := (front+1) mod size
				               full := full-1
				          accept)
		            while)
	          thread)
        class)"""
    tryWithChecker(str)
  }

  
  
}

