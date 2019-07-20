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

import tests.TestsBase

@RunWith(classOf[JUnitRunner])
class CheckerTests extends TestsBase {

  "The Checker" should "check an empty file" in {
    val str = ""
    tryWithChecker(str)
  }
  
  it should "check the int object declaration" in {
    val str = """
              (class Test()
                obj c: int32 := 0;
              class)
            """
    tryWithChecker(str)
  }

  it should "check a 'ghost' object" in {
    val str = """ 
                  (class Test() 
                    ghost obj c: int32:=0; 
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
	                (thread(*t0*) claim c@0.5 claim x
	                thread) 
                class)"""
    tryWithChecker(str)
  }
}

