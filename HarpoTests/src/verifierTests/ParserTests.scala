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
class ParserTests extends TestsBase {

  "The parser" should "parse an empty file" in {
    val str = ""
    tryWithParser(str)
  }

  it should "parse a 'ghost' object" in {
    val str = """ 
                  (class Test() 
                    ghost obj c: Int32:=0; 
                  class) 
              """
    tryWithParser(str)
  }
  it should "parse an invariant " in {
    val str = """
                  (class Test()
	                  invariant 5>0
                  class)
              """
    tryWithParser(str)
  }
  it should "parse a 'claim' specification of class Test" in {
    val str = """
                 (class Test()
	                  claim x@0.9-0.6,y@0.4
                  class)
              """
    tryWithParser(str)
  }

  it should "parse an 'assert' command" in {
    val str = """ 
                  (class Test()
	                    obj c: Int32:=0;
	                    const x: Int32:=9;
	                    (thread(*t0*)
		                    assert c+2<x-7
	                    thread) 
                  class) 
             """
    tryWithParser(str)
  }

  it should "parse an 'assume' command" in {
    val str = """  
                  (class Test()
	                    obj c: Int32:=0;
	                    const x: Int32:=9;
	                    (thread(*t0*)
		                    assume c+2<x-7
	                    thread) 
                   class)
              """
    tryWithParser(str)
  }

  // Constructor Arguments of "in" object category

  it should "parse a class with 'ghost' constructor args" in {
    val str = """
                  (class Test(ghost obj par1: Int16)
                  class)
              """
    tryWithParser(str)
  }

  // Class with ghost variable declarations
  
  it should "parse 'ghost const' declarations" in {
    val str = """
                (class Test(ghost obj par1: Int16)
	                ghost obj c: Int32 := 0;
	                ghost const x: Int32 := 1;
                class)
               """
    tryWithParser(str)
  }

  it should "parse a class with 'ghost' local declarations/initialization" in {
    val str = """
                  (class Test()
	                  (thread(*t0*)
                      ghost const a: Int32 := 9;
	                    ghost obj c: Int32:=0;
	                    ghost const x: Int32:=9;
	                  thread) 
                  class)
              """
    tryWithParser(str)
  }


  it should "parse thread(*t0*) with claim specification" in {
    val str = """ 
                (class Test()
	                ghost obj c: Int32:=0;
	                ghost const x: Int32:=9;
	                (thread(*t0*) claim c@0.5 claim x
	                thread) 
                class)"""
    tryWithParser(str)
  }  
  
  it should "parse Method Declaration with specifications" in {
    val str = """ 
                (class Test()
	                ghost obj c: Int32:=0;
	                ghost const x: Int32:=9;
                  public proc add(ghost in par: Int32, ghost in par2: Int32,in par3: Int64, out par4: Int16)
                	pre c>8
                  post c'>10
                  post x'<1
                	takes x
                	gives c
                	(thread(*t0*) claim c@0.5 claim c@0.6,x
                		ghost const a: Int32:=9;
                		c:=2+2
                		assume c>9 
                		assert a+2<20
                	thread) 
                class)"""
    tryWithParser(str)
  }
}

