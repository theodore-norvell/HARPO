package tests

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.Assertions._
import java.io._
import org.junit.runner.RunWith
import java.io.OutputStreamWriter
import scala.collection.mutable.ArrayBuffer
import parser.HarpoParser
import frontEnd.ErrorRecorder
import frontEnd.AST 
import checker.Checker 

@RunWith(classOf[JUnitRunner]) 
class CheckerTests extends TestsBase { 
  
  "The checker" should "work an empty file" in {
    val str = "" 
    tryWithChecker(str) 
  }
 
  it should "detect dupplicate names at top level" in {
    val str = """(interface Fred interface)
        (class Fred() class)""" 
    val ter = tryWithChecker(str, 1, 0) 
    assertResult( "The name ::Fred is already in use." )( ter.getFatalText(0) )
    assertResult( AST.Coord("<unknown>", 2, 16) )( ter.getFatalCoord(0) )
  }
 
  it should "detect duplicate names at class level" in {
    val str = """
        (class Fred()
            obj x : Int16 := 0
            obj y : Int32 := 0
            obj x : Int8 := 0
        class)""" 
    val ter = tryWithChecker(str, 1, 0) 
    assertResult( "The name ::Fred::x is already in use." )( ter.getFatalText(0) )
    assertResult( AST.Coord("<unknown>", 5, 17) )( ter.getFatalCoord(0) )
  }
  
  it should "work with a thread" in {
    val str = """
        (class Fred()
            obj x : Int8 := 0
            (thread
                 obj y : Real32 := 3
                 y := x+y )
        class)""" 
    val ter = tryWithChecker(str, 0, 0) 
  }
  
  it should "reject bad super types" in {
    val str = """
        (interface Fred extends Int8 )
        (class Bob() implements Fred[10] )"""  
    val ter = tryWithChecker(str, 2, 0) 
    assertResult("Only interfaces and instances of interfaces can be supertypes")( ter.getFatalText(0) )
    assertResult( AST.Coord("<unknown>", 2, 33) )( ter.getFatalCoord(0) )
    assertResult("Only interfaces and instances of interfaces can be supertypes")( ter.getFatalText(1) )
    assertResult( AST.Coord("<unknown>", 3, 37) )( ter.getFatalCoord(1) )
    
  }
  
  it should "reject circular super types0" in {
    val str = """
        (interface Fred extends Fred )""" 
    val ter = tryWithChecker(str, 1, 0) 
    assertResult( "There is a cycle of super types involving types ::Fred and ::Fred." )( ter.getFatalText(0) )
    assertResult( AST.Coord("<unknown>", 2, 20) )( ter.getFatalCoord(0) )
  }
  
  it should "reject circular super types1" in {
    val str = """
        (interface Ginger extends Fred )
        (interface Fred extends Ginger )""" 
    val ter = tryWithChecker(str, 1, 0) 
    assertResult("There is a cycle of super types involving types ::Fred and ::Ginger.")( ter.getFatalText(0) )
    assertResult( AST.Coord("<unknown>", 3, 20) )( ter.getFatalCoord(0) )
  }
  
  it should "reject undeclared methods" in {
      val str = """
        (class Fred() implements Bob
            obj x  : Int32 := 0
            (thread
                 (accept foo( in p : Int32, out q : Int32 ) when 1=2
                     q := p + x + z) 
                 bar(1, x) ) // Bar is not declared
        class)
        (interface Bob
             public proc foo( in p : Int32, out q : Int32 )
             public obj z : Int8 := 1
        )""" 
    val ter = tryWithChecker(str, 1, 0) 
    assertResult("Could not find bar")( ter.getFatalText(0) )
    assertResult( AST.Coord("<unknown>", 7, 18) )( ter.getFatalCoord(0) )
  }
  
  it should "reject implementations of undeclared method" in {
      val str = """
        (class Fred() implements Bob
            obj x  : Int32 := 0
            (thread
                 // foo is not declared
                 (accept foo( in p : Int32, out q : Int32 ) when 1=2
                     q := p + x + z) 
                 bar(1, x) )
        class)
        (interface Bob
             public proc bar(in p : Int32, out q : Int32)
             public obj z : Int8 := 1
        )""" 
    val ter = tryWithChecker(str, 1, 0) 
    assertResult("Could not find foo")( ter.getFatalText(0) )
    assertResult( AST.Coord("<unknown>", 6, 26) )( ter.getFatalCoord(0) )
  }
  
  it should "resolve inherited names" in {
      val str = """
        (class Fred() implements Bob
            obj x  : Int32 := 0
            (thread
                 (accept foo( in p : Int32, out q : Int32 ) when 1=2
                     q := p + x + z) 
                 bar(1, x) )
        class)
        (interface Bob
             public proc foo( in p : Int32, out q : Int32 )
             public proc bar(in p : Int32, out q : Int32)
             public obj z : Int8 := 1
        )""" 
    val ter = tryWithChecker(str, 0, 0) 
  }
  
  /*
   * ================Start==========================
   * Checker Tests with Annotations for Verification
   * ===============================================
  */
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
  
  it should "check invariant with PermissionOp specification" in {
    val str = """ 
                (class Test()
                  ghost const x: Int32:=9;
                  invariant permission(x) = 1;              
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