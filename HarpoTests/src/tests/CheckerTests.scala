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
}