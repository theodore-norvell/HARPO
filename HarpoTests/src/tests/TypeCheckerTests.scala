package tests

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.io._
import org.junit.runner.RunWith
import java.io.OutputStreamWriter
import scala.collection.mutable.ArrayBuffer
import parser.HarpoParser
import frontEnd.ErrorRecorder
import frontEnd.AST 
import checker.Checker 

@RunWith(classOf[JUnitRunner]) 
class TypeCheckerTests extends TestsBase  { 
      
    behavior of "The type checker" ;
 
    it should "allow initialization with a constant of the same type" in {
        val str = """obj i : Int8 := 99 as Int8
                     obj j : Int16 := 1000 as Int16
                     obj k : Int32 := 1_000_000
                     obj l : Int64 := 1_000_000_000_000 as Int64
                     obj p : Bool := true 
                     obj q : Bool := false
                     obj r : Real16 := 1.0 as Real16
                     obj s : Real32 := 1.0
                     obj t : Real64 := 1.0 as Real64""" 
                     
        val ter = tryWithChecker(str, 0, 0) 
    }
 
    it should "allow widening conversions in initializations" in {
        val str = """obj j : Int16 := 99 as Int8
                     obj k : Int32 := 1000 as Int16
                     obj l : Int64 := 1_000_000 as Int32
                     obj p : Real32 := 1.0 as Real16
                     obj q : Real64 := 1.0 as Real32""" 
        val ter = tryWithChecker(str, 0, 0) 
    }
    
    it should "forbid narrowing conversions in intializations" in {
        val str = """obj i : Int8 := 1000 as Int16
                     obj j : Int16 := 1_000_000 as Int32
                     obj k : Int32 := 1_000_000_000_000 as Int64
                     obj p : Real16 := 1.0 as Real32
                     obj q : Real32 := 1.0 as Real64""" 
        val ter = tryWithChecker(str, 5, 0) 
    }
    
    it should "forbid widening to boolean in intializations" in {
        val str = """obj j : Bool := 1_000_000
                     obj k : Bool := 1.0""" 
        val ter = tryWithChecker(str, 2, 0) 
    }
 
    it should "forbid widening from boolean in intializations" in {
        val str = """obj i : Int8 := true
                     obj j : Real16 := true""" 
        val ter = tryWithChecker(str, 2, 0) 
    }
 
    it should "accept Int8+Int8 -> Int8" in {
        val str = """obj i : Int8 := 20
                     obj j : Int8 := 40
                     obj k : Int8 := i+j""" 
        val ter = tryWithChecker(str, 0, 0) 
    }
 
    it should "reject type check Int8+Int16 -> Int8" in {
        val str = """obj i : Int8 := 20
                     obj j : Int16 := 40
                     obj k : Int8 := i+j""" 
        
        val ter = tryWithChecker(str, 1, 0) 
        assertResult( "Can not convert ::Int16 to ::Int8." )( ter.getFatalText(0) )
        assertResult( AST.Coord("<unknown>", 3, 39) )( ter.getFatalCoord(0) )
    }
 
    it should "reject type check Int16+Int8 -> Int8" in {
        val str = """obj i : Int16 := 20
                     obj j : Int8 := 40
                     obj k : Int8 := i+j""" 
        
        val ter = tryWithChecker(str, 1, 0) 
        assertResult( "Can not convert ::Int16 to ::Int8." )( ter.getFatalText(0) )
        assertResult( AST.Coord("<unknown>", 3, 39) )( ter.getFatalCoord(0) )
    }
    
     
    it should "infer type from expression in intializations" in {
        val str = """obj i := 99 as Int8
                     obj i2 : Int8 := i
                     obj j := 1000 as Int16
                     obj j1 : Int8 := j // Should error
                     obj j2 : Int16 := j // Should not
                     obj k := 1_000_000 as Int32
                     obj k1 : Int16 := k // Should error
                     obj k2 : Int32 := k // Should not
                     obj l := 1_000_000_000_000 as Int64
                     obj l1 : Int32 := l // Should error
                     obj l2 : Int64 := l // Should not
                     obj p := true
                     obj p2 : Bool := p // Should not error
                     obj q := 1.0 as Real16
                     obj q2 : Real16 := q
                     obj r := 2.0 as Real32
                     obj r1 : Real16 := r // Should error
                     obj r2 : Real32 := r // Should not
                     obj s := 3.0 as Real64
                     obj s1 : Real32 := s // Should error
                     obj s2 : Real64 := s // Should not""" 
        val ter = tryWithChecker(str, 5, 0) 
    }
    
    it should "deal with commands" in {
        val str = """(class Fred()
                         (thread 
                            obj x := 0
                            obj y := 1
                            (if  x = y
                            then x := y
                            else x,y := y,x  x := y if)
                         )
                     class)""" 
        val ter = tryWithChecker(str, 0, 0) 
    }
    
    
    it should "typecheck class members" in {
        val str = """(class Fred(obj s : Server)
                         (thread 
                            obj x := 0
                            obj y : Int32 := 1
                            s.m(x, y)
                         )
                     class)
                     (class Server() implements ServerI 
                          (thread
                              (accept m(in p : Int32, out q : Int32)
                                   q := p )))
                     (interface ServerI
                         proc m(in p : Int32, out q : Int32) )""" 
        val ter = tryWithChecker(str, 0, 0) 
    }
  
    it should "reject mismathed method implementation and declaration 0" in {
        val str = """
          (class Fred()
              proc foo( in p : Int32 )
              obj x  : Int32 := 0
              (thread
                  (accept foo( in p : Int32, out q : Int32 ) ) )
          class)""" 
      val ter = tryWithChecker(str, 1, 0) 
      assertResult("Implementation and declaration of method foo have different numbers of arguments.")( ter.getFatalText(0) )
      assertResult( AST.Coord("<unknown>", 6, 27) )( ter.getFatalCoord(0) )
    }
  
    it should "reject mismathed method implementation and declaration 1" in {
        val str = """
          (class Fred()
              proc foo( in p : Int32, out q : Int32  )
              obj x  : Int32 := 0
              (thread
                  (accept foo( in p : Int32 ) ) )
          class)""" 
      val ter = tryWithChecker(str, 1, 0) 
      assertResult("Implementation and declaration of method foo have different numbers of arguments.")( ter.getFatalText(0) )
      assertResult( AST.Coord("<unknown>", 6, 27) )( ter.getFatalCoord(0) )
    }
  
    it should "reject mismathed method implementation and declaration 2" in {
        val str = """
          (class Fred()
              proc foo( in p : Int32, out q : Int32  )
              obj x  : Int32 := 0
              (thread
                  (accept foo( out p : Int32  out q : Int32  ) ) )
          class)""" 
      val ter = tryWithChecker(str, 1, 0) 
      assertResult("Implementation and declaration of parameter p have different modes.")( ter.getFatalText(0) )
      assertResult( AST.Coord("<unknown>", 6, 36) )( ter.getFatalCoord(0) )
    }
  
    it should "reject mismathed method implementation and declaration 3" in {
        val str = """
          (class Fred()
              proc foo( in p : Int32, out q : Int32  )
              obj x  : Int32 := 0
              (thread
                   (accept foo( in p : Int32  in q : Int32  ) ) )
          class)""" 
      val ter = tryWithChecker(str, 1, 0) 
      assertResult("Implementation and declaration of parameter q have different modes.")( ter.getFatalText(0) )
      assertResult( AST.Coord("<unknown>", 6, 50) )( ter.getFatalCoord(0) )
    }
  
    it should "reject mismathed method implementation and declaration 4" in {
        val str = """
          (class Fred()
              proc foo( in p : Int32, out q : Int32  )
              obj x  : Int32 := 0
              (thread
                   (accept foo( in r : Int32  out q : Int32  ) ) )
          class)""" 
      val ter = tryWithChecker(str, 1, 0) 
      assertResult("Parameter r should be named 'p'.")( ter.getFatalText(0) )
      assertResult( AST.Coord("<unknown>", 6, 36) )( ter.getFatalCoord(0) )
    }
  
    it should "reject mismathed method implementation and declaration 5" in {
        val str = """
          (class Fred()
              proc foo( in p : Int32, out q : Int32  )
              obj x  : Int32 := 0
              (thread
                   (accept foo( in p : Real32  out q : Int32  ) ) )
          class)""" 
      val ter = tryWithChecker(str, 1, 0) 
      assertResult("Implementation and declaration of parameter p have different types.")( ter.getFatalText(0) )
      assertResult( AST.Coord("<unknown>", 6, 36) )( ter.getFatalCoord(0) )
    }
  
    it should "reject too few arguments" in {
        val str = """
          (class Fred() implements Bob
              obj x  : Int32 := 0
              (thread
                   (accept foo( in p : Int32, out q : Int32 ) when 1=2
                       q := p + x + z) 
                   bar(1) ) // Should be 2 arguments
          class)
          (interface Bob
               public proc foo( in p : Int32, out q : Int32 )
               public proc bar(in p : Int32, out q : Int32)
               public obj z : Int8 := 1
          )""" 
      val ter = tryWithChecker(str, 1, 0) 
      assertResult("Incorrect number of arguments. 2 were expected, but there is 1.")( ter.getFatalText(0) )
      assertResult( AST.Coord("<unknown>", 7, 20) )( ter.getFatalCoord(0) )
    }
    
    it should "reject too many arguments" in {
        val str = """
          (class Fred() implements Bob
              obj x  : Int32 := 0
              (thread
                   (accept foo( in p : Int32, out q : Int32 ) when 1=2
                       q := p + x + z) 
                   bar(1, x, 1 ) )// Should be 1 argument1
          class)
          (interface Bob
               public proc foo( in p : Int32, out q : Int32 )
               public proc bar( in p : Int32 )
               public obj z : Int8 := 1
          )""" 
      val ter = tryWithChecker(str, 1, 0) 
      assertResult("Incorrect number of arguments. 1 was expected, but there are 3.")( ter.getFatalText(0) )
      assertResult( AST.Coord("<unknown>", 7, 20) )( ter.getFatalCoord(0) )
    }
}