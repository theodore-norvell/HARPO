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
 
  it should "parse an empty interface" in {
    val str = "(interface Fred interface)" 
    tryWithParser(str) 
  }
  
  it should "fail to parse when there is a missing end" in {
    val str = "(interface Fred interface" 
    tryWithParser(str, 1) 
  }
 
  it should "parse an empty class" in {
    val str = "(class Fred() class)" 
    tryWithParser(str)  
  }
  
  it should "parse nested multiline comments" in {
      val str = "(class Fred() (* comment (* nested (**) comment *) more comment *) class)" 
      tryWithParser(str)
  }
  
  it should "parse multiline comment at end of file" in {
      val str = "(*  *)"
      tryWithParser(str)
  }
  
  it should "fail on incomplete multiline comment" in {
      val str = "(class Fred() class) (* (* *)" 
      tryWithParser(str, 1)
  }
  
  it should "parse single line comments" in {
      val str = """(class Fred() // Fred has class
                class) // this one is unterminated""" 
      tryWithParser(str)
  }
  
  it should "allow a class name to be repeated at the end" in {
    val str = "(class Fred() class Fred)" 
    tryWithParser(str)  
  }
  
  it should "allow an assert command" in {
    val str = "(class Fred(assert c=4;) class Fred)" //should fail
    tryWithParser(str)  
  }
  
  it should "allow an assert command inside thread" in {
    val str = "(class Fred()	public proc add()	(thread (*t0*)(while true do (accept add() c:=2+2;assert c=4;accept)	while) thread) class)" 
    tryWithParser(str)  
  }
  
  
  
  
  it should "warn when the class name at the end does not match" in {
    val str = "(class Fred() \nclass Bob)" 
    val ter = tryWithParser(str, 0, 1)  
    assertResult( "Name does not match." )( ter.getWarningText(0) )
    assertResult( AST.Coord("<unknown>",2,7) )( ter.getWarningCoord(0) )
    
  }
  
  it should "allow an interface name to be repeated at the end" in {
    val str = "(interface Fred interface Fred)" 
    tryWithParser(str)  
  }
  
  it should "warn when the interface name at the end does not match" in {
    val str = "(interface Fred \ninterface Bob)" 
    val ter = tryWithParser(str, 0, 1)  
    assertResult( "Name does not match." )( ter.getWarningText(0) )
    assertResult( AST.Coord("<unknown>",2,11) )( ter.getWarningCoord(0) ) ;
  }
 
  it should "parse a class with generic args" in {
    val str = "(class Fred{type a, type b extends B}() class)" 
    tryWithParser(str)  
  }
 
  it should "parse a class with constructor args" in {
    val str = "(class Fred{ type a, type b extends B}(in x : X, obj y : Y) class)" 
    tryWithParser(str)  
  }
 
  it should "parse a class with declarations" in {
    val str = """(class Fred{ type a, type b extends B}
                            (in x : X, obj y : Y)
                     obj u:U := nil   obj v : V := nil    obj w : W := nil
                 class)""" 
    tryWithParser(str)  
  }
 
  it should "parse obj declarations" in {
    val str = "obj x : X := y obj y : Y := x" 
    tryWithParser(str) 
  }
    
    it should "reject mismatched assignment length 0" in {
        
        val str = """(class Fred()
                         (thread 
                            obj x := 0
                            obj y := 1
                            x, y := x
                         )
                     class)""" 
        val ter = tryWithParser(str, 1, 0)
        assertResult("Left and right hand sides of := are not the same length")( ter.getFatalText(0) )
        assertResult( AST.Coord("<unknown>", 5, 34) )( ter.getFatalCoord(0) )
    }
    
    it should "reject mismatched assignment length 1" in {
        
        val str = """(class Fred()
                         (thread 
                            obj x := 0
                            obj y := 1
                            x, y := x, y, x
                         )
                     class)""" 
        val ter = tryWithParser(str, 1, 0) 
        assertResult("Left and right hand sides of := are not the same length")( ter.getFatalText(0) )
        assertResult( AST.Coord("<unknown>", 5, 34) )( ter.getFatalCoord(0) )
    }
  
  it should "parse expressions with unicode characters" in {
    def show( str : String ) {
      for( ch <- str ) { printf("%d, %x", ch.toInt, ch.toInt) ; println  }
      println }
    val andSign = "\u2227"
    val orSign = "\u2228"
    val notSign = "\u00ac"
    val notEqSign = "\u2260"
    val lteq0 = "\u2264"
    val lteq1 = "\u2a7d"
    val gteq0 = "\u2265"
    val gteq1 = "\u2a7e"
    val implies = "\u21d2"
    val followsFrom = "\u21d0"
    val bicond = "\u21d4"
    val str0 = ("obj d : Int := 8 ; obj b : Int := 9 ;\n"
              +"obj c := (a "+lteq0+" b "+gteq0+" a "+andSign+notSign+"a ≠ b "+notEqSign+" a) "
              +implies+" ( a "+bicond+" (a ⩽ b ⩾ a) )" )
    show(str0)
    tryWithParser(str0)
    val str1 = ("obj e : Int := 8 ; obj b : Int := 9 ;\n"
              +"obj c := (a "+lteq1+" b "+gteq1+" a "+andSign+notSign+"a ≠ b "+notEqSign+" a) "
              +followsFrom+" ( a "+bicond+" (a ⩽ b ⩾ a) )" )
    show(str1)
    tryWithParser(str1) }
 
  it should "parse a class with a thread" in {
    val str = """(class Fred()
                     (thread 
                        obj x := 0
                        obj y := 1
                        (if  x = y
                        then x := y
                        else x,y := y,x x := y if)
                     )
                 class)""" 
    tryWithParser(str)  
  }
}

