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
class ParserTests extends BoogieTestsBase {

  "The parser" should "parse an empty file" in {
    val str = ""
    tryWithParser(str)
  }

  it should "parse a ghost object" in {
    val str = "ghost obj c:Int16"
    tryWithParser(str)
  }
  it should "parse an invariant " in {
    val str = "invariant x>9"
    tryWithParser(str)
  }
  it should "parse a claim" in {
    val str = "claim x"
    tryWithParser(str)
  }

  it should "allow an assert command" in {
    val str = "(class Fred(assert c=4;) class Fred)"
    tryWithParser(str)
  }

  it should "allow an assert command inside thread" in {
    val str = "(class Math()	public proc add()	(thread (*t0*)(while true do (accept add() c:=2+2;assert c=4;accept)	while) thread) class)"
    tryWithParser(str)
  }
  
    it should "allow an assume command" in {
    val str = "(class Fred(assume c=4;) class Fred)"
    tryWithParser(str)
  }

  it should "allow an assume command inside thread" in {
    val str = "(class Math()	public proc add()	(thread (*t0*)(while true do (accept add() c:=2+2;assume c=4;accept)	while) thread) class)"
    tryWithParser(str)
  }
  
  // Constructor Arguments of "in" object category
  
  it should "parse a class with constructor args" in {
    val str = "(class Fred{ type a, type b extends B}(in x : X, obj y : Y) class)"
    tryWithParser(str)
  }

  // Class with ghost variable declarations
  
  it should "parse a class with declarations" in {
    val str = """(class Fred{ type a, type b extends B}
                            (in x : X, obj y : Y)
                     ghost obj u:U := nil  ghost obj v : V := nil ghost obj w : W := nil
                 class)"""
    tryWithParser(str)
  }

  it should "parse ghost obj declarations" in {
    val str = "ghost obj x : X := y ghost obj y : Y := x"
    tryWithParser(str)
  }

  it should "parse ghost const declarations" in {
   val str = "ghost const x : X := y ghost const y : Y := x"
   tryWithParser(str)
  }
  
  behavior of "The Boogie back end with Harpo 'Buffer' class";
  it should " parse the assert command inside thread " in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	obj c: Int32:=0;
          	(thread(*t0*) 
          		assert c>9
          	thread) 
          class) """
  }

  it should "parse ghost Object Declaration/Initialization " in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	ghost obj c: Int32:=0;
          class) """
  }
  
  it should "parse ghost Const Declaration/Initialization " in {
  val str = """
          //Math Class in HARPO/L
          (class Math() 
          	ghost const c: Int32:=0;
          class) """
  }
  
  it should "Parse Boolean Expression" in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	obj c: Bool:= 3 /\ 5;
          class) """
  }
  it should "parse Chain Expression " in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	obj c: Bool:= 4 /\ 5 \/ 9;
          class) """
  }
  it should "parse Arithmetic Expression" in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	obj c: Int32:= 450 + 30 - 200*5; 
          class) """
  }
  it should "parse Assignment Command" in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	obj c: Int32:=0;
          	(thread(*t0*) 
          		obj b: Int32 := c+4;
          	thread) 
          class) """
  }
  it should "parse While Command" in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	obj c: Int32:=0;
          	(thread(*t0*)
              (while(true) 
          		    //do nothing
               while)
          	thread) 
          class) """
  }
  it should "generate Boogie code for For Command" in {
  }
  it should "generate Boogie code for if Expression" in {
  }
  it should "generate Boogie code for Thread Declaration" in {
  }
  it should "generate Boogie code for Method Declaration" in {
  }
  it should "generate Boogie code for Method Call" in {
  }
  it should "generate Boogie code for Class Constructor" in {
  }
  it should "generate Boogie code for Co Command" in {
  }

}

