package verifierTests
import org.scalatest.FlatSpec
import scala.util.matching.Regex
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfterEach
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
import executive.HarpoToBoogieTranslator
import util.OutputBuilder;

@RunWith(classOf[JUnitRunner])
class BackendTests extends FlatSpec with BeforeAndAfterEach {

  val out = new OutputStreamWriter(System.out)

  def getBoogie(fileName: String, fileContent: String) = {
    var hb = new HarpoToBoogieTranslator()
    hb.addFile(fileName, fileContent)
    val (errorRecorder, outputBuilder) = hb.runHarpoToBoogieTrans(true)
    println("Total Fetal Count For " + fileName + " " + errorRecorder.getFatalCount())
    errorRecorder.printErrors(System.out)
    assertResult(0)(errorRecorder.getFatalCount())
    val boogieCode: String = outputBuilder.resultAsString()
    boogieCode
  }

  def findSnippets(fileName: String, expBoogieList: List[String], genBoogie: String): Boolean = {

    var flag = false;
    var result = ""
    println("=============== Found Match From File Name " + "\'" + fileName + "\'" + "==============")
    for (expLine <- expBoogieList) {
      val result$ = (expLine.r findAllIn genBoogie).mkString("\n")
      flag = if (result$.isEmpty()) false else true
      result += result$ + ("\n")
    }
    println(result)
    println("======================================================================================= ")
    flag
  }

  behavior of "The Boogie backend";

  it should "generate code for Class Declaration" in {
    val fileName = "ClassDeclaration"
    val str = """
                (class Math() 
                 class) """
    val genBoogie = getBoogie(fileName, str)

    val expBoogie = List(
      ".*const *unique *Math *: *ClassName *;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }

  it should "generate code for Interface Declaration " in {
    val fileName = "Interface_Declaration"

    val str = """(interface MyInterface
                    interface)"""
    val genBoogie = getBoogie(fileName, str)

    val expBoogie = List(
      ".*const +unique +MyInterface *: *ClassName *;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)

  }

  it should "generate code for Object Declaration and Initialization inside constructor procedure " in {
    val fileName = "ObjectDeclaration"
    val str = """
            (class Math()
            	obj c: Int32:=0;
            class) """

    val genBoogie = getBoogie(fileName, str)

    val expBoogie = List(
      ".*const *unique *Math.c *: *Field *int *;.*",
      ".*Heap\\[This.Math,Math.c\\] *:= *0 *;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)

  }

  it should "generate code for ghost Object Declaration and Initialization inside constructor procedure " in {
    val fileName = "Ghost_Object_Declaration"
    val str = """
            (class Math()
            	ghost obj c: Int32:=0;
            class) """

    val genBoogie = getBoogie(fileName, str)

    val expBoogie = List(
      ".*const *unique *Math.c *: *Field *int *;.*",
      ".*Heap\\[This.Math,Math.c\\] *:= *0 *;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }

  it should "generate code for const Object Declaration and Initialization inside constructor procedure " in {

    val fileName = "Const_Object_Declaration"
    val str = """
            (class Math()
            	const c: Int32:=0;
            class) """

    val genBoogie = getBoogie(fileName, str)

    val expBoogie = List(
      ".*const *unique *Math.c *: *Field *int *;.*",
      ".*Heap\\[This.Math,Math.c\\] *:= *0 *;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }

  it should "generate code for ghost constant Object Declaration and Initialization inside constructor procedure " in {
    val fileName = "Ghost_Object_Declaration_And_Initialization"
    val str = """
            (class Math()
            	ghost const c: Int32:=0;
            class) """

    val genBoogie = getBoogie(fileName, str)

    val expBoogie = List(
      ".*const *unique *Math.c *: *Field *int *;.*",
      ".*Heap\\[This.Math,Math.c\\] *:= *0 *;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }

  it should "generate the boogie types from HARPO primitive Types" in {
    val fileName = "Type_Conversion"
    val str = """
                  (class Math()
                      obj a: Int8 := 0;
                      obj b: Int16 := 0;
                      obj c: Int32 := 0;
                      obj d: Int64 := 0;
                      obj e: Real16 := 0.0;
                      obj f: Real32 := 0.0;
                      obj g: Real64 := 0.0;
                      obj h: Bool := true;
                      obj i: Int8[10] := (for i: 10 do 0 for)
                  class)
                """
    val genBoogie = getBoogie(fileName, str)

    val expBoogie = List(
      ".*const *unique *Math.[a-i] *: *Field *(int|real|bool) *;.*",
      ".*Heap\\[This.Math,Math.[a-i]\\] *:= *(0|0.0|true|TODO) *;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }

  it should "generate code for Boolean Initialization Expression" in {
    val fileName = "Initialization_with_Boolean_Expression"
    val str = """
            //Math Class in HARPO/L
            (class Math()
            	obj c: Bool:= true /\ false;
            class) """

    val genBoogie = getBoogie(fileName, str)

    val expBoogie = List(
      ".*const *unique *Math.c *: *Field *bool *;.*",
      ".*Heap\\[This.Math,Math.c\\] *:= *true *\\&\\& *false *;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }
  it should "generate code for Chained Initialization Expression " in {
    val fileName = "Chained_Initialization_Expression"
    val str = """
            //Math Class in HARPO/L
            (class Math()
            	obj c: Bool:= false /\ true \/ true;
            class) """
    val genBoogie = getBoogie(fileName, str)

    val expBoogie = List(
      ".*const *unique *Math.c *: *Field *bool *;.*",
      ".*Heap\\[This.Math,Math.c\\] *:= *false *\\&\\& *\\ *true *\\|\\| *true *;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }

  it should "generate code for Arithmetic Initialization Expression" in {
    val fileName = "Arithmetic_Initialization"
    val str = """
            //Math Class in HARPO/L
            (class Math()
            	obj c: Int32:= 450 + 30 - 200*5;
            class) """

    val genBoogie = getBoogie(fileName, str)

    println(genBoogie)

    val expBoogie = List(
      ".*const *unique *Math.c *: *Field *int *;.*",
      ".*Heap\\[This.Math,Math.c\\] *:= *450 *\\+ *30 *\\- *200 *\\* *5 *;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }

  it should "Translate the Initialization Expressions from HARPO to Boogie" in {
    val fileName = "Initialization_Expression"
    val str = """
                  (class Math()
                      obj a: Int8 := 5+9;
                      obj b: Int16 := 8*6+9;
                      obj c: Int32 := 2000 div 10;
                      obj d: Int64 := 4000000*8;
                      obj e: Real16 := 2000.035/89.08;
                      obj f: Real32 := 96.0*85.0-42.3+3000.2*23.220014;
                      obj g: Real64 := 0.00000000005658/96.22;
                      obj h: Bool := true /\ false \/ true;
                      obj i: Bool := (5 _< 89) => (4 _< 89)
                      obj j: Bool := (4 _< 89) <= (5 _< 89)
                      obj k: Bool := (5 _< 89) <=> (4 _< 89)
                      obj l: Bool := ~ (true)
                      obj m: Bool := (5 = 89)
                      obj n: Bool := (5 _< 89)
                      obj o: Bool := (5 >_ 89)
                      obj p: Bool := (5 ~= 89)
                      obj q: Bool := (5 < 89)
                      obj r: Bool := (5 > 89)
                      obj s: Int32 := 1000 mod 20
                      obj t: Bool := (5 _< 89)
                      obj u : Int32 := a - 20
                  class)
                """

    val genBoogie = getBoogie(fileName, str)

    println(genBoogie)

    val expBoogie = List(
      ".*const *unique *Math.[a-u] *: *Field *(int|real|bool) *;.*",
      ".*Heap\\[This.Math,Math.[a-u]\\] *:= *.*;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)

  }

  it should "generate code for class claim specification" in {
    val fileName = "Class_Claim_Specification"
    val str = """
            //Math Class in HARPO/L
            (class Math()
              claim c@1.0, d@0.000003, e@0.1000003000000007
            	obj c: Int32:=0;
              obj d: Int32:=0;
              obj e: Int32:= 1;
            	(thread(*t0*)
            		assert c>9
            	thread)
            class) """

    val genBoogie = getBoogie(fileName, str)

    val expBoogie = List(
      ".*assert IsValidPermission\\(([0-9]+.[0-9]+)\\)",
      ".*Permission\\[This.Math,Math.(c|d|e) *\\] *:= *Permission\\[This.Math,Math.(c|d|e) *\\] *\\+ *([0-9]+.[0-9]+) *;.*",
      "assert *0.0 *<= *Permission\\[This.Math,Math.(c|d|e)\\] *\\&\\& *Permission\\[This.Math,Math.(c|d|e)\\] *<= *1.0 *;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }

  it should "generate code for 'assert' command inside thread" in {
    val fileName = "assert_command"
    val str = """
            //Math Class in HARPO/L
            (class Math()
            	obj c: Int32:=0;
            	(thread(*t0*)
            		assert (c>9 /\ c<100) /\ (c ~= 20 /\ c~= 60)
            	thread)
            class) """

    val genBoogie = getBoogie(fileName, str)
    println(genBoogie)
    val expBoogie = List(
      ".*assert +isInt32\\((9|100|20|60|Heap\\[This.Math,Math.c\\])\\).*",
      ".*assert +Permission\\[This.Math,Math.c\\] *> *0.0.*",
      ".*assert *Heap\\[This.Math,Math.c\\] *> *9 *\\&\\& *Heap\\[This.Math,Math.c\\] *< *100 *\\&\\& *Heap\\[This.Math,Math.c\\] *!= *20 *\\&\\& *Heap\\[This.Math,Math.c\\] *!= *60 *;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }

  it should "generate the code for Local Variable Declaration" in {
    val fileName = "Local_Variable_Declaration_Assignment"
    val str = """
            //Math Class in HARPO/L
            (class Math()
            	obj c: Int32:=0;
            	(thread(*t0*)
            		obj b: Int32 := c+4;
            	thread)
            class) """
    val genBoogie = getBoogie(fileName, str)

    val expBoogie = List(
      ".*assert +isInt32\\((Heap\\[This.Math,Math.c\\]|4)\\).*",
      ".*Heap\\[This.Math,Math.t#0.b\\] *:= *Heap\\[This.Math,Math.c\\] *\\+ *4 *;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }

  it should "generate the code for defindness of Bool type Local Variable Declaration" in {
    val fileName = "Bool_Type_Local_Variable_Declaration_Defindness";
    val str = """
            //Math Class in HARPO/L
            (class Math()
            	obj c: Bool:= false;
            	(thread(*t0*)
            		obj b: Bool := c /\ false;
            	thread)
            class) """
    val genBoogie = getBoogie(fileName, str)

    println(genBoogie)
    val expBoogie = List(
      ".*assert +isBool\\((Heap\\[This.Math,Math.c\\]|false)\\).*",
      ".*Heap\\[This.Math,Math.t#0.b\\] *:= *Heap\\[This.Math,Math.c\\] *\\&\\& *false *;.*")

    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }

  it should "generate code for 'assume' command inside thread" in {
    val fileName = "assume_command"
    val str = """
            //Math Class in HARPO/L
            (class Math()
            	obj c: Int32:=0;
            	(thread(*t0*)
                  obj d:Real32 := 34.98765;
            		  assume c>9 /\ d < 35
            	thread)
            class) """
    val genBoogie = getBoogie(fileName, str)
    val expBoogie = List(
      ".*assert *isReal64\\(34.98765\\) *;.*",
      ".*Heap\\[This.Math,Math.c\\] *:= *34.98765 *;.*",
      ".*assert *isInt32\\((9|35|Heap\\[This.Math,Math.c\\])\\) *;.*",
      ".*assert *isReal32\\(Heap\\[This.Math,Math.t#0.d\\]\\) *;.*",
      ".*assert *Permission\\[This.Math,Math.c\\] *> 0.0 *\\&\\& *Permission\\[This.Math,Math.t#0.d\\] *> *0.0 *;.*",
      ".*assume *Heap\\[This.Math,Math.c\\] *> 9 *\\&\\& *Heap\\[This.Math,Math.t#0.d\\] *< *35 *;.*")
    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }

  it should "generate the code for if command with defined guard" in {
    val fileName = "if_Command"
    val str = """
            //Math Class in HARPO/L
            (class Math()
            	obj c: Int8:= 20;
            	(thread(*t0*)
            		(if(c _< 21) then obj b: Bool := false;)
            	thread)
            class) """
    val genBoogie = getBoogie(fileName, str)
    val expBoogie = List(
      ".*assert *isInt8\\(Heap\\[This.Math,Math.c\\]\\) *;.*",
      ".*assert *isInt32\\(21\\) *;.*",
      ".*assert *Heap\\[This.Math,Math.c\\] *> *0.0 *;.*",
      ".*if *\\(.*\\) *\n* *\\{ *\n*([^\\}]*).*")
    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }

  it should "generate code for While Command" in {
    val fileName = "While_Command"
    val str = """
            //Math Class in HARPO/L
            (class Math()
            	obj c: Int32:=0;
            	(thread(*t0*)
                (while(true)
                  do
            		    c := c+1;
                 while)
            	thread)
            class) """

    val genBoogie = getBoogie(fileName, str)
    val expBoogie = List(".*while *\\( *true *\\) *\n* *invariant *\\(.*?\\) *; *\n* *\\{ *\n*([^\\}]*).*")
    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }
  
    it should "generate code for Division By Zero Exp" in { //Let it fail
    val fileName = "Div_By_Zero"
    val str = """
            //Math Class in HARPO/L
            (class Math()
            	obj c: Real64:=0.0;
            	(thread(*t0*)
            		    c := 1/0;
            	thread)
            class) """

    val genBoogie = getBoogie(fileName, str) 
      println(genBoogie)
    val expBoogie = List(".*")
    val result = findSnippets(fileName, expBoogie, genBoogie)

    assertResult(result)(true)
  }
  
  it should "generate code for 'For_Command' " in {
    val fileName = "For_Command"
    val str = """
            //Math Class in HARPO/L
            (class Math()
            	obj c: Int32:=20;
              obj f: Int64 := 0;
            	(thread(*t0*)
                (for x : c do
                  f := x+1
                for)
            	thread)
            class) """
    val genBoogie = getBoogie(fileName, str)
    val expBoogie = List(".*while *\\(.*\\) *\n* *\\{ *\n*([^\\}]*).*")
    val result = findSnippets(fileName, expBoogie, genBoogie) 
  }

  it should "generate the code for Call to Multiply method declaration" in {
   val fileName = "Call_Command"
    val str = """
                (class Math()
                obj a : Int32 := 0;
                obj res : Int32 := 0;
                proc Mul(in a : Int32, in b : Int32)
                  pre a > 0 /\ b > 0
                  post res > 0
                (thread (*t0*)
                  obj argument : Int32 := 0;
                  Mul(argument,1)
                thread)
                class)
                """
    val genBoogie = getBoogie(fileName, str)
    println(genBoogie)
    val expBoogie = List()
    val result = findSnippets(fileName, expBoogie, genBoogie) 
  }
  //
  //  it should "generate code for 'Method Decl Call' " in {
  //    val str = """(class Math()
  //                  obj a : Int32 := 21;
  //                  obj b : Int32 := 31;
  //                  obj sum : Int32 :=0;
  //                  proc add()
  //                    pre a > 20 /\ b > 10
  //                    post sum > 30
  //                  (thread (*t0*)
  //                    (accept add()
  //                        sum := a+b;
  //                    accept)
  //                  thread)
  //
  //                  (thread (*t1*)
  //                    add()
  //                  thread)
  //                class)
  //              """
  //    val genBoogie = getBoogie("Method_decl_call", str)
  //    println(genBoogie)
  //    // TODO
  //  }
  //
  //  it should "generate the code for Permission(ObjectId) Function" in {
  //    val str = """(class Math()
  //                  claim x@1.0
  //                  invariant permission(x) = 1.0
  //                  obj x : Int32 := 0;
  //                  class)
  //                  """
  //    val genBoogie = getBoogie ("Permission_Fetch", str)
  //    println(genBoogie)
  //  }
  //  it should "generate code for 'Method Declaration'" in {
  //    // TODO
  //  }
  //  it should "generate code for 'Method Call' " in {
  //    // TODO
  //  }
  //  it should "generate code for 'Class Constructor' " in {
  //    // TODO
  //  }
  //  it should "generate code for 'Co Command' " in {
  //    // TODO
  //  }
  //
  //
  //
  //
  //
  //
  //
  //

}
