package verifierTests
import org.scalatest.FlatSpec
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
      val (errorRecorder, outputBuilder) = hb.runHarpoToBoogieTrans( true )
      assertResult( 0 )( errorRecorder.getFatalCount() ) 
      val boogieCode: String = outputBuilder.result().mkString("\n")
      boogieCode
  }

  behavior of "The Boogie back end with Harpo 'Buffer' class";
  it should "generate Boogie code for Class Declaration " in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          class) """
    val expBoogie = """  """ // TODO
    val genBoogie = getBoogie("Class Declaration", str)
    assertResult(expBoogie)(genBoogie)

  }
  it should "generate Boogie code for Interface Declaration " in {
    val str = """ """

    val expBoogie = """  """ // TODO
    val genBoogie = getBoogie("Interface Declaration", str)
    assertResult(expBoogie)(genBoogie)
  }
  it should "generate Boogie code for Object Declaration/Initialization " in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	obj c: Int32:=0;
          class) """

    val expBoogie = """  """ // TODO
    val genBoogie = getBoogie("Object Declaration", str)
    assertResult(expBoogie)(genBoogie)
  }
  it should "generate Boogie code for Constant Declaration" in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	const obj c: Int32:=0; 
          class) """
    val expBoogie = """  """ // TODO
    val genBoogie = getBoogie("Constant Declaration", str)
    assertResult(expBoogie)(genBoogie)
  }
  it should "generate Boogie code for Boolean Expression" in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	obj c: Bool:= 3 /\ 5;
          class) """
    val expBoogie = """  """ // TODO
    val genBoogie = getBoogie("Boolean Expression", str)
    assertResult(expBoogie)(genBoogie)
  }
  it should "generate Boogie code for Chain Expression " in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	obj c: Bool:= 4 /\ 5 \/ 9;
          class) """
    val expBoogie = """  """ // TODO
    val genBoogie = getBoogie("Chain Expression", str)
    assertResult(expBoogie)(genBoogie)
  }
  it should "generate Boogie code for Arithmetic Expression" in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	obj c: Int32:= 450 + 30 - 200*5; 
          class) """
    val expBoogie = """  """ // TODO
    val genBoogie = getBoogie("Arithmetic Expression", str)
    assertResult(expBoogie)(genBoogie)
  }

  it should "generate Boogie code for assert command inside thread" in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	obj c: Int32:=0;
          	(thread(*t0*) 
          		assert c>9
          	thread) 
          class) """
    val expBoogie = """  """ // TODO
    val genBoogie = getBoogie("Assert Command", str)
    assertResult(expBoogie)(genBoogie)
  }

  it should "generate Boogie code for assume command inside thread" in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	obj c: Int32:=0;
          	(thread(*t0*) 
          		assume c>9
          	thread) 
          class) """
    val expBoogie = """  """ // TODO
    val genBoogie = getBoogie("Assume Command", str)
    assertResult(expBoogie)(genBoogie)
  }

  it should "generate Boogie code for Assignment Command" in {
    val str = """
          //Math Class in HARPO/L
          (class Math() 
          	obj c: Int32:=0;
          	(thread(*t0*) 
          		obj b: Int32 := c+4;
          	thread) 
          class) """
    val expBoogie = """  """ // TODO
    val genBoogie = getBoogie("Assignment Command", str)
    assertResult(expBoogie)(genBoogie)
  }
  it should "generate Boogie code for While Command" in {
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
    val expBoogie = """  """ // TODO
    val genBoogie = getBoogie("While Command", str)
    assertResult(expBoogie)(genBoogie)
  }
  it should "generate Boogie code for For Command" in { // TODO
  }
  it should "generate Boogie code for if Expression" in { // TODO
  }
  it should "generate Boogie code for Thread Declaration" in { // TODO
  }
  it should "generate Boogie code for Method Declaration" in { // TODO
  }
  it should "generate Boogie code for Method Call" in { // TODO
  }
  it should "generate Boogie code for Class Constructor" in { // TODO
  }
  it should "generate Boogie code for Co Command" in { // TODO
  }

}
