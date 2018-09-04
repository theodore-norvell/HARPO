package verifierTests
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
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

@RunWith(classOf[JUnitRunner])
class UnitTests extends BufferSystemTestsBase {
  val out = new OutputStreamWriter(System.out)
  behavior of "The Boogie back end with Harpo 'Buffer' class" ;

  it should "generate Boogie code for Class Declaration " in {
  }
  it should "generate Boogie code for Interface Declaration " in {
  }
  it should "generate Boogie code for Object Declaration/Initialization " in {
  }
  it should "generate Boogie code for Constant Declaration" in {
  }
  it should "generate Boogie code for Field Declaration" in {
  }
  it should "generate Boogie code for Boolean Expression" in {
  }
  it should "generate Boogie code for Chain Expression " in {
  }
  it should "generate Boogie code for Arithmetic Expression" in {
  }
  it should "generate Boogie code for Assignment Command" in {
  }
  it should "generate Boogie code for While Command" in {
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