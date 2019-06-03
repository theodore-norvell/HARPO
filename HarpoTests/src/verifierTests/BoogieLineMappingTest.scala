package verifierTests
import scala.sys.process._
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
class BoogieLineMappingTest extends TestsBase {
  val out = new OutputStreamWriter(System.out)
  behavior of "The Boogie back end";

  it should "map boogie line numbers to Harpo line numbers" in {
    val str = """
          (class Alice()

              (thread
                  assert 1 = 2 
              thread)
          class)

"""

    val BoogieSource = tryWithBoogieBackEnd(str)
    assert( false, "todo more" )
  }
}
