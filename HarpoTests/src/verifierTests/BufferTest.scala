/* 
 * These tests contains all the variations of the counter class
 * @Author: Inaam Ahmed
 * */

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
class BufferTest extends VerifierTestBase {
  val fileName = getClass.getName
  behavior of "The Boogie back end with Harpo 'Buffer' class";
  it should "generate Boogie code for Buffer class" in {
    
    val str = """

          (class Buffer()

	          proc deposit(in value : Real64)

	          proc fetch(out value :  Real64)

	          const size : Int32 := 10 
	          obj buf : Real64[size] := (for i:size do 0 for)
	          obj front : Int32 :=0
	          obj rear : Int32 :=0
	          obj full : Int32 :=0

	          (thread (*t0*) claim front, rear, full, {i:{0,..size} do buf[i]}
		          (while true
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
        class)
"""

 val (errors, builder) = translateAndVerify(fileName,str)

}
}
