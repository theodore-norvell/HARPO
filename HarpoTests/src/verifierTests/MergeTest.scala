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
import util.OutputBuilder;

@RunWith(classOf[JUnitRunner])
class MergeTest extends VerifierTestBase {
  
  
  behavior of "The Boogie back end with Harpo 'Merge' class";
  it should "generate Boogie code for Merge class" in {
    
    val str = """
// Merge class to implement the merge algorithm on two segments of // an array

(class M(in size : Int32, obj a : Real64[size],obj b : Real64[size])
	         pre a ~= b
	         pre 0 < size

	       proc merge(in left: Int32, in right: Int32, in end : Int32)	          
	          pre 0 _< left /\ left < right /\ right _< end /\ end < length(a)
	          takes {i:{left,..,end} do a[i]}
	          takes {i:{left,..,end} do b[i]}
	          pre(forall i,j: left _< i /\ i < j /\ j < right do a[i] _< a[j])
	          pre(forall i,j: right _<i /\ i < j /\ j _< end do a[i] _< a[j])
	          post(forall i,j: left _<i /\ i < j /\ j _< end do a[i] _< a[j])
	          gives {i:{left,..,end} do a[i]}
	          gives {i:{left,..end} do b[i]}

	  (thread(*t0*)
		    (accept merge(in left: Int32, in right:Int32,in end:Int32)
		      obj ileft:Int32 := left;
		      obj iright:Int32 := right;
		      obj k:Int32:= left;
		     (while (left _< k /\ k _< end)
			      invariant acc left@0.5,right@0.5,end@0.5,k,ileft,iright,{i:{left,..,end} do a[i]},{i:{left,..,end} do b[i]}
			      invariant (left _< k /\ k _< end+1)
			      invariant (left _< ileft /\ ileft _< right)
			      invariant (right _< iright /\ iright _< end+1)
			      invariant (forall i,j:(left _< i /\ i < k /\ ileft _< j /\ j < right) do b[i] _< a[j])
			      invariant (forall i,j:(left _< i /\ i < k /\ iright _< j /\ j < end) do b[i] _<a [j])
			      invariant (forall i,j:(left _< i /\ i _< j /\ j < k) do b[i] _< b[j]);
			do
			  	(if(ileft < iright /\ iright _< end /\ k _<end)
			  		then
			  				(if(a[ileft] _< a[iright])
			  					then
			  						b[k] := a[ileft];
			  						ileft := ileft+1;
			  				else
			  					b[k] :=a[iright];
			  					iright := iright+1;
			  				if)
			  	(if(ileft = right /\ iright _< end /\ k _< end)
			  		then
			  				b[k]:=a[iright];
			  				iright:=iright+1;
			  				k:=k+1;
			   	)
			
			  	(if(iright = end+1 /\ ileft < right /\ k _< end)
			  		then
			  				b[k] := a[ileft];
			  				ileft := ileft+1;
			  				k := k+1;
			  	)
				if)
			while)
			
			k:=left
			
			(while (left _< k /\ k _< end)
				invariant acc left@0.5,end@0.5,k,{i:{left,..end} do a[i]},{i:{left,..,end} do b[i]}
				invariant (left _< k /\ k _< end+1)
				invariant (forall i: (left _< i /\ i < k) => a[i] = b[i]);
			 do
				a[k]:=b[k]
				k:=k+1
			while)
		accept)
	thread)
class)



 """

 val (errors : StandardErrorRecorder, boogieCode: OutputBuilder) = translateAndVerify(str)

}
}
