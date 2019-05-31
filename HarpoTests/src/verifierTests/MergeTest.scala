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
class Scenario3Test extends TestsBase {
  
  behavior of "The Boogie back end with Harpo 'Counter' class";
  it should "generate Boogie code for Merge class" in {
    
    var str = """
// Merge class to implement the merge algorithm on two segments of // an array

    (class M()

	      (in length : int32, onj a,b : real[length])
	         pre a!=b
	         pre 0<length

	       proc merge(in left: int32, in right: int32, in end : int32)
	          pre 0_<left^left<right ^right_<end /\ end < length(a)
	          takes {i:{left,..,end}.a(i)}
	          takes {i:{left,..,end}.b(i)}
	          pre(forall i,j:left_<i /\ i < j /\ j < right.a[i] _< a[j])
	          pre(forall i,j:right _<i /\ i < j /\ j _< end.a[i] _< a[j])
	          post(forall i,j:left_<i /\ i < j /\ j _< end.a[i] _< a[j])
	          gives{i:{left,..,end}.a(i)}
	          gives{i:{left,..end}.b(i)}

	  (thread(*t0*)
		    (accept merge(in left:int32, int32, in right:int32,in end:int32)
		      obj ileft:int32 :=left
		      obj iright:int32 :=right
		      obj k:int32 := right
		      obj k:int32:=left
		     (while (left _<k /\ k_<end)
			      invariant acc left@0.5,right@0.5,end@0.5,k,ileft,iright,{i:{left,..,end}.a[i]},{i:{left,..,end}.b[i]}
			      invariant (left _< k /\ k _< end+1)
			      invariant (left _< ileft /\ ileft _< right)
			      invariant (right _< iright /\ iright_end+1)
			      invariant (forall i,j:(left _< i /\ i < k /\ ileft _< j /\ j < right).b[i] _< a[j])
			      invariant (forall i,j:(left _< i /\ i < k /\ iright _< j /\ j < end).b[i] _<a [j])
			      invatiant (forall i,j:(left _< i /\ i _< j /\ j < k).b[i] _< b[j]);
			
			  (if(ileft < iright /\ iright _< end /\ k_<end))
				(if(a[ileft] _< a[iright]))
					b[k] := a[ileft];
					ileft := ileft+1
				(else
					b[k] :=a[iright]
					iright := iright+1)
			   if)
			  k:=k+1
			  (else if(ileft=right /\ iright _< end /\ k_<end)
			  b[k]:=a[iright]
			  iright:=iright+1
			  k:=k+1)
			
			 (else if(iright = end+1 /\ ileft < right /\ k_<end)
			  b[k] :=a[ileft]
			  ileft:=ileft+1
			  k=k+1)
			if)
			while)
			
			k:=left
			(while (left_<k /\ k_<end)
				invariant acc left@0.5,end@0.5,k {i:{left,..end}.a[i]},{i:{left,..,end}.b[i]}
				invariant(left_<k /\ k_<end+1)
				invariant(forall i:(left_<i /\ i<k)==> a[i]=b[i]);
				a[k]:=b[k]
				k:=k+1
			while)
		accept)
	thread)
class) """

    var BoogieSource = tryWithBoogieBackEnd(str)
    
    println(BoogieSource)

}
}