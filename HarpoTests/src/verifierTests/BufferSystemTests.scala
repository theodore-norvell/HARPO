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
class BufferSystemTests extends BufferSystemTestsBase {
  val out = new OutputStreamWriter(System.out)
  behavior of "The Boogie back end with Harpo 'Buffer' class" ;

  it should "generate Boogie code for buffer class" in {
    val str = """
          //Buffer Class in HARPO/L
          (class Buffer()
    	        proc deposit(in value : real)
    	        proc fetch(out value :  real)
    	        obj buf : real[size] := (for i:size do 0 for)
    	        obj front : int32 :=0
            	obj rear : int32 :=0
            	obj full : int32 :=0
            	const size : int32 := 10
            	(thread (*t0*) claim front, rear, full
                		(while true
                			invariant canRead(front)
                			invariant(0_<front /\ front < size) /\ (0_<rear /\ rear < size) /\ (0 _<full /\ full < size)
                			invariant ((front+full) mod size = rear)
                      do
                  				(accept deposite (in value : real) when (full<size)
                      				buf[rear] := value
                      				rear := (rear+1) mod size
                      				full := full+1
                      				|
                      				fetch(out ovalue:real) when (0<full)
                      				ovalue := buf[front]
                      				front := (front+1) mod size
                      				full := full-1
                  				accept)
                		while)
            	thread)
          class)      
"""
    
    val BoogieSource = tryWithBoogieBackEnd(str)
    assert(BoogieSource === """
          type Ref;
          type Field x;
          type HeapType = <x> [Ref,Field x]x;
          var Heap:HeapType;
          type ArrayRef x;
          type ArrayHeapType = <x> [ArrayRef x, int]x;
          var ArrayHeap:ArrayHeapType;
          type Perm = real;
          type PermissionType = <x> [Ref,Field x]Perm;
          type ArrayPermissionType = <x>[ArrayRef x, int] Perm;
          type GuardPermissionType = <x> [Ref,Field x]Perm;
          var LockPermission: PermissionType;
          var ArrayLockPermission: ArrayPermissionType;
          function Length<x>(Field (ArrayRef x))returns(int);
          const unique min8:int;
          axiom min8 == -128;
          const unique max8:int;
          axiom max8==127;
          const unique min16:int;
          axiom min16 == -32768;
          const unique max16:int;
          axiom max16 == 32767;
          const unique min32:int;
          axiom min32 == -2147483648;
          const unique max32:int;
          axiom max32 == 2147483647;
          const unique min64:int;
          axiom min8 == -9223372036854775808;
          const unique max64:int;
          axiom max8== 9223372036854775807;
          function Isint8(int) returns (bool);
          axiom (forall x:int :: Isint8(x) <==> min8 <=x&&x<=max8);
          function Isint16(int) returns (bool);
          axiom (forall x:int :: Isint16(x) <==> min16 <=x&&x<=max16);
          function Isint32(int) returns (bool);
          axiom (forall x:int :: Isint32(x) <==> min32 <=x&&x<=max32);
          function Isint64(int) returns (bool);
          axiom (forall x:int :: Isint64(x) <==> min64 <= x && x <= max64);
          // Buffer Translation
          type ClassName;
          function dtype(Ref) returns (ClassName);
          const unique Buffer:ClassName;
          const unique Buffer.buf:Field(ArrayRef real);
          const unique Buffer.front:Field int;
          const unique Buffer.rear:Field int;
          const unique Buffer.full:Field int;
          const unique Buffer.size:int;
          const unique g0:Field bool;
          const unique g1:Field bool;
          axiom Buffer.size==10;
          const unique Buffer.value :Field real;
          const unique Buffer.ovalue : Field real;
          procedure Buffer.t0(this:Ref)
          	modifies Heap, ArrayHeap;
          	requires dtype(this) <: Buffer;
                {
              	var oldHeap,preHeap,Heap_tmp:HeapType;
              	var oldArrayHeap,preArrayHeap,ArrayHeap_tmp:ArrayHeapType;
              	var Permission,oldPermission,prePermission:PermissionType;
              	var ArrayPermission,oldArrayPermission,preArrayPermission: ArrayPermissionType;
                var GuardPermission,oldGuardPermission,preGuardPermission:GuardPermissionType;
              	//initial permission
              	oldPermission := Permission;
              	havoc Permission;
              	assume (forall<x> r:Ref, f:Field x :: Permission [r,f] == 	0.0);
              	//array initial permission  
              	oldArrayPermission := ArrayPermission;
              	havoc ArrayPermission;
              	assume (forall<x> r:ArrayRef x, f: int :: ArrayPermission 	[r,f] == 0.0);             
                //guards initial permission              
                oldGuardPermission := GuardPermission;
                havoc GuardPermission;
              	assume (forall<x> r:Ref, f:Field x :: GuardPermission [r,f] == 	0.0);
              	//claim front,rear, full
              	oldPermission := Permission;
              	havoc Permission;
              	assume Permission[this,Buffer.front]==1.0;
              	assume Permission[this,Buffer.rear]==1.0;
              	assume Permission[this,Buffer.full]==1.0;
              	assume(forall <x> r:Ref, f:Field x :: !(r==this && 	f==Buffer.front) && !(r==this && 	f==Buffer.rear)&& !(r==this&&f==Buffer.full) ==> 	Permission[r,f] == oldPermission[r,f]);
              	//claim {i:{0,..size}.buf[i]}
              	oldArrayPermission := ArrayPermission;
              	havoc ArrayPermission;
              	assume (forall<x> r:ArrayRef x, f : int :: 	(r==Heap[this,Buffer.buf]) && (0<=f&& f<Buffer.size)==> 	ArrayPermission[r,f]==oldArrayPermission[r,f]);
                //claim GuardPermissions
                oldGuardPermission := GuardPermission;
                havoc GuardPermission;
                assume GuardPermission[this,g0]==1.0;
                assume GuardPermission[this,g1]==1.0;               
                //initial valuses of claimed locations
              	oldHeap:=Heap;
              	havoc Heap;
              	assume Heap[this,Buffer.rear] == 0;
              	assume Heap[this,Buffer.front]==0;
              	assume Heap[this,Buffer.full]==0;
              	assume (forall<x> r:Ref,f:Field x :: !(r==this && f==Buffer.front) && !(r==this && f==Buffer.rear) && !(r==this && f==Buffer.full) ==> Heap[r,f] == oldHeap[r,f]);
                oldArrayHeap := ArrayHeap;
              	havoc ArrayHeap;
              	assume(forall<x> r:ArrayRef x,f:int :: !((r==Heap[this,Buffer.buf])&& (0<=f&&f<Buffer.size))==> 	ArrayHeap[r,f]==oldArrayHeap[r,f]);
              	oldPermission := Permission;
              	oldArrayPermission := ArrayPermission;
              	oldHeap :=Heap;
              	oldArrayHeap := ArrayHeap;
              	while(true)
                  	invariant Permission[this,Buffer.front] == 1.0 &&	Permission[this,Buffer.rear] == 1.0 && Permission[this,Buffer.full] == 1.0 && (forall <x> r:ArrayRef x, f:int::(r==Heap[this,Buffer.buf])&&(0<=f &&f < Buffer.size) ==> ArrayPermission[r,f] == 1.0);
                  	invariant(forall<x> r:Ref, f:Field x :: !(r==this && f== 	Buffer.front)&& !(r==this && f== Buffer.rear) && !(r==this&& 	f==Buffer.full)==> Permission [r,f]==oldPermission[r,f]);
                  	invariant(forall<x> r:ArrayRef	x, f:int :: !((r==Heap[this,Buffer.buf])&&(0<=f&&f<Buffer.size))==>ArrayPermission[r,f]==oldArrayPermission[r,f]);
                  	invariant 0<= Heap[this, Buffer.front] && Heap[this, Buffer.front] 	< Buffer.size && (0<=Heap[this,Buffer.rear]&& Heap[this,Buffer.rear]	<Buffer.size)&& (0<Heap[this,Buffer.full] && 	Heap[this,Buffer.full]<=Buffer.size);         
                  	invariant((Heap[this,Buffer.front]+ Heap[this,Buffer.full])	mod Buffer.size) == Heap[this, Buffer.rear];	
                  	invariant(forall<x> r:Ref, f: Field x:: !(r==this && f==Buffer.front)&&!(r==this && f==Buffer.rear) && !(r==this && f== 	Buffer.full) && !(r==this&&f==Buffer.value) && ! (r== this && 	f== Buffer.ovalue) ==> Heap [r,f] == oldHeap[r,f]);      
                  	invariant(forall <x> r:ArrayRef x, 	f:int:: !((r==Heap[this,Buffer.buf])&&(0<=f && 	f<Buffer.size))==> ArrayHeap[r,f] == oldArrayHeap[r,f]);
                  	//while body
                        	{
                        	assert GuardPermission[this,g0] == 1.0;
                          assert GuardPermission[this,g1] == 1.0;
                          Heap[this,g0]:= 0 < Heap[this,Buffer.full];
                          Heap[this,g1] := Heap[this,Buffer.full]<Buffer.size;
                          assert (Heap[this,g0])||(Heap[this,g1]);
                          if(!(Heap[this,g0]))
                          {
                          goto fetch;
                          }
                          if(!(Heap[this,g1]))
                          {
                          goto deposit;
                          }
                        	goto deposit,fetch;                      
                            deposit:
                                      assert Permission[this,Buffer.full]>0.0;
                                      if(Heap[this,Buffer.full]<Buffer.size)
                                      {
                                      prePermission := Permission;
                                      Permission[this,Buffer.value]:= Permission[this,Buffer.value]+0.5;       
                                      if(prePermission[this,Buffer.value]==0.0)
                                      {
                                      assert Permission[this, Buffer.value]>0.0;
                                      havoc Heap_tmp;
                                      Heap[this,Buffer.value]:= Heap_tmp[this,Buffer.value];
                                      }    
                                      //deposite body
                                      preHeap := Heap;
                                      preArrayHeap := ArrayHeap;
                                      assert Permission[this, Buffer.rear] > 0.0 && Permission[this,Buffer.value]>0.0;
                                      assert ArrayPermission[Heap[this,Buffer.buf],Heap[this,Buffer.rear]] == 1.0;
                                      assert 0<= Heap[this,Buffer.rear] && Heap[this,Buffer.rear]< Buffer.size;
                                      assert Isint32(Heap[this,Buffer.rear]);
                                      ArrayHeap[Heap[this,Buffer.buf],Heap[this,Buffer.rear]] := Heap [this, Buffer.value];
                                      assert Permission[this,Buffer.rear]==1.0;
                                      assert Isint32(Heap[this,Buffer.rear]);
                                      Heap[this,Buffer.rear] := (Heap[this,Buffer.rear]+1) mod Buffer.size;
                                      assert Isint32(Heap[this,Buffer.full]);
                                      assert Permission[this,Buffer.full] == 1.0;
                                      assert Isint32(Heap[this,Buffer.full]);
                                      Heap[this,Buffer.full] := Heap[this,Buffer.full]+1;
                                      assert Isint32 (Heap[this,Buffer.full]);
                                      //give permission
                                      assert Permission[this, Buffer.value] >= 0.5;
                                      Permission[this,Buffer.value] := Permission[this,Buffer.value]-0.5;
                                      }
                                      goto Done;
                            fetch:
                                      assert Permission[this,Buffer.full]>0.0;
                                      if(0< Heap[this,Buffer.full])
                                      {
                                      Permission[this, Buffer.ovalue] := Permission[this,Buffer.ovalue]+1.0;
                                      havoc Heap_tmp;
                                      Heap[this,Buffer.ovalue] := Heap_tmp[this,Buffer.ovalue];
                                      // body fetch
                                      preHeap :=Heap;
                                      assert Permission[this,Buffer.ovalue] == 1.0 && Permission[this,Buffer.front]>0.0;
                                      assert ArrayPermission[Heap[this,Buffer.buf],Heap[this,Buffer.front]]>0.0;
                                      assert Isint32(Heap[this,Buffer.front]);
                                      Heap[this,Buffer.front] := (Heap[this,Buffer.front]+1) mod Buffer.size;
                                      assert Isint32(Heap[this,Buffer.front]);
                                      Heap[this,Buffer.front] := (Heap[this,Buffer.front]+1) mod Buffer.size;
                                      assert Isint32(Heap[this,Buffer.front]);
                                      assert Permission[this,Buffer.full] == 1.0;
                                      assert Isint32(Heap[this,Buffer.full]);
                                      Heap[this,Buffer.full]:= Heap[this,Buffer.full]-1;
                                      assert Isint32(Heap[this,Buffer.full]);
                                      assert Permission[this,Buffer.ovalue] == 1.0;
                                      Permission[this,Buffer.ovalue]:= Permission[this,Buffer.ovalue]-1.0;
                                      }
                            goto Done;
                        Done:
                       }
                     }
 """)
    
  }
}