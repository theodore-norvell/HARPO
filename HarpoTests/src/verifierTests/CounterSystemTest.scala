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
class CounterSystemTest extends VerifierTestBase {
  val out = new OutputStreamWriter(System.out)
  behavior of "The Boogie back end with Harpo 'Counter' class";

  it should "generate Boogie code for Counter class" in {
    val str = """
          //Counter Class in HARPO/L, A program that shows class invariant and a lock block

          (class Counter()

	          claim count@0.5
	          invariant canRead(count)
	          invariant count >_ 0
	            
              proc increment()
  	            takes count@0.5
  	            pre count>_0
  	            post count'>0
                gives count@0.5

              obj count: Int32 := 0

              (thread (*t0*)
                  (while true
                    do
                      (accept increment()
                        (with this
                           do
                             count := count+1;
                        with)
                       accept)
                   while)
              thread)
          class)

"""

    val BoogieSource = tryWithBoogieBackEnd(str)
    assert(BoogieSource === """
          //Prelude
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
            axiom min64 == -9223372036854775808;
            const unique max64:int;
            axiom max64 == 9223372036854775807;
            function Isint8(int) returns (bool);
            axiom (forall x:int :: Isint8(x) <==> min8 <=x&&x<=max8);
            function Isint16(int) returns (bool);
            axiom (forall x:int :: Isint16(x) <==> min16 <=x&&x<=max16);
            function Isint32(int) returns (bool);
            axiom (forall x:int :: Isint32(x) <==> min32 <=x&&x<=max32);
            function Isint64(int) returns (bool);
            axiom (forall x:int :: Isint64(x) <==> min64 <= x && x <= max64);

// Counter Translation
            type ClassName;
            function dtype(Ref) returns (ClassName);
            const unique Counter: ClassName;
//Claim
            var oldPermission: PermissionType;
            var Permission : PermissionType where (forall <x> r:Ref, f : Field x :: Permission[r, f] == 0.0);
            procedure Counter.constructor(this : Ref)
            modifies Heap, ArrayHeap, Permission;
            requires dtype(this) <: Counter;
            {
            //claim
            Permission[this, Counter.count] := 0.5;
            //invariant
            assert Permission[this,Counter.count] >= 0.0;
            //invariant
            assert Permission[this, Counter.count] >= 0.0 && Heap[this, Counter.count] >= 0;
            }
// Class Member
            const unique Counter.count : Field int;
            
// Thread *t0*
            procedure Counter.t0(this : Ref)
            modifies Heap, Permission, oldPermission;
            requires dtype(this) <: Counter;
            {
            var oldHeap,preHeap, Heap_temp: HeapType;
            oldHeap := Heap;
            havoc Heap;
            while (true)
            {
            goto increment;
            increment:

//takes
            oldPermission := Permission;
            if(Permission[this,Counter.count] == 0.0)
            {
            havoc Heap_temp;
            Heap[this,Counter.count] := Heap_temp[this,Counter.count];
            Permission[this, Counter.count] := Permission[this, Counter.count] + 0.5;	
            }

//pre
            oldHeap := Heap;
            havoc Heap;
            assume Heap[this, Counter.count] >= 0;
// body
            preHeap := Heap;
            assume Permission[this, Counter.count] > 0;
            assume Permission[this, Counter.count] > 0 && Heap[this,Counter.count] >= 0;

//assert thread has permission

            Heap[this,Counter.count] := Heap[this, Counter.count] + 1;
            assert Permission[this, Counter.count] > 0 && Heap[this,Counter.count] >= 0;

//post
            assert Permission[this, Counter.count] > 0.0;
            assert Heap[this, Counter.count] == preHeap[this, Counter.count] + 1;

//gives
            assert Permission[this, Counter.count] == oldPermission[this, Counter.count] + 0.5;
            Permission [this, Counter.count] := Permission[this, Counter.count] - 0.5;
            assert Permission[this,Counter.count] == oldPermission[this, Counter.count];

         goto done_increment;
         done_increment:
}
}
 """)
    val cmd = "Boogie " + BoogieSource + ".bpl" 
    val output = cmd.!!
  }
}