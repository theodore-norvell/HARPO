/* 
 * These tests contains Permission Transfer Scenario 2
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
class Scenario2Test extends VerifierTestBase {
  
  val fileName = getClass.getName
  behavior of "The Boogie back end with Harpo 'Scenario 2' class";
  it should "generate Boogie code for 'Scenario2' class" in {
    
    val str = """
       //Permission Transder Scenario 2
      (class C()

        public proc main()
        // receives the permission on field a and stores the amount of gp1
        // gives read permission to t2

        public proc worker1_start(ghost in p1: Real64)
          takes a@p1
          pre 0 < p1 /\ p1 < 1.0
          post gp1' = p1
          post gp2' = p1/2
          gives gp1@0.5 , gp2@0.5
        // returns read permission to caller
        // the retuned permission is the half of the permission in 
        // received via worker1_start

      public proc worker1_finish(ghost in p1_init : Real64, out po1: Real64)
        takes gp1@0.5
        pre p1_init = gp1
        post po1' = p1_init/2
        gives a@po1

        //receives read permission on field a and stores the amount in gp2

        public proc worker2_start(ghost in p2:Real64)
          takes a@p2
          pre 0<p2 and p2<1.0
          post gp2'= p2
          gives gp2@0.5

        // return read permission to caller
        // the returned permission is equal to the permission it received // via worker2_start

        public proc worker2_finish(ghost in p2_init: Real64, ghost out po2: Real64)
          takes gp2@0.5
          pre p2_init = gp2
          post po2' = p2_init
          gives a@po2

        public obj a: Bool := false
        public ghost obj gp1 : Real64 := 0 // Can not declare multiple objects
        public ghost obj gp2 : Real64 := 0

        (thread (*t0_client*) claim a@1.0
	        (while true
          do
		        (accept main()
			        ghost obj p : Real64 :=0
			        ghost obj pc1: Real64 :=0
              ghost obj pr1 : Real64 :=0
              ghost obj pr2 : Real64 :=0
			        p:=1
			        pc1 := p/2
			        p := p-pc1
			        worker1_start(pc1)
			        worker1_finish(pc1,pr1)
			        p := p+pr1;
			        worker2_finish(gp2,pr2)
			        p :=p+pr2
			        assert p = 1
		        accept)
	        while)
        thread)


        (thread (*t1_server1*) claim gp1@1.0
	        (while true
          do
		        ghost obj pc2 : Real64 := 0;
		        (accept worker1_start(ghost in p1: Real64)
			        gp1 := p1
			        pc2 :=gp1/2
			        worker2_start(pc2)
		        accept)
		        (accept worker1_finish(ghost in p1_init : Real64, ghost out po1: Real64)
		        accept)
	        while)
        thread)

        (thread (*t2_server2*) claim gp2@1
	        (while true
          do
		        (accept worker2_start (ghost in p2: Real64)
			        gp2 := p2
		        accept)
		        (accept worker2_finish(ghost in p2_init: Real64, ghost out po2 : Real64)
			        po2 := p2_init
		        accept)
	        while)
        thread)
        class) """

        translateAndVerify(fileName,str)

}
}
