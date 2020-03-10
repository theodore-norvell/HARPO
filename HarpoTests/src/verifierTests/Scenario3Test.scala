/*
 * These tests contains Permission Transfer Scenario 3
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
class Scenario3Test extends VerifierTestBase {
  
  val fileName = getClass.getName
  behavior of "The Boogie back end with Harpo 'Scenario 3' class";
  it should "generate Boogie code for Scenario 3 " in {

    val str = """ //Permission Transfer Scenario 3

        (class C()
        	
        	public proc main()
        
        	//receives read permission on field a and stores the amount 	// in gp1
        
        	public proc worker1_start(ghost in p1 : Real64)
        	takes a@p1
        	pre 0<p1 and p1<1.0
        	post gp1'=p1
        	gives gp1@0.5
        
        	// returns read permission equal to received permission via  	// worker1_start
        
        	public proc worker1_finish(ghost in p1_init : Real64)
        	takes gp1@0.5
        	pre p1_init = gp1
        	gives a@p1_init
        	
        	// receives read permission on field a and stores the amount 	// in gp2
        
        	public proc worker2_start(ghost in p2: Real64)
        	takes a@p2
        	pre 0<p2 /\ p2<1.0
        	post gp2' = p2
        	gives gp2@0.5
        
        	// receives read permission on gp1 and gp2
        	// which have the received permissions of t1 and t2
        	// returns the sum of its received read permission and the 	
        // read permission of t1
        
        public proc worker2_finish(ghost in p1_init: Real64, ghost in p2_init : Real64, ghost out po: Real64)
        takes gp1@0.5, gp2@0.5
        pre p2_init = gp2
        pre p1_init = gp1
        post po' = p1_init + p2_init
        gives a@po
        
        public obj a : Bool :=0
        public ghost obj gp1 : Real64 := 0.0
        public ghost obj gp2 : Real64 := 0.0
        
        (thread (*t0_client*) claim a@1.0
        	(while true
        		invariant acc a@1.0
          do
        		(accept main()
        			ghost obj p : Real64 := 1.0
        			ghost obj pc1 :Real64 := 0.0
              ghost obj pc2 :Real64 := 0.0
              ghost obj po :Real64 := 0.0
        			pc1 := p/2
        			p := p-pc1
        			pc2 := p/2  			
        			worker1_start(pc1)
        			worker2_start(pc2)
        			worker2_finish(pc1,pc2,po)
        			p := p+po
        		accept)
        	while)
        thread)
        
        
        (thread (*t1_server1*) claim gp1@1.0
        	(while true
        		invariant acc gp1@1.0
           do
        		(accept worker1_start (ghost in p1: Real64)
        			gp1 := p1
        		accept)
        		(accept worker1_finish (ghost in p1_init : Real64)
        		accept)
        	while)
        thread)
        
        (thread (*t2_server2*) claim gp2@1.0
        	(while true
        		invariant acc gp2@1.0
          do
        		(accept worker2_start(ghost in p2: Real64)
        			gp2:=p2
        		accept)
        		(accept worker2_finish ( ghost in p1_init :Real64, ghost in p2_init : Real64, ghost out po: Real64)
        		worker1_finish(p1_init)
        		po := p1_init + p2_init
        		accept)
        	while)
        thread)
        class) 
                
 """
   translateAndVerify(fileName,str)

  }
}
