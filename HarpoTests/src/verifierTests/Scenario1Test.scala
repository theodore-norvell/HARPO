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
class Scenario1Test extends VerifierTestBase {

  behavior of "The Boogie back end with Harpo 'Scenario 1' class";
  it should "generate Boogie code for Scenario 1 " in {

    var str = """ //Permission Transfer Scenario 1

                  (class Scenario1()
	
	                  public obj a : Bool := false;	
	                  public ghost obj gp1 : Real64 := 0;
	                  public ghost obj gp2 : Real64 := 0;
	
	                  public proc main()
	
	                  // receives read permission on a and stores the amount of in gp1
	
	                  public proc worker1_start(ghost in p1: Real64)
	                    takes a@p1
	                    pre 0 < p1 /\ p1 < 1.0
	                    post gp1' = p1
	                    gives gp1@0.5

	                  // return read permission equal to received permission via worker1_start
                    public proc worker1_finish(ghost in p1_init : Real64)
                      takes gp1@0.5
                      pre p1_init = gp1
                      gives a@p1_init

	                  //returns read permission equal to recieved permision via worker1_start

	                  public proc worker2_start(ghost in p2 : Real64)
	                    takes a@p2
	                    pre 0 < p2 /\ p2 < 1.0
                      post gp2' = p2
	                    gives gp2@0.5
                    
                    public proc worker2_finish(ghost in p2_init : Real64)
                      takes gp2@0.5
                      pre p2_init = gp2
                      gives a@p2_init

	                  //t0 has full permission on field a
	                  //gives read permission to t1 and finally receives back it from t1

	                  (thread (*t0_client*) claim a@1.0
		                  (while true
			                  invariant canWrite(a)
			                  do
			                  (accept main()
				                  ghost obj pc1 : Real64 :=0.5
				                  worker1_start(pc1)
				                  worker1_finish(pc1)
			                  accept)
		                  while)
	                  thread)

	                  (thread (*t1_server1*) claim gp1@1.0
		                  (while true
			                  invariant canWrite(gp1)
			                  do
			                  (accept worker1_start(ghost in p1: Real64)
				                  gp1 := p1
			                  accept)
			                  ghost obj pc2 : Real64 := gp1/2
			                  worker2_start(pc2)
			                  worker2_finish(pc2)
			                  (accept worker1_finish(ghost in p1_init : Real64)
			                  accept)
		                  while)
	                  thread)

	                  (thread (*t2_server2*) claim gp2@1.0
		                  (while true
		                    invariant canWrite(gp2)
		                    do
		                    (accept worker2_start(ghost in p2 : Real64)
			                    gp2 :=p2
		                    accept)	
		                    (accept worker2_finish(ghost in p2_init :Real64)
		                    accept)
		                  while)
	                  thread)
                  class) """

    var BoogieSource = tryWithBoogieBackEnd(str)

    println(BoogieSource)

  }
}
