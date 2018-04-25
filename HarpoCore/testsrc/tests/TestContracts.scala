package tests

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import contracts.Contracts

@RunWith(classOf[JUnitRunner]) 
class CheckerTests extends FlatSpec with Contracts { 
    
  "preconditions and postconditions" should "do nothing when true 0" in {
      val r = pre (true) post ((r : Int) => true) in { 2+2 }
      assert( r == 4 ) 
      
  }
  
  "preconditions and postconditions" should "do nothing when true 1" in {
      val r = pre (
                  true
              ) post (
                  (r : Int) => true
              ) in (
                  2+2 )
      assert( r == 4 ) 
  }
  
  "preconditions and postconditions" should "do nothing when true 2" in {
      val r = pre {
                  true
              } post {
                  (r : Int) => true
              } in {
                  2+2 }
      assert( r == 4 ) 
  }
  
  "preconditions and postconditions" should "do nothing when true 3" in {
      val r = (pre {
                  true }
               post {
                  (r : Int) => true }
               in {
                  2+2 } )
      assert( r == 4 )
  }
  
  "preconditions and postconditions" should "do nothing when true 4" in {
      val r = pre( true )
              .pre( true ) 
    		  .post ( (r : Int) => true )
    		  .post ( (r : Int) => true )
    		  .in (
                  2+2 )
      assert( r == 4 ) }
  
  "preconditions" should "throw when precond is false 0" in {
      def squareRoot( i : Int ) =
          pre( i >= 0 ) post( (r:Int) => r*r <= i && i < (r+1)*(r+1) ) in {
              2+2 
          } 
      try { squareRoot(-1) ; assert(false)  }
      catch{ case e : AssertionError => ()
             case e : Any => assert(false)
      }
  }
  
  "preconditions" should "throw when precond is false 1" in {
      var x : Int = 1 ;
      def incrX( i : Int ) {
          val x0 = x 
          pre( i >= 0 ) post( x == x0+i ) in { x += i } }
      try { val r = incrX(-1) ; assert(false)  }
      catch{ case e : AssertionError => ()
             case e : Any => print(e) ; assert(false)
      } }
  
  "postconditions" should "throw when postcond is false" in {
      def squareRoot( i : Int ) =
          pre( i >= 0 ) post
          ( (r:Int) => r*r <= i && i < (r+1)*(r+1) ) in
          {
              2+2 
          } 
      try { squareRoot(10) ; assert(false)  }
      catch{ case e : AssertionError => ()
             case e : Any => assert(false)
      }
  }
  
  "postconditions" should "not throw when postcond is true 0" in {
      def squareRoot( i : Int ) = (
          pre( i >= 0 )
          post( (r:Int) => r*r <= i && i < (r+1)*(r+1) )
          in {
              2+2 
          } )
      try { val r = squareRoot(17) ; assert(r==4)  }
      catch{ case e : AssertionError => assert(false) 
             case e : Any => assert(false)
      }
  }
  
  "postconditions" should "not throw when postcond is true 1" in {
      var x : Int = 1 ;
      def incrX( i : Int ) {
          val x0 = x 
          pre( i >= 0
          ) post( x == x0+i
          ) in {
              x += i 
          } }
      try { val r = incrX(17) ; assert(x==18)  }
      catch{ case e : AssertionError => assert(false) 
             case e : Any => print(e) ; assert(false)
      }
  }
  
  "pre and postconditions" should "be chainable 0" in {
      def squareRoot( i : Int ) = (
          pre( i >= 0 )
          pre( i < 100 )
          post( (r:Int) => r*r <= i)
          post( (r:Int) => i < (r+1)*(r+1) )
          in {
              2+2 
          } )
      try { val r = squareRoot(17) ; assert(r==4)  }
      catch{ case e : AssertionError => assert(false) 
             case e : Any => assert(false)
      }
  }
  
  "pre and postconditions" should "be chainable 1" in {
      var x : Int = 1 ;
      def incrX( i : Int ) {
          val x0 = x 
          pre( i >= 0
          ) pre( i < 100
          ) post( x == x0+i
          ) post( x-i == x0
          ) in {
              x += i 
          } }
      try { val r = incrX(17) ; assert(x==18)  }
      catch{ case e : AssertionError => assert(false) 
             case e : Any => print(e) ; assert(false)
      }
  }
}