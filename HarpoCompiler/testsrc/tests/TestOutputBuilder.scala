package tests

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

import frontEnd.AST.Coord 
import util.OutputBuilder

@RunWith(classOf[JUnitRunner]) 
class TestOutputBuilder  extends FlatSpec {
    
  "putting a single line" should "put the line" in {
      val out = new OutputBuilder
      out.put( "abc" ) 
      out.newLine
      var result = ""
      var lines = 0 
      for( line <- out.result() ) { result += line + "\n" ; lines += 1 }
      assertResult( 1 )( lines )
      assertResult( "abc\n")( result )
  }
    
  "embedded EOL sequences" should "be treated as newlines" in {
      val out = new OutputBuilder
      out.put( "abc\r\ndef" ) 
      out.put( "\n\n\r\rxyz\r\n" ) 
      var result = ""
      var lines = 0 
      for( line <- out.result() ) { result += line + "\n" ; lines += 1 }
      assertResult( 6 )( lines )
      assertResult( "abc\ndef\n\n\n\nxyz\n")( result )
  }
    
  "indenting" should "work" in {
      val out = new OutputBuilder
      out.put( "abc" ) 
      out.newLine
      out.indent
      out.put( "def" ) 
      out.newLine
      out.indent
      out.put( "ghi" ) 
      out.newLine
      out.put( "jkl" )
      out.newLine
      out.dedent 
      out.put( "mno" )
      out.newLine
      out.dedent 
      out.put( "pqr" )
      out.newLine 
      var result = ""
      for( line <- out.result() ) result += line + "\n" 
      assertResult(
"""abc
    def
        ghi
        jkl
    mno
pqr
""" ) ( result )
    }
    
    private val coord1 = new Coord( "Fred", 1, 1 )
    private val coord2 = new Coord( "Fred", 2, 1 )
    private val coord3 = new Coord( "Fred", 3, 1 )
    private val coord4 = new Coord( "Fred", 4, 1 )
    
    private val error1 = "Error 1 "
    private val error2 = "Error 2 "
    private val error3 = "Error 3 "
    private val error4 = "Error 4 "
    
    
    "recording errors" should "work 0" in {
      val out = new OutputBuilder
      out.setError( error1, coord1 )
      out.put( "abc" )
      out.newLine
      out.setError( error2, coord2 )
      out.put( "def" )
      out.newLine
      out.setError( error3, coord3 )
      out.indent
      out.put( "def" ) 
      out.newLine
      
      assertResult( None )( out.getError( -1 ) ) 
      assertResult( None )( out.getError( 0 ) ) 
      assertResult( Some(error1, coord1) )( out.getError( 1 ) )
      assertResult( Some(error2, coord2) )( out.getError( 2 ) )
      assertResult( Some(error3, coord3) )( out.getError( 3 ) ) 
      assertResult( None )( out.getError(4) ) 
      assertResult( None )( out.getError(5) ) 
    }
    
    "recording errors" should "work 1" in {
      val out = new OutputBuilder
      out.put( "abc" )
      out.setError( error1, coord1 )
      out.newLine
      out.put( "def" )
      out.setError( error2, coord2 )
      out.newLine
      out.indent
      out.put( "def" ) 
      out.setError( error3, coord3 )
      out.newLine
      
      assertResult( None )( out.getError( -1 ) ) 
      assertResult( None )( out.getError( 0 ) ) 
      assertResult( Some(error1, coord1) )( out.getError( 1 ) )
      assertResult( Some(error2, coord2) )( out.getError( 2 ) )
      assertResult( Some(error3, coord3) )( out.getError( 3 ) ) 
      assertResult( None )( out.getError(4) ) 
      assertResult( None )( out.getError(5) ) 
    }
    
    "recording errors" should "work 2" in {
      val out = new OutputBuilder
      out.setError( error1, coord1 )
      out.put( "abc" )
      out.newLine
      out.clearError
      out.put( "def" )
      out.newLine
      out.setError( error3, coord3 )
      out.indent
      out.put( "def" ) 
      out.newLine
      
      assertResult( None )( out.getError( -1 ) ) 
      assertResult( None )( out.getError( 0 ) ) 
      assertResult( Some(error1, coord1) )( out.getError( 1 ) )
      assertResult( None )( out.getError( 2 ) )
      assertResult( Some(error3, coord3) )( out.getError( 3 ) ) 
      assertResult( None )( out.getError(4) ) 
      assertResult( None )( out.getError(5) ) 
    }
    
    "recording errors" should "persist" in {
      val out = new OutputBuilder
      out.put( "abc" )
      out.setError( error1, coord1 )
      out.newLine //1
      out.put( "def" )
      out.clearError
      out.newLine //2
      out.indent
      out.put( "ghi" ) 
      out.newLine //3
      out.put( "jkl" )
      out.setError( error3, coord3 )
      out.newLine //4
      out.put( "mno" )
      out.newLine //5
      
      assertResult( None )( out.getError( -1 ) ) 
      assertResult( None )( out.getError( 0 ) ) 
      assertResult( Some(error1, coord1) )( out.getError( 1 ) )
      assertResult( None )( out.getError( 2 ) )
      assertResult( None )( out.getError( 3 ) ) 
      assertResult( Some(error3, coord3) )( out.getError(4) ) 
      assertResult( Some(error3, coord3) )( out.getError(5) ) 
      assertResult( None )( out.getError( 6 ) ) 
    }
    
    "recording errors" should "not fail if we clear and then set error" in {
      val out = new OutputBuilder
      out.put( "123" )
      out.newLine   // Line 1. No error.
      out.put( "abc" ) 
      out.setError( error1, coord1 )
      out.newLine  // Line 2, error 1
      out.clearError
      out.put( "def" )
      out.setError( error2, coord2 )
      out.newLine  // Line 3 error 2
      out.setError( error2, coord2 )
      out.newLine  // Line 4 error 2
      
      assertResult( None )( out.getError( -1 ) ) 
      assertResult( None )( out.getError( 0 ) ) 
      assertResult( None )( out.getError( 1 ) ) 
      assertResult( Some(error1, coord1) )( out.getError( 2 ) )
      assertResult( Some(error2, coord2 )) ( out.getError( 3 ) )
      assertResult( Some(error2, coord2) )( out.getError( 4 ) ) 
      assertResult( None )( out.getError(5) ) 
      assertResult( None )( out.getError(6) ) 
    }
    
    "recording errors" should "fail if we set and then clear error" in {
      val out = new OutputBuilder
      out.put( "abc" )
      out.setError( error1, coord1 )
      out.newLine
      out.setError( error2, coord2 )
      out.put( "def" )
      try { out.clearError ; assert( false ) }
      catch { case e : Throwable =>
          assert( e.isInstanceOf[AssertionError]  )
      }
    }
    
    "recording errors" should "fail if we overwrite an error" in {
      val out = new OutputBuilder
      out.put( "abc" )
      out.setError( error1, coord1 )
      out.newLine
      out.setError( error2, coord2 )
      out.put( "def" )
      try { out.setError( error3, coord3) ; assert( false ) }
      catch { case e : Throwable =>
          assert( e.isInstanceOf[AssertionError] )
      }
    }
    
    "OutputBuilder" should "allow backpatching 0" in {
      val out = new OutputBuilder
   
      val p0 = out.saveLine()
      out.indent ; out.indent
      out.put( "abc\n" )
      out.dedent ; out.dedent
      out.put( "uvw\n" )
      out.goToAfter( p0 ) 
      out.put( "def\n" )
      out.goToEnd() 
      out.put( "xyz\n" ) 
      var result = "" 
      for( line <- out.result() ) result += line + "\n" 
      assertResult(
"""        abc
        def
uvw
xyz
""" ) ( result )  
    }
    
    "OutputBuilder" should "allow backpatching 1" in {
      val out = new OutputBuilder
   
      val p0 = out.saveLine()
      out.indent
      out.indent
      out.put( "abc\n" )
      out.dedent
      out.dedent
      out.put( "uvw\n" )
      val p1 : out.Line = out.saveLine()
      out.goToBefore( p0 )
      out.put( "def\n" )
      out.goToAfter( p1 ) 
      out.put( "xyz\n" ) 
      var result = "" 
      for( line <- out.result() ) result += line + "\n" 
      assertResult(
"""def
        abc
uvw
xyz
""" ) ( result )    
    }
}
