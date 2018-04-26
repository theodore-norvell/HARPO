package tests

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.BeforeAndAfterEach
import org.scalatest.TestData
import org.junit.runner.RunWith
import java.io.StringReader
import frontEnd.StandardErrorRecorder
import frontEnd.AST.FloatLiteralExpNd
import frontEnd.AST.IntLiteralExpNd
import checker.Checker
import checker.TypeChecker
import checker.TypeCreator
import checker.CheckerTypes
import checker.CheckerTypes.PrimitiveType
import parser.HarpoParser
import parser.ParseException
import parser.TokenMgrError

@RunWith( classOf[ JUnitRunner ] )
class LiteralTests extends TestsBase with BeforeAndAfterEach {
    val verbose = true;
    
    override def beforeEach( td: TestData ) {
    if ( verbose ) println( ">>>>>>>>>>>>>Starting "+td.name+" >>>>>>>>>>>>>>>>>>" )
    }

    override def afterEach( td: TestData ) {
        if ( verbose ) println( "<<<<<<<<<<<<<Finished "+td.name+" <<<<<<<<<<<<<<<<<" )
    }
    
    behavior of "The builder";
    // Int literals
    val intBuilderTestValues = List( "2#1101", "8#17", "100", "16#FF" )
    for ( value <- intBuilderTestValues ) {
        checkIntLiteral( value )
    }
    
    // Real literals
    val ints : List[String] = List( "2", "15", "264", "1243" )
    val fracs : List[String] = List( ".5", ".54", ".132", ".5433" )
    val exps : List[String] = List( "+0", "+2", "+5", "-6", "-1" )
    for ( exp <- exps ) {
        for ( value <- ints ) {
            val str1 = value + "e" + exp
            checkRealLiteral( str1, str1.toDouble )
           
            val str2 = value + ".e" + exp
            checkRealLiteral( str2, str2.toDouble )
            
            for ( frac <- fracs ) {
                val str3 = value + frac + "e" + exp
                checkRealLiteral( str3, str3.toDouble )
            }
        }
        
        for ( frac <- fracs ) {
            val str4 = frac + "e" + exp
            checkRealLiteral( str4, str4.toDouble )
        }
    }
    
    behavior of "the parser"
    
    // Int literals
    val validIL = List( "2#1101", "8#17", "23", "16#AF", "16#11", "2#11_01", "8#1_7", "2_3", "16#A_F" )
    
    for ( valid <- validIL ) {
        it should "accept " + valid + " as an integer literal form" in {
            val str = "obj intLit : Int16 := " + valid + ";";
            tryWithParser( str, 0, 0, true );
        }
    }
    
    val invalidIL = List( "2#2345", "8#slkdfj", "10#3242", "16#hijk" )
    for ( invalid <- invalidIL ) {
        it should "forbid " + invalid + " as an integer literal form" in {
            val str = "obj intLit : Int16 := " + invalid + ";";
            tryWithParser( str, 1, 0, true );
        }
    }
    
    // Real literals
    val validBeforeRL = List( ".1", "1.1", "1.", "1", ".1_1", "1_1.1_1", "1_1.", "1_1" )
    val validAfterRL = List( "+2", "-2", "2" )
    
    for ( before <- validBeforeRL ) {
        for ( after <- validAfterRL ) {
            it should "accept " + before + "e" + after + " as a real literal form" in {
                val str = "obj realLit : Real16 := " + before + "e" + after + ";";
                tryWithParser( str, 0, 0, true );
            }
            
            it should "accept " + before + "E" + after + " as a real literal form" in {
                val str = "obj realLit : Real16 := " + before + "E" + after + ";";
                tryWithParser( str, 0, 0, true );
            }
        }
    }
    
    val invalidBeforeRL = List( "8#17e+2", "2#1e+2" )
    val invalidAfterRL = List( "1e++2", "1e+a", "1e+1.2", "1e+2#1", "1e+8#17", "1e+16#b" )
    
    for ( before <- invalidBeforeRL ) {
        it should "forbid " + before + " as a real literal form" in {
            val str = "obj realLit := " + before + ";";
            
            val ter = tryWithParser( str, 1, 0, true );
        }
    }
    
    for ( after <- invalidAfterRL ) {
        it should "forbid " + after + " as a real literal form" in {
            val str = "obj realLit := " + after + ";";
            
            val ter = tryWithParser( str, 1, 0, true );
        }
    }
    
    behavior of "The type checker";
    
    // Int literals
    val small = List( "2#1111", "16#FF", "8#17", "0", "2_147_483_648" )
    val large = List( "2#11111111111111111111111111111111", "16#FFAFFFFF", "2_147_483_649", "8#101010101010" )
    for ( value <- small ) {
        checkIntLiteralType( value, CheckerTypes.int32 )
    }
    for ( value <- large ) {
        checkIntLiteralType( value, CheckerTypes.int64 )
    }
    
    val strStart = "obj intLit : "
    val strEndInt = " := 25;"
    acceptLiteralCoercion( strStart + "Int8" + strEndInt, CheckerTypes.int8, "Int" )
    acceptLiteralCoercion( strStart + "Int16" + strEndInt, CheckerTypes.int16, "Int" )
    acceptLiteralCoercion( strStart + "Int32" + strEndInt, CheckerTypes.int32, "Int" )
    acceptLiteralCoercion( strStart + "Real16" + strEndInt, CheckerTypes.real16, "Int" )
    acceptLiteralCoercion( strStart + "Real32" + strEndInt, CheckerTypes.real32, "Int" )
   
    forbidLiteralCoercion( strStart + "Int64" + strEndInt, CheckerTypes.int64, "Int" )
    forbidLiteralCoercion( strStart + "Real64" + strEndInt, CheckerTypes.real64, "Int" )
    forbidLiteralCoercion( strStart + "Bool" + strEndInt, CheckerTypes.bool, "Int" )
    
    val strEndReal = " := 1.5e+2;"
    acceptLiteralCoercion( strStart + "Real16" + strEndReal, CheckerTypes.real16, "Real" )
    acceptLiteralCoercion( strStart + "Real32" + strEndReal, CheckerTypes.real32, "Real" )
    
    forbidLiteralCoercion( strStart + "Int8" + strEndReal, CheckerTypes.int8, "Real" )
    forbidLiteralCoercion( strStart + "Int16" + strEndReal, CheckerTypes.int16, "Real" )
    forbidLiteralCoercion( strStart + "Int32" + strEndReal, CheckerTypes.int32, "Real" )
    forbidLiteralCoercion( strStart + "Int64" + strEndReal, CheckerTypes.int64, "Real" )
    forbidLiteralCoercion( strStart + "Bool" + strEndReal, CheckerTypes.bool, "Real" )
    
    // Real literals
    val realTypeTestValues : List[String] = List( "1e0", "1.532e5", "23.54352e-2", ".35423e3", "2353.e-1" )
    for ( value <- realTypeTestValues ) {
        checkRealLiteralType( value )
    }
    
    def checkIntLiteral( str: String ) {
        val errorRecorder = new StandardErrorRecorder()
        val builder = new frontEnd.Builder( errorRecorder )
        
        val coord = new frontEnd.AST.Coord( "testFile", 1, 10 )
        val base = if ( str.contains("#") ) str.split("#")(0).toInt else 10
        val value = if ( str.contains("#") ) str.split("#")(1) else str
        val node : IntLiteralExpNd = builder.intLiteralExp(base, value, coord)
        
        val expectedValue = Integer.parseInt( value, base )
        
        it should "accept " + str + " resulting in value " + expectedValue in {
            assertResult( expectedValue )( node.i )
            assertResult( 0 )( errorRecorder.getFatalCount() + errorRecorder.getWarningCount() )
        }
    }
    
    def checkIntLiteralType( str: String, expectedType : PrimitiveType ) {
        val errorRecorder = new StandardErrorRecorder()
        val builder = new frontEnd.Builder( errorRecorder )
        val typeCreator = new TypeCreator( errorRecorder )
        val typeChecker = new TypeChecker( errorRecorder, typeCreator )
        
        val coord = new frontEnd.AST.Coord( "testFile", 1, 10 )
        val base = if ( str.contains("#") ) str.split("#")(0).toInt else 10
        val value = if ( str.contains("#") ) str.split("#")(1) else str
        val node : IntLiteralExpNd = builder.intLiteralExp(base, value, coord)
        
        val tcResult = typeChecker.typeCheck( node )
        
        it should "return " + str + " as a " + expectedType in {
            tcResult match {
              case None => assert( false )
              case Some( tipe ) =>
                  assertResult( expectedType )( tipe )
                  assert( node.tipe.isDefined )
                  assertResult( 0 )( errorRecorder.getFatalCount() + errorRecorder.getWarningCount() )
            }
        }
    }
    
    def acceptLiteralCoercion( str: String, expectedType: PrimitiveType, typeName: String ) {
        val errorRecorder = new StandardErrorRecorder
        val reader = new StringReader( str )
        val p : HarpoParser = new HarpoParser( reader )
        val builder = new frontEnd.Builder( errorRecorder )
        val checker = new Checker( errorRecorder )

        p.setBuilder( builder )
        
        val dl : frontEnd.AST.DeclList = 
            try { p.Start().asInstanceOf[frontEnd.AST.DeclList] }
            catch { case ex : ParseException => {    
                        val coord = if( ex.currentToken != null ) frontEnd.AST.Coord( "fileName", ex.currentToken.beginLine, ex.currentToken.beginColumn ) 
                                    else frontEnd.AST.Coord( "fileName" ) ;
                        errorRecorder.reportFatal( ex.getMessage(), coord )
                        null }
                    case ex : TokenMgrError => {    
                        val coord = frontEnd.AST.Coord( "fileName" ) 
                        errorRecorder.reportFatal( ex.getMessage(), coord )
                        null }
                  }
            
        checker.runChecker( dl )
        
        val obj = dl.decls.head.asInstanceOf[frontEnd.AST.ObjDeclNd]
        val tcResult = obj.init.asInstanceOf[frontEnd.AST.ValueInitExpNd].exp.asInstanceOf[frontEnd.AST.ExpNd].tipe
        it should "accept a literal " + typeName + " giving " + expectedType in {
            tcResult match {
                case None => assert( false )
                case Some( tipe ) =>
                    assertResult( 0 )( errorRecorder.getFatalCount() + errorRecorder.getWarningCount() )
                    assertResult( expectedType )( tipe )
                    assert( tcResult.isDefined )
            }
        }
    }
    
    def forbidLiteralCoercion( str: String, badType: PrimitiveType, typeName: String )
    {
        val errorRecorder = new StandardErrorRecorder
        val reader = new StringReader( str )
        val p : HarpoParser = new HarpoParser( reader )
        val builder = new frontEnd.Builder( errorRecorder )
        val checker = new Checker( errorRecorder )
        p.setBuilder( builder )        
        
        val dl : frontEnd.AST.DeclList = 
            try { p.Start().asInstanceOf[frontEnd.AST.DeclList] }
            catch { case ex : ParseException => {    
                        val coord = if( ex.currentToken != null ) frontEnd.AST.Coord( "fileName", ex.currentToken.beginLine, ex.currentToken.beginColumn ) 
                                    else frontEnd.AST.Coord( "fileName" ) ;
                        errorRecorder.reportFatal( ex.getMessage(), coord )
                        null }
                    case ex : TokenMgrError => {    
                        val coord = frontEnd.AST.Coord( "fileName" ) 
                        errorRecorder.reportFatal( ex.getMessage(), coord )
                        null }
            }
            
        // We need to stop the tests from terminating on a
        // BailOutException.  The checker throws a BailOutException
        // when there is a fatal error at any point during the
        // runChecker process.
        try { checker.runChecker( dl ) }
        catch { case ex : frontEnd.CompilerBailOutException => {}}

        it should "forbid a literal " + typeName + " being coerced to a " + badType in {
            assertResult( 1 )( errorRecorder.getFatalCount() )
            assertResult( 0 )( errorRecorder.getWarningCount() )
        }
    }
    
    def checkRealLiteral( str: String, expectedValue: Double ) {
        val errorRecorder = new StandardErrorRecorder()
        val builder = new frontEnd.Builder( errorRecorder )
            
        val coord = new frontEnd.AST.Coord( "testFile", 1, 10 )
        val node : FloatLiteralExpNd = builder.realLiteralExp( str, coord )
        
        it should "accept " + str + " resulting in value " + expectedValue in {            
            assert( Math.abs(expectedValue - node.x) < 0.0000001 )
            assertResult( 0 )( errorRecorder.getFatalCount() + errorRecorder.getWarningCount() )
            
        }
    }
    
    def checkRealLiteralType( str : String, expectedType : PrimitiveType = CheckerTypes.real64 ) {        
        val errorRecorder = new StandardErrorRecorder()
        val builder = new frontEnd.Builder( errorRecorder )
        val typeCreator = new TypeCreator( errorRecorder )
        val typeChecker = new TypeChecker( errorRecorder, typeCreator )
        
        val coord = new frontEnd.AST.Coord( "testFile", 1, 10 )
        val node : FloatLiteralExpNd = builder.realLiteralExp( str, coord )
        
        val tcResult = typeChecker.typeCheck( node )
        
        it should "return " + str + " as a " + expectedType in {
            tcResult match {
                case None => assert( false )
                case Some( tipe ) =>
                    assertResult( expectedType )( tipe )
                    assert( node.tipe.isDefined )
                    assertResult( 0 )( errorRecorder.getFatalCount() + errorRecorder.getWarningCount() )
            }
        }
    }
}