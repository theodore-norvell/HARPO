package tests

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.TestData
import org.scalatest.BeforeAndAfterEach
import java.io._
import org.junit.runner.RunWith
import java.io.OutputStreamWriter
import scala.collection.mutable.ArrayBuffer
import parser.HarpoParser
import frontEnd.ErrorRecorder
import frontEnd.AST
import frontEnd.AST._
import frontEnd.FQN
import checker.Checker
import checker.CheckerTypes
import frontEnd.StandardErrorRecorder
import checker.TypeChecker
import checker.TypeCreator

@RunWith( classOf[ JUnitRunner ] )
class TypeCheckerUnitTests extends FlatSpec with BeforeAndAfterEach {
    val verbose = true

    override def beforeEach( td: TestData ) {
        if ( verbose ) println( ">>>>>>>>>>>>>Starting "+td.name+" >>>>>>>>>>>>>>>>>>" )
    }

    override def afterEach( td: TestData ) {
        if ( verbose ) println( "<<<<<<<<<<<<<Finished "+td.name+" <<<<<<<<<<<<<<<<<" )
    }
    
    val coordOp = new AST.Coord( "testfile", 1, 20 )
    val coordX = new AST.Coord( "testfile", 1, 10 )
    val coordY = new AST.Coord( "testfile", 1, 30 )

    val int8Node = makeNamedTypeNode( "Int8", CheckerTypes.int8 )
    val int16Node = makeNamedTypeNode( "Int16", CheckerTypes.int16 )
    val int32Node = makeNamedTypeNode("Int32", CheckerTypes.int32)
    val int64Node = makeNamedTypeNode("Int64", CheckerTypes.int64)
    val real16Node = makeNamedTypeNode("Real16", CheckerTypes.real16)
    val real32Node = makeNamedTypeNode("Real32", CheckerTypes.real32)
    val real64Node = makeNamedTypeNode("Real64", CheckerTypes.real64)
    val boolNode = makeNamedTypeNode( "Bool", CheckerTypes.bool )
    
    val realTypes : List[NamedTypeNd] = List(real16Node, real32Node, real64Node)
    val intTypes : List[NamedTypeNd] = List(int8Node, int16Node, int32Node, int64Node)
    
    /* Operators sorted into two lists based on whether
     * they are used with Bools or with arithmetic primitives.
     * Each element is a list containing the operator and a
     * string representation of the operator to make the
     * test outputs more readable.
     */
    val boolOps = List(
                      List(ImpliesOp, "=>"),
                      List(EquivOp, "<=>"),
                      List(OrOp, "\\/"),
                      List(AndOp, "/\\"))
    val compOps = List(
                      List(EqualOp, "="),
                      List(NotEqualOp, "~="),
                      List(LessOp, "<"),
                      List(LessOrEqualOp, "_<"),
                      List(GreaterOp, ">"),
                      List(GreaterOrEqualOp, ">_"))   
    val arithOps = List(
                      List(AddOp, "+"),
                      List(SubOp, "-"),
                      List(MulOp, "*"))
    val intDivOps = List(
                      List(WordDivOp, "div"),
                      List(RemOp, "mod"))                   
    val allBinaryOps = (boolOps ::: arithOps ::: intDivOps) :+ List(SlashDivOp, "/")                  
               
    val unaryOps = List(
                      List(NegativeOp, "-"),
                      List(NotOp, "~"))
                      
    behavior of "The type checker";
    
    // Positive Tests
    
    // Bool-Bool tests with all boolean operators.
    for (op <- boolOps) {
        it should "allow Bool " + op(1) + " Bool giving Bool" in {
            val x = makeOperandNode( boolNode, "x", coordX )
            val y = makeOperandNode( boolNode, "y", coordY )
            val expectedType = CheckerTypes.bool
            allow( op(0).asInstanceOf[BinaryOperator], x, y, expectedType )
        }
    }
    
    //Arithmetic-Arithmetic tests with all comparative operators.
    val allowedCompTable = List(int8Node, int16Node, int32Node, real16Node, real32Node, real64Node)
    for (op <- compOps) {
        val expectedType = CheckerTypes.bool
        for (type1 <- allowedCompTable) {
            for (type2 <- allowedCompTable) {
                it should "allow " + type1.toString().substring(2) + " " + op(1) + " " + type2.toString().substring(2) + " giving Bool" in {
                    val x = makeOperandNode( type1, "x", coordX )
                    val y = makeOperandNode( type2, "y", coordY )
                    allow( List( op(0).asInstanceOf[ChainingOperator] ), List( x, y ) )
                }
            }
        }
    }
    
    // Do a couple tests on bigger chained expressions.
    val coordW = new AST.Coord( "testfile", 1, 50 )
    val coordZ = new AST.Coord( "testfile", 1, 70 )
    
    val chainW = makeOperandNode( real16Node, "w", coordW )
    val chainX = makeOperandNode( int8Node, "x", coordX )
    val chainY = makeOperandNode( int16Node, "y", coordY )
    val chainZ = makeOperandNode( real64Node, "z", coordZ )
    
    val ops = List( compOps(0)(0).asInstanceOf[ChainingOperator],
                    compOps(1)(0).asInstanceOf[ChainingOperator] )
    val opers = List( chainW, chainX, chainY )
    
    it should "accept 3 chained operands for comparison giving Bool" in {
        allow( ops, opers )
    }
    
    it should "accept 4 chained operands for comparison giving Bool" in {
        allow( ops :+ compOps(2)(0).asInstanceOf[ChainingOperator], opers :+ chainZ )
    }
    
    /* All valid type combinations and the expected result.
     * Since this only includes valid combinations, any
     * combination where the expected result is "None" is
     * excluded and will be tested later in the suite.
     */
    val allowedArithTable : List[List[Object]] =
        List(
            // Int-Int Combinations
            List( int8Node,   int8Node,   CheckerTypes.int8   ), // 0
            List( int8Node,   int16Node,  CheckerTypes.int16  ),
            List( int8Node,   int32Node,  CheckerTypes.int32  ),
            List( int8Node,   int64Node,  CheckerTypes.int64  ),
            List( int16Node,  int16Node,  CheckerTypes.int16  ),
            List( int16Node,  int32Node,  CheckerTypes.int32  ),
            List( int16Node,  int64Node,  CheckerTypes.int64  ),
            List( int32Node,  int32Node,  CheckerTypes.int32  ),
            List( int32Node,  int64Node,  CheckerTypes.int64  ),
            List( int64Node,  int64Node,  CheckerTypes.int64  ), // 9
            // Int-Real Combinations
            List( int8Node,   real16Node, CheckerTypes.real16 ), // 10
            List( int8Node,   real32Node, CheckerTypes.real32 ),
            List( int8Node,   real64Node, CheckerTypes.real64 ),
            List( int16Node,  real16Node, CheckerTypes.real32 ),
            List( int16Node,  real32Node, CheckerTypes.real32 ),
            List( int16Node,  real64Node, CheckerTypes.real64 ),
            List( int32Node,  real16Node, CheckerTypes.real64 ),
            List( int32Node,  real32Node, CheckerTypes.real64 ),
            List( int32Node,  real64Node, CheckerTypes.real64 ), // 18
            // Real-Real Combinations
            List( real16Node, real16Node, CheckerTypes.real16 ), // 19
            List( real16Node, real32Node, CheckerTypes.real32 ),
            List( real16Node, real64Node, CheckerTypes.real64 ),
            List( real32Node, real32Node, CheckerTypes.real32 ),
            List( real32Node, real64Node, CheckerTypes.real64 ),
            List( real64Node, real64Node, CheckerTypes.real64 )) // 24
    
    /*  Loop through all valid pairs of types with every valid operator.
     *  Special cases handled:
     *    - If 2 of the same type is used, only one test is needed
     *      since swapping the types gives the same expression.
     */
    for (allowed <- allowedArithTable)
    {
        for (op <- arithOps)
        {
            val type1 = allowed(0).asInstanceOf[NamedTypeNd]
            val type2 = allowed(1).asInstanceOf[NamedTypeNd]
            val expectedType = allowed(2).asInstanceOf[CheckerTypes.PrimitiveType]
            
            it should "allow " + type1.toString().substring(2) + " " + op(1) + " " + type2.toString().substring(2) + " giving " + expectedType.toString().substring(2) in {
                val x = makeOperandNode( type1, "x", coordX )
                val y = makeOperandNode( type2, "y", coordY )
                allow( op(0).asInstanceOf[BinaryOperator], x, y, expectedType )
            }
                
            if (!type1.equals(type2))
            {
                it should "allow " + type2.toString().substring(2) + " " + op(1) + " " + type1.toString().substring(2) + " giving " + expectedType.toString().substring(2) in {
                    val x = makeOperandNode( type2, "x", coordX )
                    val y = makeOperandNode( type1, "y", coordY )
                    allow( op(0).asInstanceOf[BinaryOperator], x, y, expectedType )
                }
            }
        }
    }
    
    // Arithmetic-Arithmetic tests with / operator.
    val allowedSlashDivTable : List[List[Object]] =
        List(
            List( int8Node,   int8Node,   CheckerTypes.real16 ),
            List( int8Node,   int16Node,  CheckerTypes.real32 ),
            List( int8Node,   int32Node,  CheckerTypes.real64 ),
            List( int8Node,   real16Node, CheckerTypes.real16 ),
            List( int8Node,   real32Node, CheckerTypes.real32 ),
            List( int8Node,   real64Node, CheckerTypes.real64 ),
            List( int16Node,  int16Node,  CheckerTypes.real32 ),
            List( int16Node,  int32Node,  CheckerTypes.real64 ),
            List( int16Node,  real16Node, CheckerTypes.real32 ),
            List( int16Node,  real32Node, CheckerTypes.real32 ),
            List( int16Node,  real64Node, CheckerTypes.real64 ),
            List( int32Node,  int32Node,  CheckerTypes.real64 ),
            List( int32Node,  real16Node, CheckerTypes.real64 ),
            List( int32Node,  real32Node, CheckerTypes.real64 ),
            List( int32Node,  real64Node, CheckerTypes.real64 ),
            List( real16Node, real16Node, CheckerTypes.real16 ),
            List( real16Node, real32Node, CheckerTypes.real32 ),
            List( real16Node, real64Node, CheckerTypes.real64 ),
            List( real32Node, real32Node, CheckerTypes.real32 ),
            List( real32Node, real64Node, CheckerTypes.real64 ),
            List( real64Node, real64Node, CheckerTypes.real64 ))
            
    for (allowed <- allowedSlashDivTable)
    {
        val type1 = allowed(0).asInstanceOf[NamedTypeNd]
        val type2 = allowed(1).asInstanceOf[NamedTypeNd]
        val expectedType = allowed(2).asInstanceOf[CheckerTypes.PrimitiveType];
        
        it should "allow " + type1.toString().substring(2) + " / " + type2.toString().substring(2) + " giving " + expectedType.toString().substring(2) in {
            val x = makeOperandNode( type1, "x", coordX )
            val y = makeOperandNode( type2, "y", coordY )
            allow( SlashDivOp, x, y, expectedType )
        }
        
        if (!type1.equals(type2)) {
            it should "allow " + type2.toString().substring(2) + " / " + type1.toString().substring(2) + " giving " + expectedType.toString().substring(2) in {
                val x = makeOperandNode( type2, "x", coordX )
                val y = makeOperandNode( type1, "y", coordY )
                allow( SlashDivOp, x, y, expectedType )
            }
        }
    }
            
    // Integer-Integer tests with div and mod operators.
    val allowedIntDivTable : List[List[Object]] =
        List(
            List(int8Node,  int8Node,  CheckerTypes.int8),
            List(int8Node,  int16Node, CheckerTypes.int16),
            List(int8Node,  int32Node, CheckerTypes.int32),
            List(int8Node,  int64Node, CheckerTypes.int64),
            List(int16Node, int16Node, CheckerTypes.int16),
            List(int16Node, int32Node, CheckerTypes.int32),
            List(int16Node, int64Node, CheckerTypes.int64),
            List(int32Node, int32Node, CheckerTypes.int32),
            List(int32Node, int64Node, CheckerTypes.int64),
            List(int64Node, int64Node, CheckerTypes.int64))
            
    for (allowed <- allowedIntDivTable)
    {
        for (op <- intDivOps)
        {
            val type1 = allowed(0).asInstanceOf[NamedTypeNd]
            val type2 = allowed(1).asInstanceOf[NamedTypeNd]
            val expectedType = allowed(2).asInstanceOf[CheckerTypes.PrimitiveType]
            
            it should "allow " + type1.toString().substring(2) + " " + op(1) + " " + type2.toString().substring(2) + " giving " + expectedType.toString().substring(2) in {
                val x = makeOperandNode( type1, "x", coordX )
                val y = makeOperandNode( type2, "y", coordY )
                allow( op(0).asInstanceOf[BinaryOperator], x, y, expectedType )
            }
            
            if (!type1.equals(type2)) {
                it should "allow " + type2.toString().substring(2) + " " + op(1) + " " + type1.toString().substring(2) + " giving " + expectedType.toString().substring(2) in {
                    val x = makeOperandNode( type2, "x", coordX )
                    val y = makeOperandNode( type1, "y", coordY )
                    allow( op(0).asInstanceOf[BinaryOperator], x, y, expectedType )
                }
            }
        }
    }
    
    // Unary operators
    it should "allow ~Bool giving Bool" in {
        val type1 = boolNode
        val x = makeOperandNode( type1, "x", coordX )
        allow( unaryOps(1)(0).asInstanceOf[UnaryOperator], x )
    }
    
    it should "allow -Arithmetic giving the same type" in {
        for (type1 <- realTypes ::: intTypes) {
            val x = makeOperandNode( type1, "x", coordX )
            allow( unaryOps(0)(0).asInstanceOf[UnaryOperator], x )
        }
    }

    // Negative Tests
    
    // Arithmetic-Arithmetic tests with boolean operators.   
    for (op <- boolOps) {
        for (type1 <- realTypes ::: intTypes) {
            for (type2 <- realTypes ::: intTypes) {
                it should "forbid " + type1.toString().substring(2) + " " + op(1) + " " + type2.toString().substring(2) in {
                    val x = makeOperandNode( type1, "x", coordX )
                    val y = makeOperandNode( type2, "y", coordY )
                    forbid( op(0).asInstanceOf[BinaryOperator], x, y )
                }
            }
        }
    }
    
    // Bool-Bool tests with chaining operators.
    for (op <- compOps) {
        it should "forbid Bool " + op(1) + " Bool" in {
            val x = makeOperandNode( boolNode, "x", coordX )
            val y = makeOperandNode( boolNode, "y", coordY )
            forbid( List( op(0).asInstanceOf[ChainingOperator] ), List( x, y ) )
        }
    }
    
    // Bool-Bool tests with arithmetic operators.
    for (op <- arithOps) {
        it should "forbid Bool " + op(1) + " Bool" in {
            val x = makeOperandNode( boolNode, "x", coordX )
            val y = makeOperandNode( boolNode, "y", coordY )
            forbid( op(0).asInstanceOf[BinaryOperator], x, y )
        }
    }
    
    // Bool-Bool test with / operator.
    it should "forbid Bool / Bool" in {
        val x = makeOperandNode( boolNode, "x", coordX )
        val y = makeOperandNode( boolNode, "y", coordY )
        forbid( SlashDivOp, x, y )
    }
    
    // Bool-Bool test with integer division operators.
    for (op <- intDivOps) {
        it should "forbid Bool " + op(1) + " Bool" in {
            val x = makeOperandNode( boolNode, "x", coordX )
            val y = makeOperandNode( boolNode, "y", coordY )
            forbid( op(0).asInstanceOf[BinaryOperator], x, y )
        }
    }
    
    // Real-Arithmetic test with integer division operators.
    for (op <- intDivOps) {
        for (type1 <- realTypes) {
            for (type2 <- intTypes)
            {
                it should "forbid " + type1.toString().substring(2) + " " + op(1) + " " + type2.toString().substring(2) in {
                    val x = makeOperandNode( type1, "x", coordX )
                    val y = makeOperandNode( type2, "y", coordY )
                    forbid( op(0).asInstanceOf[BinaryOperator], x, y )
                }
                
                if (!type1.equals(type2)) {
                    it should "forbid " + type2.toString().substring(2) + " " + op(1) + " " + type1.toString().substring(2) in {
                        val x = makeOperandNode( type2, "x", coordX )
                        val y = makeOperandNode( type1, "y", coordY )
                        forbid( op(0).asInstanceOf[BinaryOperator], x, y )
                    }
                }
            }
            
            for (type2 <- realTypes)
            {
                it should "forbid " + type1.toString().substring(2) + " " + op(1) + " " + type2.toString().substring(2) in {
                    val x = makeOperandNode( type1, "x", coordX )
                    val y = makeOperandNode( type2, "y", coordY )
                    forbid( op(0).asInstanceOf[BinaryOperator], x, y )
                }
            }
        }
    }
    
    // Bool-Arithmetic tests with all operators.
    for (op <- allBinaryOps) {
        for (type1 <- realTypes ::: intTypes) {
            it should "forbid " + type1.toString().substring(2) + " " + op(1) + " Bool" in {
                val x = makeOperandNode( type1, "x", coordX )
                val y = makeOperandNode( boolNode, "y", coordY )
                forbid( op(0).asInstanceOf[BinaryOperator], x, y )
            }
            
            it should "forbid Bool " + op(1) + " " + type1.toString().substring(2) in {
                val x = makeOperandNode( boolNode, "x", coordX )
                val y = makeOperandNode( type1, "y", coordY )
                forbid( op(0).asInstanceOf[BinaryOperator], x, y )
            }
        }
    }
    
    for (op <- compOps) {
        for (type1 <- realTypes ::: intTypes) {
            it should "forbid " + type1.toString().substring(2) + " " + op(1) + " Bool" in {
                val x = makeOperandNode( type1, "x", coordX )
                val y = makeOperandNode( boolNode, "y", coordY )
                forbid( List( op(0).asInstanceOf[ChainingOperator] ), List( x, y ) )
            }
            
            it should "forbid Bool " + op(1) + " " + type1.toString().substring(2) in {
                val x = makeOperandNode( boolNode, "x", coordX )
                val y = makeOperandNode( type1, "y", coordY )
                forbid( List( op(0).asInstanceOf[ChainingOperator] ), List( x, y ) )
            }
        }
    }
    
    // Int64-Real tests with all operators
    for (op <- compOps) {
        for (type1 <- realTypes) {
            it should "forbid Int64 " + op(1) + " " + type1.toString().substring(2) in {
                val x = makeOperandNode( int64Node, "x", coordX )
                val y = makeOperandNode( type1, "y", coordY )
                forbid( List( op(0).asInstanceOf[ChainingOperator] ), List( x, y ) )
            }
            
            it should "forbid " + type1.toString().substring(2) + op(1) + " Int64" in {
                val x = makeOperandNode( type1, "x", coordX )
                val y = makeOperandNode( int64Node, "y", coordY )
                forbid( List( op(0).asInstanceOf[ChainingOperator] ), List( x, y ) )
            }
        }
    }
    
    // Unary operators
    it should "forbid ~Arithmetic" in {
        for ( type1 <- realTypes ::: intTypes ) {
            val x = makeOperandNode( type1, "x", coordX )
            forbid( unaryOps(1)(0).asInstanceOf[UnaryOperator], x )
        }
    }
    
    it should "forbid -Bool" in {
        val type1 = boolNode
        val x = makeOperandNode( type1, "x", coordX )
        forbid( unaryOps(0)(0).asInstanceOf[UnaryOperator], x )
    }

    private def makeNamedTypeNode( name: String, tipe: CheckerTypes.Type ) = {
        val node = NamedTypeNd( NameNd( FQN( List( name ) ) )( noCoord ) )( noCoord )
        node.tipe = Some( tipe )
        node
    }

    private def makeOperandNode( typeNode: NamedTypeNd, name: String, coord: Coord ) = {
        val node = NameExpNd( NameNd( FQN( List( name ) ) )( coord ) )( coord )
        val decl = ObjDeclNd( true, PrivateAccess, typeNode, null )( name, noCoord );
        node.name.decl = Some( decl )
        node
    }

    private def allow( op: BinaryOperator,
                       x: ExpNd,
                       y: ExpNd,
                       expectedType: CheckerTypes.Type ) = {
        val ter = new StandardErrorRecorder;
        val typeCreator = new TypeCreator(ter) ;
        val typeChecker = new TypeChecker( ter, typeCreator );
        val ast = BinaryOpExpNd( op, x, y )( coordOp )
        val tcResult = typeChecker.typeCheck( ast )
       
        if ( verbose ) {
            println( ast.toString ); println( "has type" ); println( tcResult )
        }

        //Expect no errors
        assertResult( 0 )( ter.getFatalCount() + ter.getWarningCount() )

        tcResult match {
            case None => assert( false )
            case Some( tipe ) =>
                // Check the type of the root
                assertResult( expectedType )( tipe )
                assert( ast.tipe.isDefined )
                //Check the types of the two branches off the root.
                assertResult( expectedType )( ast.tipe.get )
                assert( ast.x.tipe.isDefined )
                assertResult( expectedType )( ast.x.tipe.get )
                assert( ast.y.tipe.isDefined )
                assertResult( expectedType )( ast.y.tipe.get )
                // Check that the branches are unchanged or are widening conversions
                assert( ast.x == x || ( ast.x match { case AsExpNd( x1, _ ) => x == x1; case _ => false } ) )
                assert( ast.y == y || ( ast.y match { case AsExpNd( y1, _ ) => y == y1; case _ => false } ) )
        }
    }
    
    private def allow( op: UnaryOperator,
                       x: ExpNd ) = {
                       println( op )
                       println( x )
                       val ter = new StandardErrorRecorder;
                       val typeCreator = new TypeCreator( ter );
                       val typeChecker = new TypeChecker( ter, typeCreator );
                       val ast = UnaryOpExpNd( op, x )( coordOp )
                       
                       val tcResult = typeChecker.typeCheck( ast )
                       
                       assertResult( 0 )( ter.getFatalCount() + ter.getWarningCount() )
                       
                       tcResult match {
                         case None => assert( false )
                         case Some( tipe ) =>
                             assert( x.tipe.isDefined )
                             assertResult( x.tipe.get )( tipe )
                             assert( ast.tipe.isDefined )
                             assertResult( x.tipe.get )( ast.tipe.get )
                             assertResult( x.tipe.get )( ast.x.tipe.get )                       }
    }
    
    private def allow( ops: List[ChainingOperator],
                       opers: List[ExpNd] ) = {
        val ter = new StandardErrorRecorder;
        val typeCreator = new TypeCreator( ter ) ;
        val typeChecker = new TypeChecker( ter, typeCreator );
        val ast = ChainExpNd( ops, opers )( coordOp )
        val tcResult = typeChecker.typeCheck( ast )
        
        if ( verbose ) {
            println( ast.toString ); println( "has type" ); println( tcResult )
        }
        
        //Expect no errors
        assertResult( 0 )( ter.getFatalCount() + ter.getWarningCount() )
        
        tcResult match {
          case None => assert( false )
          case Some( tipe ) =>
              //Check the type of the root.
              assertResult( CheckerTypes.bool )( tipe )
              assert( ast.tipe.isDefined )
        }
    }

    private def forbid( op: BinaryOperator,
                        x: ExpNd,
                        y: ExpNd ) = {
        val ter = new StandardErrorRecorder;
        val typeCreator = new TypeCreator(ter) ;
        val typeChecker = new TypeChecker( ter, typeCreator );
        val ast = BinaryOpExpNd( op, x, y )( coordOp )
        val tcResult = typeChecker.typeCheck( ast )

        if ( verbose ) {
            println( ast.toString ); println( "has type" ); println( tcResult )
        }

        assert( !tcResult.isDefined )
        
        //Expect one fatal error
        assertResult( 1 )( ter.getFatalCount() )
        assertResult( 0 )( ter.getWarningCount() )
        
        assertResult( coordOp )( ter.getErrorCoord(0) )

        if ( verbose ) println( ter.getFatalCoord( 0 )+" "+ter.getFatalText( 0 ) );
    }
    
    private def forbid( op: UnaryOperator,
                        x: ExpNd ) = {
        val ter = new StandardErrorRecorder;
        val typeChecker = new TypeChecker( ter, null );
        val ast = UnaryOpExpNd( op, x )( coordOp );
        val tcResult = typeChecker.typeCheck( ast );
        
        if ( verbose ) {
            println( ast.toString ); println( " hase type " ); println( tcResult );
        }
        
        assertResult( 1 )( ter.getFatalCount() );
        assertResult( 0 )( ter.getWarningCount() );
        
        assertResult( coordOp )( ter.getErrorCoord( 0 ) );
        
        if ( verbose ) println( ter.getFatalCoord( 0 ) + " " + ter.getFatalText( 0 ) );
    }

    private def forbid( ops: List[ChainingOperator],
                       opers: List[ExpNd] ) = {
        val ter = new StandardErrorRecorder;
        val typeChecker = new TypeChecker( ter, null );
        val ast = ChainExpNd( ops, opers )( coordOp )
        val tcResult = typeChecker.typeCheck( ast )
        
        if ( verbose ) {
            println( ast.toString ); println( "has type" ); println( tcResult )
        }
        
        //Expect one fatal error
        assertResult( 1 )( ter.getFatalCount() )
        assertResult( 0 )( ter.getWarningCount() )
        
        assertResult( coordOp )( ter.getErrorCoord(0) )
        
        if ( verbose ) println ( ter.getFatalCoord( 0 )+" "+ter.getFatalText( 0 ) );
    }
}