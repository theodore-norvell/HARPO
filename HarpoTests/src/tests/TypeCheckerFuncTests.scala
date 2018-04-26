package tests

import org.scalatest.BeforeAndAfterEach
import org.scalatest.TestData
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import java.io.StringReader
import parser.TokenMgrError
import parser.HarpoParser
import checker.Checker
import frontEnd.AST._
import frontEnd.StandardErrorRecorder
import parser.ParseException
import checker.CheckerTypes

@RunWith(classOf[JUnitRunner]) 
class TypeCheckerFuncTests extends TestsBase with BeforeAndAfterEach {
    val verbose = true;
    
    override def beforeEach( td: TestData ) {
    if ( verbose ) println( ">>>>>>>>>>>>>Starting "+td.name+" >>>>>>>>>>>>>>>>>>" )
    }

    override def afterEach( td: TestData ) {
        if ( verbose ) println( "<<<<<<<<<<<<<Finished "+td.name+" <<<<<<<<<<<<<<<<<" )
    }
    
    /****************************************
     * TYPE CHECKING TESTS:
     * EXPRESSIONS
     ****************************************/
    behavior of "AsExpNd";
    it should "accept arithmetic as arithmetic" in {
        val result = checkCode( "obj x : Int16 := 5 as Int8" )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
        
        for ( decl <- dl.decls ) {
            if ( decl.isInstanceOf[ObjDeclNd] ) {
                val obj = decl.asInstanceOf[ObjDeclNd]
                if ( obj.init.isInstanceOf[WidenInitExpNd]) {
                    val widen = obj.init.asInstanceOf[WidenInitExpNd]
                    println( widen.a )
                    if ( widen.a.isInstanceOf[ValueInitExpNd] ) {
                        val init = widen.a.asInstanceOf[ValueInitExpNd]
                        if ( init.exp.isInstanceOf[AsExpNd] ) {
                            val as = init.exp.asInstanceOf[AsExpNd]
                            as.ty.tipe match {
                                case None => assert( false )
                                case Some( tipe ) =>
                                    assertResult( CheckerTypes.int8 )( tipe )
                            }
                        }
                    }
                }
            }
        }
    }
    
    it should "reject arithmetic as non-arithmetic" in {
        val result = checkCode( "obj x : Int16 := 5 as Bool" )
        
        val dl = result._1
        val er = result._2
        
        assertResult( 1 )( er.getFatalCount() )
        assertResult( 0 )( er.getWarningCount() )
        assertResult( "The as operation can only convert to arithmetic types." )( er.getErrorText(0) )
    }
    
    it should "reject non-arithmetic" in {
        val result = checkCode( "obj x : Bool := true as Int8" )
        
        val dl = result._1
        val er = result._2
        
        printErrors( er )
        assertResult( 1 )( er.getFatalCount() )
        assertResult( 0 )( er.getWarningCount() )
        assertResult( "The as operation applies only to arithmetic values." )( er.getErrorText(0) )
    }
    
    behavior of "MemberExpNd";
    //TODO: how do I test this?
    
    /****************************************
     * TYPE CHECKING TESTS:
     * INITIALIZATION EXPRESSIONS
     ****************************************/
    behavior of "ValueInitExpNd";
    it should "accept a constant value" in {
        val result = checkCode( "obj x : Int8 := 5" )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
    }
    
    it should "accept a variable that holds a constant value" in {
        val result = checkCode( "obj x : Int8 := 5; obj y : Int8 := x;")
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
    }
    
    behavior of "NewInitExpNd";
    it should "accept constructors of non-generic types with the proper number of arguments" in {
        val code = """(class A(obj x : Int8, obj y : Int8, obj z : Bool) class A)
                      obj a : A := new A(5, 4, false)"""
    
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
    }
    
    //FIXME: (FAIL) gets zero fatal errors - expected 1
    it should "reject constructors with the wrong number of arguments" in {
        val code = """(class A(obj x : Int8, obj y : Int8, obj z : Bool) class A)
                      obj a := new A(5, 4)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assertResult( 1 )( er.getFatalCount() )
        assertResult( 0 )( er.getWarningCount() )
        assertResult( "..." )( er.getErrorText(0) )
    }
    
    //FIXME: (FAIL) gets zero fatal errors - expected 1
    it should "reject constructors with the wrong types of arguments" in {
        val code = """(class A(obj x : Int8, obj y : Int8, obj z : Bool) class A)
                      obj a := new A(5, false, true)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assertResult( 1 )( er.getFatalCount() )
        assertResult( 0 )( er.getWarningCount() )
        assertResult( "..." )( er.getErrorText( 0 ) )
    }
    
    //FIXME: (FAIL) gets zero fatal errors - expected 1
    it should "reject constructors to generic classes" in {
        val code = """(class A {type T} (obj x : Int8, obj y : Int8, obj z : Bool) class A)
                      obj a := new A(5, 4, true)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assertResult( 1 )( er.getFatalCount() )
        assertResult( 0 )( er.getWarningCount() )
        assertResult( "..." )( er.getErrorText(0) )
    }
    
    //FIXME: (FAIL) Assertion error on "check"
    behavior of "ArrayInitExpNd";
    it should "create an ArrayType with the type of the elements and the bounds" in {
        val code = """obj x := (for i : 10 do i)"""
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
        
        for ( decl <- dl.decls ) {
            println( "Decl: " + decl )
            //TODO: Create a proper test for this
            // I was just trying to get a look at the structure of the AST
            // and the test failed before even finishing with the Checker.
        }
    }
    
    behavior of "IfInitExpNd";
    //FIXME: (FAIL) Assertion failed "unreachable"
    it should "pick the least supertype of all InitExps" in {
        val code = "obj x := 5; obj y := (if x < 10 then 5 as Int16 else 15 as Int64)"// else if x < 15 then 10 as Int32 else 15 as Int64)"
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
        
        for ( decl <- dl.decls ) {
            if (decl.isInstanceOf[ObjDeclNd]) {
                val obj = decl.asInstanceOf[ObjDeclNd]
                if ( obj.init.isInstanceOf[IfInitExpNd] ) {
                    val init = obj.init.asInstanceOf[IfInitExpNd]
                    init.tipe match {
                        case None => assert( false )
                        case Some( tipe ) =>
                            assertResult( CheckerTypes.int16 )( tipe )
                    }
                }
            }
        }
    }
    
    //FIXME: (FAIL) Assertion failed "todo" - not implemented yet.
    it should "reject non-bool guards" in {
        val code = "obj y := (if 10 then 5 as Int16 else 15 as Int64)"
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assertResult( 1 )( er.getFatalCount() )
        assertResult( 0 )( er.getWarningCount() )
        assertResult( "..." )( er.getErrorText(0) )
    }
    
    /****************************************
     * TYPE CHECKING TESTS:
     * DECLARATIONS
     ****************************************/
    behavior of "ClassDecl";
    it should "have the proper types on all class parameters" in {
        val paramTypes = List(
                            CheckerTypes.int8,
                            CheckerTypes.int16,
                            CheckerTypes.real64,
                            CheckerTypes.bool )
                            
        val memTypes = List(
                          CheckerTypes.int8,
                          CheckerTypes.int16 )
          
        val code = """(class A(obj x : Int8, obj y : Int16, obj z : Real64, obj w : Bool)
                          obj a : Int8 := x;
                          obj b := y;
                          private proc test(in i : Int16);
                      class A)"""
                    
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
        
        for ( decl <- dl.decls ) {
            if ( decl.isInstanceOf[frontEnd.AST.ClassDeclNd] ) {
                val node = decl.asInstanceOf[frontEnd.AST.ClassDeclNd]
                
                var i = 0
                for ( param <- node.constructorParams ) {
                    param.ty.tipe match {
                        case None => assert( false )
                        case Some( tipe ) =>
                            val base = tipe.asInstanceOf[CheckerTypes.LocationType].base
                            assertResult( paramTypes(i) )( base )
                    }
                    i += 1
                }
                
                i = 0
                for ( member <- node.directMembers ) {
                    if ( member.isInstanceOf[frontEnd.AST.ObjDeclNd] ) {
                        val obj = member.asInstanceOf[frontEnd.AST.ObjDeclNd]
                        obj.ty.tipe match {
                            case None => assert( false )
                            case Some( tipe ) =>
                                val base = tipe.asInstanceOf[CheckerTypes.LocationType].base
                                assertResult( memTypes(i) )( base )
                        }
                        i += 1
                    }
                }
            }
        }
    }   
    
    behavior of "IntfDecl";
    it should "have the proper types on all interface parameters" in {
        val memTypes = List(
                              CheckerTypes.real16,
                              CheckerTypes.bool,
                              CheckerTypes.int64 )
       
        val code = """(interface A
                          obj x : Real16 := 1.0;
                          obj y : Bool := false;
                          obj z : Int64 := 4;
                      interface A)"""
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
        
        for ( decl <- dl.decls ) {
            if ( decl.isInstanceOf[ClassDeclNd] ) {
                val node = decl.asInstanceOf[ClassDeclNd]
                
                var i = 0
                for ( member <- node.directMembers ) {
                    if ( member.isInstanceOf[ObjDeclNd] ) {
                        val obj = member.asInstanceOf[ObjDeclNd]
                        obj.ty.tipe match {
                            case None => assert( false )
                            case Some( tipe ) =>
                                val base = tipe.asInstanceOf[CheckerTypes.LocationType].base
                                assertResult( memTypes(i) )( base )
                        }
                        i += 1
                    }
                }
            }
        }
    }

    behavior of "ObjDecl";
    it should "match the type of the InitExp if no type is specified" in {
        val result = checkCode( "obj x := 1000 as Int8")
        val dl = result._1
        val er = result._2
      
        val objDecl = dl.decls.head.asInstanceOf[ObjDeclNd]
        val objDeclType = objDecl.ty.tipe.get.asInstanceOf[CheckerTypes.LocationType].base
        val initExpType = objDecl.init.tipe
        
        assertResult( 0 )( er.getFatalCount() + er.getWarningCount() )
        assertResult( initExpType.get )( objDeclType )
    }
    
    it should "allow a specified type that is a super type of its InitExp" in {
        val result = checkCode( "obj x : Int32 := 10 as Int8" )
        val dl = result._1
        val er = result._2
        
        val objDecl = dl.decls.head.asInstanceOf[ObjDeclNd]
        val objDeclType = objDecl.ty.tipe.get.asInstanceOf[CheckerTypes.LocationType].base
        
        assertResult( 0 )( er.getFatalCount() + er.getWarningCount() )
        assertResult( CheckerTypes.int32 )( objDeclType )
    }
    
    //FIXME: (FAIL) gets zero fatal errors - expected 1
    // Type cannot be generic
    it should "forbid a generic type from being used" in {
        val code = """(class A {type T} () class A)
                      obj x : A := new A()"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 1 )
        assertResult( "..." )( er.getErrorText(0) )
     
    }
    
    it should "declare a named location if the type, or inferred type, is primitive" in {
        val result = checkCode( "obj x : Int32 := 10 as Int8" )
        val dl = result._1
        val er = result._2
        
        val objDecl = dl.decls.asInstanceOf[ObjDeclNd]
        val objDeclType = objDecl.ty.tipe.get
        
        assert( objDeclType.isInstanceOf[CheckerTypes.LocationType] )
        
        assertResult( 0 )( er.getFatalCount() + er.getWarningCount() )
    }
    
    //FIXME: (FAIL) Assertion error on "check"
    it should "declare an array of named locations if the type is an array type" in {
        val result = checkCode( "obj x : Int8[10] := (for i : 10 do i)" )
        val dl = result._1
        val er = result._2
        
        val objDecl = dl.decls.asInstanceOf[ObjDeclNd]
        val objDeclType = objDecl.ty.tipe.get
          
        assertResult( 0 )( er.getFatalCount() + er.getWarningCount() )
    }
    
    it should "name an object if the type is a class" in {
        val code = """(class B(obj x : Int8) public obj w : Int8 := x; class B);
                      obj y : B := new B(5)
                      obj z : Int8 := y.w"""
        
        val result = checkCode( code )
        
        val dl = result._1
        val er = result._2
        
        // This succeeding shows that the line "obj z : Int8 := y.w" actually
        // works, meaning that y is a valid name for a class object.
        assertResult( 0 )( er.getFatalCount() + er.getWarningCount() )
    }
    
    it should "require the type to be primitive if the declaration is const" in {
        val result = checkCode( "const x : Int32 := 5 as Int8" )
        
        val dl = result._1
        val er = result._2
        
        val objDecl = dl.decls.asInstanceOf[ObjDeclNd]
        
        assertResult( 0 )( er.getFatalCount() + er.getWarningCount() )
    }
    
    //FIXME: (FAIL) gets zero fatal errors - expected 1
    it should "forbid non-primitives if the declaration is const" in {
        val code = """
                   (class A(obj x : Int8) class A)
                   const y : A := new A(5)"""
        
        val result = checkCode( code )
        
        val er = result._2
        
        assertResult( 1 )( er.getFatalCount() )
    }
    
    behavior of "MethodDecl";
    it should "have the proper types on all method parameters" in {
        val paramTypes = List(
                          CheckerTypes.bool,
                          CheckerTypes.int32,
                          CheckerTypes.real16 )
                         
        val code = """(class A()
                          private proc test(in a : Bool, in b : Int32, out c : Real16);
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
        
        for ( decl <- dl.decls ) {
            if ( decl.isInstanceOf[MethodDeclNd] ) {
                val method = decl.asInstanceOf[MethodDeclNd]
                var i = 0
                for ( param <- method.params ) {
                    param.ty.tipe match {
                        case None => assert( false )
                        case Some( tipe ) =>
                              val base = tipe.asInstanceOf[CheckerTypes.LocationType].base
                              assertResult( paramTypes(i) )( base )
                    }
                    i += 1
                }
            }
        }
    }
    
    it should "forbid non-primitive types being used as params" in {
        val result = checkCode( "(class A() class A) (class B() private proc test(in a : A); class B)" )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 1 )
        assertResult( "Parameters must have primitive types" )( er.getFatalText(0) )
    }
    
    behavior of "ThreadDecl";
    it should "have the proper types on all local variables" in {
        val localDeclTypes = List(
                                CheckerTypes.int16,
                                CheckerTypes.real32 )
        val code = """(class A()
                          (thread 
                              obj x : Int16 := 5
                              obj y : Real32 := 2.5
                          thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
        
        for ( decl <- dl.decls ) {
            if ( decl.isInstanceOf[ClassDeclNd] ) {
                val classDecl = decl.asInstanceOf[ClassDeclNd]
                for ( member <- classDecl.directMembers ) {
                    if ( member.isInstanceOf[ThreadDeclNd] ) {
                        val thread = member.asInstanceOf[ThreadDeclNd]
                        val block_0 = thread.block.asInstanceOf[LocalDeclCmdNd]
                        val decl_0 = block_0.decl.asInstanceOf[LocalDeclNd]
                        
                        decl_0.ty.tipe match {
                            case None => assert( false )
                            case Some( tipe ) =>
                                val base = tipe.asInstanceOf[CheckerTypes.LocationType].base
                                assertResult( localDeclTypes(0) )( base )
                        }
                        
                        val block_1 = decl_0.cmd.asInstanceOf[LocalDeclCmdNd]
                        val decl_1 = block_1.decl.asInstanceOf[LocalDeclNd]
                        
                        decl_1.ty.tipe match {
                            case None => assert( false )
                            case Some( tipe ) =>
                                val base = tipe.asInstanceOf[CheckerTypes.LocationType].base
                                assertResult( localDeclTypes(1) )( base )
                        }
                    }
                }
            }
        }
    }
    
    behavior of "GenericParamDecl";
    it should "accept non-generic types/classes as generic parameters" in {
        val code = """(class A {type T} () class A)
                      (class B() obj x : Int8 := 5; class B)
                      obj a : A{B} := new A{B}()
                      """
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
    }
    
    //FIXME: (FAIL) gets zero fatal errors - expected 1
    it should "forbid generic types as generic parameters" in {
        val code = "(class A {type T, type R extends T} () class A)"
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 1 )
        assertResult( "..." )( er.getFatalText(0) )
    }
    
    it should "forbid generic classes as generic parameters" in {
        val code = """(class A {type T} () class A)
                      (class B {type R extends A} () class B)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        assert( er.getFatalCount() + er.getWarningCount() == 1 )
        assertResult( "..." )( er.getFatalText(0) )
    }
    
    /****************************************
     * TYPE CHECKING TESTS:
     * COMMANDS
     ****************************************/    
    behavior of "CallCmdNd";
    // FIXME: (FAIL) Type error: should be type ::type but it is of type Some(::type)
    it should "accept calling of public methods in any class" in {
        val code = """(class A()
                        obj o1 : B := new B()
                        obj o2 : Int64 := 1
                        obj o3 : Int64 := 0
                        (thread
                          o1.b1(o2, o3)
                        thread)
                      class A)
                      
                      (class B()
                        public proc b1(in y1 : Int64, out y2 : Int64)
                        private proc b2(in y3 : Int64, out y4 : Int64)
                        (thread
                          (accept
                            b1(in y1 : Int64, out y2 : Int64)
                              y2 := y1
                            |
                            b2(in y3 : Int64, out y4 : Int64)
                              y4 := y3
                          accept)
                        thread)
                      class B)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
    }
    
    it should "accept calling of public or private methods in the current class (same thread)" in {
        // FIXME: (FAIL) a1, a2 does not represent an object or location
        val code = """(class A()
                        obj o1 : Int64 := 1
                        obj o2 : Int64 := 0
                        obj o3 : Int64 := 0
                        public proc a1(in x1 : Int64, out x2 : Int64)
                        private proc a2(in x3 : Int64, out x4 : Int64)
                        (thread
                          (accept
                            a1(in x1 : Int64, out x2 : Int64)
                              x2 := x1
                            |
                            a2(in x3 : Int64, out x4 : Int64)
                              x4 := x3
                          accept)
                          a1(o1, o2)
                          a2(o2, o3)
                        thread)
                        //(thread
//
                        //thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
    }
    
    it should "accept calling of public or private methods in the current class (different thread)" in {
        val code = """(class A()
                        obj o1 : Int64 := 1
                        obj o2 : Int64 := 0
                        obj o3 : Int64 := 0
                        public proc a1(in x1 : Int64, out x2 : Int64)
                        private proc a2(in x3 : Int64, out x4 : Int64)
                        (thread
                          (accept
                            a1(in x1 : Int64, out x2 : Int64)
                              x2 := x1
                            |
                            a2(in x3 : Int64, out x4 : Int64)
                              x4 := x3
                          accept)
                          
                        thread)
                        (thread
                          a1(o1, o2)
                          a2(o2, o3)
                        thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
    }
    
    it should "reject non class/interface types being used as object ID" in {
        val code = """(class A()
                        obj oo1 : Int64 := 0
                        obj o2 : Int64 := 1
                        obj o3 : Int64 := 0
                        (thread
                          oo1.b1(o2, o3)
                        thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 1 )
        assertResult( "Only classes and interfaces have members." )( er.getFatalText(0) )
    }
    
    it should "reject calls to private methods in other classes/interfaces" in {
      // FIXME: (FAIL) Type error instead of accessibility error.
        val code = """(class A()
                        obj o1 : B := new B()
                        obj o2 : Int64 := 1
                        obj o3 : Int64 := 0
                        (thread
                          o1.b2(o2, o3)
                        thread)
                      class A)
                      
                      (class B()
                        public proc b1(in y1 : Int64, out y2 : Int64)
                        private proc b2(in y3 : Int64, out y4 : Int64)
                        (thread
                          (accept
                            b1(in y1 : Int64, out y2 : Int64)
                              y2 := y1
                            |
                            b2(in y3 : Int64, out y4 : Int64)
                              y4 := y3
                          accept)
                        thread)
                      class B)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 1 )
        assertResult( "..." )( er.getFatalText(0) )
    }
    
    it should "accept the same type for 'in' parameters" in {
        // FIXME: (FAIL) a1 does not represent an object or location
        val code = """(class A()
                        obj o1 : Int64 := 1
                        obj o2 : Int64 := 0
                        public proc a1(in x1 : Int64, out x2 : Int64)
                        (thread
                          (accept
                            a1(in x1 : Int64, out x2 : Int64)
                              x2 := x1
                          accept)
                        thread)
                        (thread
                           a1(o1, o2)
                        thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
    }
    
    it should "accept types that can be widened to the proper type for 'in' parameters" in {
        // FIXME: (FAIL) a1 does not represent an object or location
        val code = """(class A()
                        obj o1 : Int32 := 1
                        obj o2 : Int64 := 0
                        public proc a1(in x1 : Int64, out x2 : Int64)
                        (thread
                          (accept
                            a1(in x1 : Int64, out x2 : Int64)
                              x2 := x1
                          accept)
                        thread)
                        (thread
                          a1(o1, o2)
                        thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
    }
    
    it should "reject invalid types for 'in' parameters" in {
        // FIXME: (FAIL) a1 does not represent an object or location
        val code = """(class A()
                        obj o1 : Bool := true
                        obj o2 : Int64 := 0
                        public proc a1(in x1 : Int64, out x2 : Int64)
                        (thread
                          (accept
                            a1(in x1 : Int64, out x2 : Int64)
                              x2 := x1
                          accept)
                        thread)
                        (thread
                          a1(o1, o2)
                        thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 1 )
        assertResult( "Argument for parameter ::A::a1::x1 should the of type ::Int64 or wider, but is of type Some(::Bool)." )( er.getFatalText(0) )
    }
    
    it should "accept location(type) of the proper type for 'out' parameters" in {
        val code = """(class A()
                        obj o1 : Int64 := 1
                        obj o2 : Int64 := 0
                        public proc a1(in x1 : Int64, out x2 : Int64)
                        (thread
                          (accept
                            a1(in x1 : Int64, out x2 : Int64)
                              x2 := x1
                          accept)
                        thread)
                        (thread
                          a1(o1, o2)
                        thread)
                      class A)"""
        
        val result = checkCode(code)
        val dl = result._1
        val er = result._2
        
        for (decl <- dl.decls) {
            if (decl.isInstanceOf[ClassDeclNd]) {
                val cls = decl.asInstanceOf[ClassDeclNd]
                for (mem <- cls.directMembers) {
                    if (mem.isInstanceOf[ThreadDeclNd]) {
                        val thread = mem.asInstanceOf[ThreadDeclNd]
                        if (thread.block.isInstanceOf[CallCmdNd]) {
                            val call = thread.block.asInstanceOf[CallCmdNd]
                            for (arg <- call.argList) {
                                if (arg.isInstanceOf[FetchExpNd]) {
                                    val fetch = arg.asInstanceOf[FetchExpNd]
                                    if (fetch.x.isInstanceOf[NameExpNd]) {
                                        if (fetch.x.asInstanceOf[NameExpNd].name.equals("o2")) {
                                            val o2 = fetch.x.asInstanceOf[NameExpNd]
                                            assert(o2.tipe.get.isInstanceOf[CheckerTypes.LocationType], "The out parameter is not a location type.")
                                        } 
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
    }
    
    it should "accept location(type) of a wider type for 'out' parameters" in {
        val code = """(class A()
                        obj o1 : Int16 := 1
                        obj o2 : Int64 := 0
                        public proc a1(in x1 : Int16, out x2 : Int16)
                        (thread
                          (accept
                            a1(in x1 : Int16, out x2 : Int16)
                              x2 := x1
                          accept)
                        thread)
                        (thread
                          a1(o1, o2)
                        thread)
                      class A)"""
        
        val result = checkCode(code)
        val dl = result._1
        val er = result._2
        
        for (decl <- dl.decls) {
            if (decl.isInstanceOf[ClassDeclNd]) {
                val cls = decl.asInstanceOf[ClassDeclNd]
                for (mem <- cls.directMembers ) {
                    if (mem.isInstanceOf[ThreadDeclNd]) {
                        val thread = mem.asInstanceOf[ThreadDeclNd]
                        if (thread.block.isInstanceOf[CallCmdNd]) {
                            val call = thread.block.asInstanceOf[CallCmdNd]
                            for (arg <- call.argList) {
                                if (arg.isInstanceOf[FetchExpNd]) {
                                    val fetch = arg.asInstanceOf[FetchExpNd]
                                    if (fetch.x.isInstanceOf[NameExpNd]) {
                                        if (fetch.x.asInstanceOf[NameExpNd].name.equals("o2")) {
                                            val o2 = fetch.x.asInstanceOf[NameExpNd]
                                            assert(o2.tipe.get.isInstanceOf[CheckerTypes.LocationType], "The out parameter is not a location type.")
                                        } 
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        assert( er.getFatalCount() + er.getWarningCount() == 0 )
    }
    
    it should "reject non-locations for 'out' parameters" in {
        val code = """(class A()
                        obj o1 : Int64 := 1
                        obj o2 : Int64 := 0
                        public proc a1(in x1 : Int64, out x2 : Int64)
                        (thread
                          (accept
                            a1(in x1 : Int64, out x2 : Int64)
                              x2 := x1
                          accept)
                        thread)
                        (thread
                          a1(o1, 5)
                        thread)
                      class A)"""
        
        val result = checkCode(code)
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 1 )
        assertResult( "Argument for 'out' parameter ::A::a1::x2 should refer to a location." )( er.getFatalText(0) )
    }
    
    it should "reject invalid types for 'out' parameters" in {
        val code = """(class A()
                        obj o1 : Int64 := 1
                        obj o2 : Bool := false
                        public proc a1(in x1 : Int64, out x2 : Int64)
                        (thread
                          (accept
                            a1(in x1 : Int64, out x2 : Int64)
                              x2 := x1
                          accept)
                        thread)
                        (thread
                          a1(o1, o2)
                        thread)
                      class A)"""
        
        val result = checkCode(code)
        val dl = result._1
        val er = result._2
        
        assert( er.getFatalCount() + er.getWarningCount() == 1 )
        assertResult( "Argument for parameter ::A::a1::x2 should the of type ::Int64 or narrower, but is of type ::Bool." )( er.getFatalText(0) )
    }
    
    behavior of "IfCmdNd";
    it should "accept boolean expressions for the guard" in {
        val code = """(class A(obj a1 : Int8)
                          obj x0 : Int8 := a1;
                          (thread
                                obj b : Int8 := -1 as Int8
                                (if x0 > 10 then
                                    b := x0
                                else if x0 < 5 then
                                    b := 0 as Int8
                                else
                                    b := 5 as Int8
                                if)
                          thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getTotalErrorCount() == 0 )
    }
    
    it should "reject non-boolean expressions for the guard" in {
        val code = """(class A(obj a1 : Int8)
                          obj x0 : Int8 := a1;
                          (thread
                                obj b : Int8 := -1 as Int8
                                (if x0 then
                                    b := x0
                                else if 4 then
                                    b := 0 as Int8
                                else
                                    b := 5 as Int8
                                if)
                          thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assertResult( 2 )( er.getFatalCount() )
        assertResult( 0 )( er.getWarningCount() )
        assertResult( "Guard expression must be boolean." )( er.getErrorText(0) )
        assertResult( "Guard expression must be boolean." )( er.getErrorText(1) )
    }
    
    behavior of "WhileCmdNd";
    // FIXME: (FAIL) Assertion failed "unreachable"
    it should "accept boolean expressions for the guard" in {
        val code = """(class A(obj a1 : Int8)
                          (thread
                              obj stop := a1
                              obj count := 0 as Int8
                              (while count < stop do
                                  count := count + 1
                              while)
                          thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getTotalErrorCount() == 0 )
    }
    
    // FIXME: (FAIL) Assertion failed "unreachable"
    it should "reject non-boolean expressions for the guard" in {
        val code = """(class A()
                          (thread
                              obj x : Int8 := 2
                              (while x do
                                  x := x + 1
                              while)
                          thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        printErrors( er )
        assertResult( 1 )( er.getFatalCount() )
        assertResult( 0 )( er.getWarningCount() )
        assertResult( "Guard expression must be boolean.")( er.getErrorText(0) )
    }
    
    behavior of "ForCmdNd";
    // FIXME: (FAIL) Assertion failed "todo"
    it should "accept arithmetic bounds" in {
        val code = """(class A()
                          (thread
                              obj x : Int8 := 10
                              obj y : Int8 := 0
                              (for i : x do
                                  y := y + i
                              for)
                          thread)
                      class A)
                      (class B()
                          (thread
                              obj z : Int8 := 0
                              (for j : 10 do
                                  z := z + j
                              for)
                          thread)
                      class B)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getTotalErrorCount() == 0 )    
    }
    
    // FIXME: (FAIL) Assertion failed "todo"
    it should "reject non-arithmetic bounds" in {
        val code = """(class A()
                          (thread
                              obj x : Bool := true
                              (for i : x do
                                  ;
                              for)
                          thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assertResult( 1 )( er.getFatalCount() )
        assertResult( 0 )( er.getWarningCount() )
        assertResult( "..." )( er.getErrorText(0) )
    }
        
    //TODO: The CoCmdNd and CoForCmdNd tests should be moved out as most of them aren't type checking tests.
    behavior of "CoCmdNd";
    it should "accept read access to the same variable in each block" in {
        val code = """(class A()
                          obj x : Int8 := 5
                          obj y : Int8 := 0
                          obj z : Int8 := 0
                          (thread
                              (co
                                  y := x
                              ||
                                  z := x
                              co)
                          thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getTotalErrorCount() == 0 )
    }
    
    it should "accept write access to different variables in each block" in {
        val code = """(class A()
                          obj x : Int8 := 5
                          obj y : Int8 := 5
                          (thread
                              (co
                                  x := 10 as Int8
                              ||
                                  y := 0 as Int8
                              co)
                          thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getTotalErrorCount() == 0 )
    }
    
    // FIXME: (FAIL) gets zero fatal errors - expected 1
    it should "reject write access to the same variable in each block" in {
        val code = """(class A()
                          obj x : Int8 := 5
                          (thread
                              (co
                                  x := 10 as Int8
                              ||
                                  x := 0 as Int8
                              co)
                          thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assertResult( 1 )( er.getFatalCount() )
        assertResult( 0 )( er.getWarningCount() )
        assertResult( "..." )( er.getErrorText(0) )
    }
    
    behavior of "CoForCmdNd";
    it should "???" in {
        // Bound should be compile time constant (no input? no variables? not sure what this means)
    }
    
    it should "accept setting all values in an array in parallel" in {
        val code = """(class A()
                          obj x : Int8[10] := (for i : 10 do i)
                          (thread
                              (co j : 10 do
                                  ;
                              co)
                          thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getTotalErrorCount() == 0 )
    }
    
    it should "reject setting a single variable in parallel with itself" in {
        val code = """(class A()
                          obj x : Int8 := 0
                          (thread
                              (co j : 10 do
                                  x := j
                              co)
                          thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assertResult( 1 )( er.getFatalCount() )
        assertResult( 0 )( er.getWarningCount() )
        assertResult( "..." )( er.getErrorText(0) )
    }

    behavior of "AcceptCmdNd";
    it should "accept bool expressions for the guard" in {
        val code = """(class A(obj x : Int8)
                          obj y : Int8 := x
                          public proc test(in a : Int8);
                          (thread
                              (accept test(in a : Int8) when y > 10
                                  ;
                              accept)
                          thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getTotalErrorCount() == 0 )
    }
    
    // FIXME: (FAIL) Assertion failed "unreachable"
    it should "reject non-bool expressions for the guard" in {
        val code = """(class A(obj x : Int8)
                          obj y : Int8 := x
                          public proc test(in a : Int8);
                          (thread
                              (accept test(in a : Int8) when y
                                  ;
                              accept)
                          thread)
                      class A)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assertResult( 1 )( er.getFatalCount() )
        assertResult( 0 )( er.getWarningCount() )
        assertResult( "Guard expression must be boolean." )( er.getErrorText(0) )
    }
    
    behavior of "WithCmdNd";
    // FIXME: (FAIL) Assertion failed "todo"
    it should "accept type Lock with no guard" in {
        val code = """(class Lock() class Lock)
                      (class B()
                          obj l : Lock := new Lock()
                          (thread
                              (with l do
                                  ;
                              with)
                          thread)
                      class B)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getTotalErrorCount() == 0 )
    }
    
    // FIXME: (FAIL) Assertion failed "unreachable"
    it should "accept bool expressions for the guard" in {
        val code = """(class Lock() class Lock)
                      (class B()
                          obj y : Bool := true
                          obj l : Lock := new Lock()
                          (thread
                              (with l when y do
                                  ;
                              with)
                          thread)
                      class B)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assert( er.getTotalErrorCount() == 0 )
    }
    
    // FIXME: (FAIL) Assertion failed "unreachable"
    it should "reject non-bool expressions for the guard" in {
        val code = """(class Lock() class Lock)
                      (class B()
                          obj y : Int8 := 5
                          obj l : Lock := new Lock()
                          (thread
                              (with l when y do
                                  ;
                              with)
                          thread)
                      class B)"""
        
        val result = checkCode( code )
        val dl = result._1
        val er = result._2
        
        assertResult( 1 )( er.getFatalCount() )
        assertResult( 0 )( er.getWarningCount() )
        assertResult( "..." )( er.getErrorText(0) )
    }
    
//---------------------------------------------------------------------------------------------------------
    def checkCode( str: String ) : ( DeclList, StandardErrorRecorder ) = {
        val er = new StandardErrorRecorder
        val reader = new StringReader( str )
        val p : HarpoParser = new HarpoParser( reader )
        val builder = new frontEnd.Builder( er )
        val checker = new Checker( er )

        p.setBuilder( builder )
        
        val dl : DeclList = 
            try { p.Start().asInstanceOf[DeclList] }
            catch { case ex : ParseException => {    
                        val coord = if( ex.currentToken != null ) Coord( "fileName", ex.currentToken.beginLine, ex.currentToken.beginColumn ) 
                                    else Coord( "fileName" ) ;
                        er.reportFatal( ex.getMessage(), coord )
                        null }
                    case ex : TokenMgrError => {    
                        val coord = Coord( "fileName" ) 
                        er.reportFatal( ex.getMessage(), coord )
                        null }
                  }
        
        if ( dl != null ) {    
          try { checker.runChecker( dl ) }
          catch { case ex : frontEnd.CompilerBailOutException => {} }
        }
        printErrors( er )

        ( dl, er )
    }
    
    def printErrors( er : StandardErrorRecorder ) {
        val num = er.getTotalErrorCount()
        if ( num > 0 ) {
            for ( i <- 0 to num - 1 ) {
                val coord = er.getErrorCoord(i)
                println( "Error on line " + coord.line + "(" + coord.col + "): " + er.getErrorText(i) )
            }
        }
    }
}