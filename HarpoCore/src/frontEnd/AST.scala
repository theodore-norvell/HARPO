package frontEnd;

import contracts.Contracts
import contracts.Contracts._
import util.Pretty
import scala.collection.mutable.ArrayBuffer
import scala.text.Document

import checker.CheckerTypes._

class AST {
    case class Coord( file: String, line: Int = 0, col: Int = 0 ) {
        override def toString = "File: "+file+" line: "+line+" column: "+col
    }

    val noCoord = new Coord( "unknown" )

    class DeclList extends Pretty {
        private var declarations = ArrayBuffer[ DeclNd ]()

        def findDeclaration( name: String ) =
            declarations.find { x => x.name == name }

        def addDeclaration( d: DeclNd ) {
            // Note that duplicate declarations are allowed (i.e. two with the same name).
            // These should be detected during the symbol table building pass.
            // Subsequent to that pass, we can count on unique names.
            declarations += d
        }
        
        def decls = declarations.toSet

        override def pp = Pretty.prettyList( declarations )
    }

    trait ClassLike extends Pretty {

        val coord: AST.Coord
        var fqn: FQN
        val errorRecorder: ErrorRecorder

        var tipe: Option[ Type ] = None;

        val genericParameters = ArrayBuffer[ GenericParamDeclNd ]()

        //TODO What about the bounds on the type variable?
        def addGeneric( name: String, ty: TypeNd, coord: AST.Coord ) {
            genericParameters += new GenericParamDeclNd( ty )( name, coord )
        }

        var superTypes = new ArrayBuffer[ TypeNd ]()

        def addSuperType( t: TypeNd ) {
            superTypes += t }

        // Deferred
        val constructorParams: Seq[ ParamDeclNd ]

        val name: String

        def checkName( name: String, coord: frontEnd.AST.Coord ) {
            if ( this.name != name ) errorRecorder.reportWarning( "Name does not match.", coord )
        }

        protected val members = new DeclList();

        def addMember( d: DeclNd ) { members.addDeclaration( d ) }

        def findMemberDirect( name: String ): Option[ DeclNd ] = members.findDeclaration( name )
        
        def findMemberIndirect( name: String ): Option[ DeclNd ] = {
          // TODO  Should there be more to this?
          members.findDeclaration( name )
        }
        
        def directMembers : Set[DeclNd] = members.decls

    }

    sealed abstract class DeclNd( val name: String, val coord: AST.Coord ) extends Pretty {
        var fqn: FQN = null
        var parent: Option[ DeclNd ] = null; // This should not be null after resolution.
        def setFQN( fqn: FQN ) { this.fqn = fqn }
    }

    sealed abstract class ClassLikeDeclNd( name: String,
                                           coord: AST.Coord )
        extends DeclNd( name, coord ) with ClassLike {}

    case class ClassDeclNd()( name: String,
                              coord: AST.Coord,
                              override val errorRecorder: ErrorRecorder )
        extends ClassLikeDeclNd( name, coord ) {
        override val constructorParams = ArrayBuffer[ ParamDeclNd ]()
        def addConstructorParam( d: ParamDeclNd ) { constructorParams += d }
        override def pp = Pretty.func( "ClassDeclNd", superTypes,
            genericParameters,
            constructorParams,
            members.decls.toSeq )
        //            Document.nest(3, ("class decl" :: name :: " super types:" :: Pretty.list( superTypes ) :/: 
        //	                                        "generic params " :: Pretty.list(genericParameters) :/:
        //	                                        "constructor params " :: Pretty.list(constructorParams) :/:
        //	                                        Pretty.stack( members.declarations ) ) )
    }

    case class IntfDeclNd()( name: String,
                             coord: AST.Coord,
                             override val errorRecorder: ErrorRecorder )
        extends ClassLikeDeclNd( name, coord ) {

        override val constructorParams = List()

        override def pp = Pretty.func( "IntfDeclNd", superTypes,
            genericParameters,
            members.decls.toSeq )
    }

    case class ObjDeclNd( isConst: Boolean, acc: Access, ty: TypeNd, var init: InitExpNd )( name: String, coord: AST.Coord )
        extends DeclNd( name, coord ) {

        override def pp = Pretty.func( "ObjDeclNd[" :: name :: "]", ty, init )
    }

    case class ParamDeclNd( ty: TypeNd, paramCategory: ParamCategory )( name: String, coord: AST.Coord )
        extends DeclNd( name, coord ) {

        override def pp = Pretty.func( "ParamDeclNd[" :: name :: "]", ty, paramCategory.toString )
    }

    case class MethodDeclNd( acc: Access, params: List[ ParamDeclNd ] )( name: String, coord: AST.Coord )
        extends DeclNd( name, coord ) {
        var tipe: Option[ MethodType ] = None;
        override def pp = {
            val ppp = Document.nest( 3, "MethodDeclNd(" :: acc.toString
                :: ", " :: params :: Document.text( ")" ) )
            tipe match {
                case Some( ty ) => Document.group( ppp :/: ": " :: ty.pp )
                case None => Document.group( ppp :/: ": NONE" )
            }
        }
    }

    case class ThreadDeclNd( block: CommandNd )( name: String, coord: AST.Coord )
        extends DeclNd( name, coord ) {
        override def pp = Pretty.func( "ThreadDeclNd[" :: name :: "]", block )
    }

    case class LocalDeclNd( isConst: Boolean, ty: TypeNd, var init: ExpNd, cmd: CommandNd )( name: String, coord: AST.Coord )
        extends DeclNd( name, coord ) {
        override def pp = Pretty.func( "LocalDeclNd[" :: name :: "]", isConst.toString, ty, init, cmd )
    }

    case class GenericParamDeclNd( ty: TypeNd )( name: String, coord: AST.Coord )
        extends DeclNd( name, coord ) {
        override def pp = Pretty.func( "GenericParamDeclNd[" :: name :: "]", ty )
    }

    // PrimitiveTypeDeclNd is used in the symbol table so that primitive types map to something.
    case class PrimitiveTypeDeclNd( qn: FQN )( coord: AST.Coord )
        extends DeclNd( qn.last, coord ) {
        fqn = qn // Ugly!
        override def pp = Pretty.func( "PrimitiveTypeDeclNd", fqn.toString )
    }

    // The variable in a for loop, co loop, or array initialization.
    // Invariant: The parent of a ForVarDecl must be a ForDecl.
    case class ForVarDecl()( name: String, coord: AST.Coord )
        extends DeclNd( name, coord ) {
        override def pp = Pretty.func( "ForVarDecl[" :: name :: "]" )
    }

    // A for loop, co loop, or array initialization.
    // These get their own FQN and so have their own "dummy" declaration
    // The name is some string that can not conflict with a user defined name
    case class ForDecl( fvd: ForVarDecl )( name: String, coord: AST.Coord )
        extends DeclNd( name, coord ) {
        override def pp = Pretty.func( "ForDecl[" :: name :: "]" )
    }

    // Method implementations are kind of odd as they are both declarations
    // and references to other declarations.
    case class MethodImplementationDeclNd( nameNd: NameNd,
                                           paramList: List[ ParamDeclNd ],
                                           var guard: ExpNd,
                                           fstCmd: CommandNd,
                                           sndCmd: CommandNd )( name: String, coord: AST.Coord )
        extends DeclNd( name, coord ) {
        override def pp = {
            val ppp = Pretty.func( "MethodImplementationDeclNd",
                name,
                paramList,
                guard,
                fstCmd,
                sndCmd )
            tipe match {
                case Some( ty ) => Document.group( ppp :/: ": " :: ty.pp )
                case None => Document.group( ppp :/: ": NONE" )
            }
        }

        // The tipe field is derived from the paramList.
        var tipe: Option[ MethodType ] = None
    }

    /****************/
    /** Statements **/
    /****************/

    abstract sealed class CommandNd( val coord: AST.Coord ) extends Pretty

    case class SkipCmdNd()( coord: AST.Coord ) extends CommandNd( coord ) {
        override def pp = Pretty.func( "SkipCmdNd" )
    }

    case class SeqCommandNd( fstCmd: CommandNd, sndCmd: CommandNd )( coord: AST.Coord )
        extends CommandNd( coord ) {
        override def pp = Pretty.func( "SeqCommandNd", fstCmd, sndCmd )
    }

    case class LocalDeclCmdNd( decl: LocalDeclNd )( coord: AST.Coord )
        extends CommandNd( coord ) {
        override def pp = Pretty.func( "LocalDeclCmdNd", decl )
    }

    case class AssignmentCmdNd( lhs: Seq[ ExpNd ], var rhs: Seq[ ExpNd ] )( coord: AST.Coord )
        extends CommandNd( coord ) {
        override def pp = Pretty.func( "AssignmentCmdNd", lhs, rhs )
    }

    case class CallCmdNd( method: ExpNd, var argList: List[ ExpNd ] )( coord: AST.Coord )
        extends CommandNd( coord ) {
        override def pp = Pretty.func( "CallCmdNd", method, argList )
    }

    case class IfCmdNd( var guard: ExpNd, thenCmd: CommandNd, elseCmd: CommandNd )( coord: AST.Coord )
        extends CommandNd( coord ) {
        override def pp = Pretty.func( "IfCmdNd", guard, thenCmd, elseCmd )
    }

    case class WhileCmdNd( var guard: ExpNd, body: CommandNd )( coord: AST.Coord )
        extends CommandNd( coord ) {
        override def pp = Pretty.func( "WhileCmdNd", guard, body )
    }

    case class ForCmdNd( decl: ForDecl, repetitions: ExpNd, body: CommandNd )( coord: AST.Coord )
        extends CommandNd( coord ) {
        override def pp = Pretty.func( "ForCmdNd", decl, body )
    }

    case class CoForCmdNd( decl: ForDecl, repetitions: ExpNd, body: CommandNd )( coord: AST.Coord )
        extends CommandNd( coord ) {
        override def pp = Pretty.func( "CoForCmdNd", decl, body )
    }

    case class CoCmdNd( fstCmd: CommandNd, sndCmd: CommandNd )( coord: AST.Coord )
        extends CommandNd( coord ) {
        override def pp = Pretty.func( "CoCmdNd", fstCmd, sndCmd )
    }

    case class AcceptCmdNd( methodImplementationList: List[ MethodImplementationDeclNd ] )( coord: AST.Coord )
        extends CommandNd( coord ) {
        override def pp = Pretty.func( "AcceptCmdNd", methodImplementationList )
    }

    case class WithCmdNd( lock: ExpNd, var guard: ExpNd, command: CommandNd )( coord: AST.Coord )
        extends CommandNd( coord ) {
        override def pp = Pretty.func( "WithCmdNd", lock, guard, command )
    }

    /************/
    /** Types **/
    /**********/
    // TODO override toString for all types.
    abstract sealed class TypeNd( val coord: AST.Coord ) extends Pretty {
        // The tipe field is set during checking.
        var tipe: Option[ Type ] = None;

        def ppp: Document

        override def pp = tipe match {
            case None => Document.group( ppp :/: ": NONE" )
            case Some( ty ) => Document.group( ppp :/: ": " :: ty.pp )
        }
    }

    case class NoTypeNd()( coord: AST.Coord ) extends TypeNd( coord ) {
        override def ppp = Pretty.func( "NoTypeNd" )
    }

    case class TopTypeNd()( coord: AST.Coord ) extends TypeNd( coord ) {
        override def ppp = Pretty.func( "TopTypeNd" )
    }

    case class NamedTypeNd( name: NameNd )( coord: AST.Coord ) extends TypeNd( coord ) {

        override def toString = name.toString

        override def ppp = Pretty.func( "NamedTypeNd", name )
    }

    case class ArrayTypeNd( baseType: TypeNd, bound: ExpNd )( coord: AST.Coord ) extends TypeNd( coord ) {
        override def ppp = Pretty.func( "ArrayTypeNd", baseType, bound )
    }

    case class SpecializedTypeNd( genericType: TypeNd, ga: List[ TypeNd ] )( coord: AST.Coord ) extends TypeNd( coord ) {
        override def ppp = Pretty.func( "SpecializedTypeNd", genericType, ga )
    }

    /*******************/
    /** Expressions  **/
    /*****************/

    abstract sealed class ExpNd( val coord: AST.Coord ) extends Pretty {
        // The tipe field is set during checking.
        var tipe: Option[ Type ] = None;

        def ppp: Document

        override def pp = tipe match {
            case None => Document.group( ppp :/: ": NONE" )
            case Some( ty ) => Document.group( ppp :/: ": " :: ty.pp )
        }
    }

    case class NoExpNd()( coord: AST.Coord ) extends ExpNd( coord ) {
        override def ppp = Pretty.func( "NoExpNd" )
    }

    case class IntLiteralExpNd( i: Long )( coord: AST.Coord ) extends ExpNd( coord ) {
        override def ppp = Pretty.func( "IntLiteralExpNd", i.toString )
    }

    case class FloatLiteralExpNd( x: Double )( coord: AST.Coord ) extends ExpNd( coord ) {
        override def ppp = Pretty.func( "FloatLiteralExpNd", x.toString )
    }

    case class NameExpNd( name: NameNd )( coord: AST.Coord ) extends ExpNd( coord ) {
        override def ppp = Pretty.func( "NameExpNd", name )
    }

    case class BinaryOpExpNd( op: BinaryOperator, var x: ExpNd, var y: ExpNd )( coord: AST.Coord ) extends ExpNd( coord ) {
        override def ppp = Pretty.func( "BinaryOpExpNd", op.toString, x, y )
    }

    case class UnaryOpExpNd( op: UnaryOperator, var x: ExpNd )( coord: AST.Coord ) extends ExpNd( coord ) {
        override def ppp = Pretty.func( "UnaryOpExpNd", op.toString, x )
    }

    case class AsExpNd( x: ExpNd, ty: TypeNd )( coord: AST.Coord ) extends ExpNd( coord ) {
        override def ppp = Pretty.func( "AsExpNd", x, ty )
    }

    case class MemberExpNd( x: ExpNd, name: String )( coord: AST.Coord ) extends ExpNd( coord ) {
        override def ppp = Pretty.func( "MemberExpNd", x, name )
    }

    case class ChainExpNd( ops: List[ ChainingOperator ], var operands: List[ ExpNd ] )( coord: AST.Coord ) extends ExpNd( coord ) {
        override def ppp = Pretty.func( "ChainExpNd",
            Pretty.list( ops map { x => Document.text( x.toString ) } ),
            operands )
    }

    case class FetchExpNd( x: ExpNd )( coord: AST.Coord ) extends ExpNd( coord ) {
        override def ppp = Pretty.func( "FetchExpNd", x )
    }

    /*******************/
    /** Operators    **/
    /*****************/

    sealed abstract class BinaryOperator
    case object ImpliesOp extends BinaryOperator
    case object EquivOp extends BinaryOperator
    case object OrOp extends BinaryOperator
    case object AndOp extends BinaryOperator
    case object AddOp extends BinaryOperator
    case object SubOp extends BinaryOperator
    case object MulOp extends BinaryOperator
    case object SlashDivOp extends BinaryOperator
    case object WordDivOp extends BinaryOperator
    case object RemOp extends BinaryOperator
    case object IndexOp extends BinaryOperator

    sealed abstract class ChainingOperator
    case object EqualOp extends ChainingOperator
    case object NotEqualOp extends ChainingOperator
    case object LessOp extends ChainingOperator
    case object LessOrEqualOp extends ChainingOperator
    case object GreaterOp extends ChainingOperator
    case object GreaterOrEqualOp extends ChainingOperator

    sealed abstract class UnaryOperator
    case object NegativeOp extends UnaryOperator
    case object NotOp extends UnaryOperator

    /*********************************/
    /** Initialization Expressions  **/
    /*********************************/

    abstract sealed class InitExpNd( val coord: AST.Coord ) extends Pretty {
        // The tipe field is set during checking.
        var tipe: Option[ Type ] = None

        def ppp = Document.text( toString )

        override def pp = tipe match {
            case None => Document.group( ppp :/: ": NONE" )
            case Some( ty ) => Document.group( ppp :/: ": " :: ty.pp )
        }
    }

    case class ValueInitExpNd( var exp: ExpNd )( coord: AST.Coord ) extends InitExpNd( coord ) {
        override def ppp = Pretty.func( "ValueInitExpNd", exp )
    }

    case class NewInitExpNd( ty: TypeNd, args: List[ ExpNd ] )( coord: AST.Coord ) extends InitExpNd( coord ) {
        override def ppp = Pretty.func( "NewInitExpNd", ty, args )
    }

    case class ArrayInitExpNd( decl: ForDecl, bound: ExpNd, a: InitExpNd )( coord: AST.Coord ) extends InitExpNd( coord ) {
        override def ppp = Pretty.func( "ArrayInitExpNd", decl, bound, a )
    }

    case class IfInitExpNd( var guard: ExpNd, a: InitExpNd, b: InitExpNd )( coord: AST.Coord ) extends InitExpNd( coord ) {
        override def ppp = Pretty.func( "IfInitExpNd", guard, a, b )
    }

    case class WidenInitExpNd( a: InitExpNd )( coord: AST.Coord ) extends InitExpNd( coord ) {
        override def ppp = Pretty.func( "WidenInitExpNd", a )
    }

    /*********************/
    /** Accessability   **/
    /*********************/

    abstract sealed class Access;
    case object PrivateAccess extends Access;
    case object PublicAccess extends Access;

    /**************************/
    /** Parameter Categories **/
    /**************************/

    abstract sealed class ParamCategory;
    case object InParamCategory extends ParamCategory;
    case object OutParamCategory extends ParamCategory;
    case object ObjParamCategory extends ParamCategory;

    /************/
    /** Names  **/
    /************/

    sealed case class NameNd( qn: QN )( val coord: AST.Coord ) extends Pretty {
        var decl: Option[ DeclNd ] = None // Set during the resolution phase.
        override def toString = qn.toString
    }
}

object AST extends AST 