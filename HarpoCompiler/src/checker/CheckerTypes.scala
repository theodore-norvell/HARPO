package checker

import scala.collection.immutable._
import scala.collection.mutable.HashMap
import frontEnd.AST
import frontEnd.FQN
import frontEnd.ErrorRecorder 
import contracts.Contracts
import util.Pretty
import frontEnd.AST.InParamCategory
import frontEnd.AST.OutParamCategory

object CheckerTypes extends Contracts {

    /////////////////////////
    // Class environments 
    //////////////////////////

    type ClassEnvironment = Map[ FQN, ClassIntfDescription ];

    sealed case class ClassIntfDescription(
        typeVars: List[ TypeVar ],
        declarations: Typothesis,
        superTypes: List[ ClassIntfType ] );

    /////////////////////////////
    // Types                   //
    /////////////////////////////

    /** Type --> ClassIntfType | PrimitiveType | UniversalType( var, Type) | AppliedType( Type, Type)
      *       | ArrayType( Type,  Bound) |
      *       | TopType | MethodType( Params ) | TypeVar | LocationType( Type )
      */
    sealed abstract class Type extends Pretty

    /** The type of an object is either a class or interface type.*/
    case class ClassIntfType( decl : AST.ClassLikeDeclNd ) extends Type {
        override def toString = decl.fqn.toString
        
        def isClass = decl.isInstanceOf[AST.ClassDeclNd]
        
        def directMembers : List[AST.DeclNd] = decl.directMembers
        
        var ancstrs : Set[ClassIntfType] = null
        
        def computeAncestors( er : ErrorRecorder ) {
            computeAncestors1( er, Set() )
        }
        
        private def computeAncestors1( er : ErrorRecorder,  s : Set[ClassIntfType] ) {
        	if( ancstrs == null ) {
                ancstrs = Set(this)
                if( s.contains(this) )
            		for( tyNd <- decl.superTypes) {
            			val ty : Type = tyNd.tipe.getOrElse( unreachable("Type not set.") )
            			if( ty.isInstanceOf[ClassIntfType] ) {
                            val ty1 = ty.asInstanceOf[ClassIntfType]
        		    		computeAncestors1(er, s union Set(this) )
                            ancstrs = ancstrs union ty1.ancestors } }
                else
                    er.reportFatal( "Circular inheritance detected.", this.decl.coord )
        	}
        }
        
        def ancestors : Set[ClassIntfType] =  {
            check( ancstrs != null, "Ancestors not computed yet." )
            ancstrs }
        
        def properAncestors : Set[ClassIntfType] =  {
            ancestors - this }
        
        def allMembers : Set[AST.DeclNd] = {
            ancestors.map( _.directMembers.toSet ).fold( Set.empty )( _.union(_) ) 
        }
    }

    case class PrimitiveType( name: FQN )( val index: Int ) extends Type {
        override def toString = name.toString
    }

    case class UniversalType( typeVar: TypeVar, body: Type ) extends Type {
        override def toString = s"(For all ${typeVar} in ${body})"
    }

    case class AppliedType( typeFunc: Type, typeArg: Type ) extends Type {
        override def toString = s"${typeFunc}{${typeArg}}"
    }

    case class ArrayType( base: Type, bound: AST.ExpNd ) extends Type {
        override def toString = s"${base}[]"
    }

    case class TopType() extends Type {
        override def toString = "Top"
    }

    case class MethodType( paramList: List[ Parameter ] ) extends Type {
        override def toString = s"method ${paramList}"
        
        // Invariant: Parameters all represent locations and are in or out.
        for( p <- paramList ) {
            Contracts.check(p.direction == InParamCategory || p.direction == OutParamCategory )
            Contracts.check(p.ty.isInstanceOf[LocationType]) }
    }

    case class TypeVar( name: FQN, bound: Type ) extends Type {
        override def toString = s"${name} extends ${bound}"
    }

    case class LocationType( base: PrimitiveType ) extends Type {
        override def toString = s"loc{${base}}"
    }

    val int8FQN = new FQN( "Int8" )

    val int8 = PrimitiveType( int8FQN )( 0 )
    
    val int8Nd = AST.NamedTypeNd( AST.NameNd(int8FQN)( AST.noCoord ) )( AST.noCoord ) 

    val int16FQN = new FQN( "Int16" )

    val int16 = PrimitiveType( int16FQN )( 1 )
    
    val int16Nd = AST.NamedTypeNd( AST.NameNd(int16FQN)( AST.noCoord ) )( AST.noCoord ) 

    val int32FQN = new FQN( "Int32" )

    val int32 = PrimitiveType( int32FQN )( 2 )
    
    val int32Nd = AST.NamedTypeNd( AST.NameNd(int32FQN)( AST.noCoord ) )( AST.noCoord ) 

    val int64FQN = new FQN( "Int64" )

    val int64 = PrimitiveType( int64FQN )( 3 )
    
    val int64Nd = AST.NamedTypeNd( AST.NameNd(int64FQN)( AST.noCoord ) )( AST.noCoord ) 

    val real16FQN = new FQN( "Real16" )

    val real16 = PrimitiveType( real16FQN )( 4 )
    
    val real16Nd = AST.NamedTypeNd( AST.NameNd(real16FQN)( AST.noCoord ) )( AST.noCoord ) 

    val real32FQN = new FQN( "Real32" )

    val real32 = PrimitiveType( real32FQN )( 5 )
    
    val real32Nd = AST.NamedTypeNd( AST.NameNd(real32FQN)( AST.noCoord ) )( AST.noCoord ) 

    val real64FQN = new FQN( "Real64" )

    val real64 = PrimitiveType( real64FQN )( 6 )
    
    val real64Nd = AST.NamedTypeNd( AST.NameNd(real64FQN)( AST.noCoord ) )( AST.noCoord ) 

    val boolFQN = new FQN( "Bool" )

    val bool = PrimitiveType( boolFQN )( 7 )
    
    val boolNd = AST.NamedTypeNd( AST.NameNd(boolFQN)( AST.noCoord ) )( AST.noCoord ) 

    val trueFQN = new FQN( "true" )

    val falseFQN = new FQN( "false" )
    
    def nodeFor( ty : Type ) : AST.TypeNd = {
        val nd = ty match {
            case `int8` => int8Nd
            case `int16` => int16Nd 
            case `int32` => int32Nd 
            case `int64` => int64Nd
            case `real16` => real16Nd 
            case `real32` => real32Nd 
            case `real64` => real64Nd 
            case `bool` => boolNd
            case _ => Contracts.unreachable("in nodeFor") }
        nd.tipe = Some(ty)
        nd }

    def isPrimitiveType( tipe: Type ) = tipe match {
        case PrimitiveType( _ ) => true
        case _ => false
    }

    type Typothesis = ListMap[ FQN, Type ];

    sealed case class Parameter(
        name: FQN,
        coord: AST.Coord,
        ty: Type,
        direction: AST.ParamCategory )
}