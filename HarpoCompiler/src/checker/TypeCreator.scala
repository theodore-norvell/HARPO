package checker

import frontEnd.AST._
import frontEnd.FQN
import frontEnd.ErrorRecorder
import CheckerTypes._
import contracts.Contracts._
import frontEnd.AST.PreCndNd
import frontEnd.AST.PostCndNd
import frontEnd.AST.ExpNd
class TypeCreator( errorRecorder : ErrorRecorder ) {
    
    private def makeMethodTipe(params : List[ParamDeclNd]) : Option[MethodType]= {
        for( p <- params ) {
            if ( p.ty.tipe == None ) 
                return None 
            else if( p.paramCategory != InParamCategory && p.paramCategory != OutParamCategory ) {
                errorRecorder.reportFatal("Parameters must be 'in' or 'out'.", p.coord ) 
                return None }
            else if( ! p.ty.tipe.get.isInstanceOf[LocationType] ) {
                errorRecorder.reportFatal( "Parameters must have primitive types", p.coord) 
                return None }
        }
        val parameters = for( p <- params ) 
                         yield Parameter( p.fqn, p.coord, p.ty.tipe.get, p.paramCategory )
        Some( MethodType( parameters ) )
    }
                  
    
    def extractTypeFromTypeNode( ty : TypeNd ) : Type = {
        ty match {
            case NamedTypeNd( name : NameNd ) =>
                val decl = name.decl.getOrElse {
                    unreachable( "Type name not resolved by type creation phase") }
                decl match {
                    case decl : ClassLikeDeclNd =>
                        var result : Type = ClassIntfType( decl ) ;
                        // If any generic parameters, then this is a universal type.
                        for( genParamDecl <- decl.genericParameters.reverse ) {
                            val t = extractTypeFromTypeNode( genParamDecl.ty )
                            val typeVar = TypeVar( genParamDecl.fqn, t)
                            result = UniversalType( typeVar, result) ;
                        }
                        result
                    case decl : GenericParamDeclNd =>
                        val t = extractTypeFromTypeNode( decl.ty )
                        TypeVar( decl.fqn, t)
                    case decl : PrimitiveTypeDeclNd => {
                        decl.fqn match {
                            case `int8FQN` => int8 
                            case `int16FQN` => int16
                            case `int32FQN` => int32
                            case `int64FQN` => int64
                            case `real16FQN` => real16
                            case `real32FQN` => real32
                            case `real64FQN` => real64
                            case `boolFQN` => bool } }
                    case _ => errorRecorder.reportFatal(
                                "Expected the name of a type, but found " +name+ ".",
                                ty.coord )
                        errorRecorder.bailOut() }
            case TopTypeNd() =>
                TopType()
            case SpecializedTypeNd( genericTypeNd, ga ) =>
                // TODO. Somewhere this well formedness of this application needs to be checked.
                val genType = extractTypeFromTypeNode( genericTypeNd )
                val genArgs = ga map (extractTypeFromTypeNode( _ ))
                var result = genType
                for( arg <- genArgs ) result = AppliedType( result, arg )
                result
            case ArrayTypeNd( baseType, bound ) =>
                val base = extractTypeFromTypeNode(baseType) 
                ArrayType(base,bound)
                if( isIntegralConstant( bound ) ) {
                    ArrayType( base, bound) }
                else {
                    errorRecorder.reportFatal(
                                    "Array size must be an integral constant",
                                    ty.coord )
                    errorRecorder.bailOut() }
            case NoTypeNd() => { assert(false, "unreachable") ; errorRecorder.bailOut() }            
        }
        
    }
    
    private def isIntegralConstant( x : ExpNd ) :  Boolean = {
    x.tipe match {
      case Some(LocationType(base)) =>
        base.name.toString() match {
          case "Int8" => true
          case "Int16" => true
          case "Int32" => true
          case "Int64" => true
          case _ =>  false
        }
      case _ => false
    }     
    }
    /** Create types and associate each TypeNd (apart from NoTypeNd) with a type.
     *  
     *  For each TypeNd within a set of declarations, set its tipe field to a Type object.
     */
    def createTypes( decls : DeclList) {
        
        def createTypeFromTypeNd( typeNd : TypeNd ) {
            val tipe = extractTypeFromTypeNode(typeNd)
            typeNd.tipe = Some(tipe) ;
        }
        
        def createTypesFromDeclList( decls : DeclList ) {
            for ( decl <- decls.decls ) createTypesFromDecl( decl )
        }
        
        def createTypesFromDeclSet( decls : List[DeclNd] ) {
            for ( decl <- decls ) createTypesFromDecl( decl )
        }

        def createTypesFromDecl( decl : DeclNd  ) {
            
            def promoteToLoc( ty : TypeNd ) {    
                check( ty.tipe != None )
                ty.tipe match {
                    case Some(tipe@PrimitiveType( _ )) =>
                        ty.tipe = Some( LocationType( tipe ) )
                    case _ => {} };}
            
            decl match {
                case d : ClassLike =>
                    //for( gp <- d.genericParameters ) createTypesFromDecl( gp ) 
                    for( typeNd <- d.superTypes ) createTypeFromTypeNd( typeNd ) 
                    for( p <- d.constructorParams ) createTypesFromDecl( p ) 
                    createTypesFromDeclSet( d.directMembers )
                    
                case ObjDeclNd(isGhost:Boolean, isConst : Boolean, acc : Access, ty : TypeNd, init : InitExpNd) =>
                    ty match { case NoTypeNd() => {}
                               case _ => {
                                    createTypeFromTypeNd( ty ) 
                                    promoteToLoc(ty) } }
                case ClaimNd(lpmn: PermissionMapNd) => {}

                case ClassInvNd(exp: ExpNd) =>  {}
                case ParamDeclNd(isGhost,ty , _) =>
                    createTypeFromTypeNd( ty ) 
                    promoteToLoc(ty)
                    check( ty.tipe != None)
                    
                case declNd@MethodDeclNd( _, params, preCndList: List[PreCndNd], postCndList: List[PostCndNd], givesPerList: List[GivesPerNd], takesPerList: List[TakesPerNd], borrowsPerList: List[BorrowsPerNd])  => { 
                  
                  for( p <- params ) createTypesFromDecl( p ) 
                  declNd.tipe = makeMethodTipe(params)
                  
                  for( pre <- preCndList ) {}
                  
                  for( post <- postCndList ) {}
                  
                  for( gives <- givesPerList ) {}
                  
                  for( takes <- takesPerList ) {}
                  
                  for( borrows <- borrowsPerList ) {}
                  
                }   
                case thread : ThreadDeclNd =>
                    createTypesFromCommand( thread.block )
                case LocalDeclNd(_, _, ty, init, stmt ) =>
                    createTypesFromCommand( stmt )
                    ty match { case NoTypeNd() => {} // Do it later.
                               case _ => {
                                    createTypeFromTypeNd( ty ) 
                                    promoteToLoc(ty)
                                    check( ty.tipe != None)
                                    errorRecorder.checkFatal( ty.tipe.get.isInstanceOf[LocationType],
                                            "Local variables must have primitive types", ty.coord) } }
                    
                case GenericParamDeclNd( ty : TypeNd) =>
                    createTypeFromTypeNd( ty ) 
               
                case PrimitiveTypeDeclNd( qn : FQN) => {}
                
                case ForDecl( fvd ) => createTypesFromDecl( fvd )
                
                case ForVarDecl() => {}
                
                case declNd@MethodImplementationDeclNd( nameNd : NameNd,
                                       paramList : List[ParamDeclNd],
                                       guard : ExpNd,
                                       fstCmd : CommandNd,
                                       sndCmd : CommandNd ) => {
                        for( p <- paramList ) createTypesFromDecl( p ) 
                        declNd.tipe = makeMethodTipe( paramList )
                        createTypesFromCommand( fstCmd  )
                        createTypesFromCommand( sndCmd  ) }
            }
        }

        def createTypesFromCommand( command : CommandNd ) {
            command match {
                case SeqCommandNd( fstCmd, sndCmd ) =>
                    createTypesFromCommand( fstCmd )
                    createTypesFromCommand( sndCmd  )
                case LocalDeclCmdNd( decl ) =>
                    createTypesFromDecl( decl  )
                case IfCmdNd( guard, thenCmd, elseCmd ) =>
                    createTypesFromCommand( thenCmd  )
                    createTypesFromCommand( elseCmd  )
                case WhileCmdNd( guard,lil, body ) => 
                    createTypesFromCommand( body  )
                case ForCmdNd( forDecl, repetitions,lil, body ) =>
                    createTypesFromCommand( body  )
                case CoForCmdNd( forDecl, repetitions,cl, body ) =>
                    createTypesFromCommand( body  )
                case CoCmdNd( cl, fstCmd, sndCmd ) =>
                    createTypesFromCommand( fstCmd  )
                    createTypesFromCommand( sndCmd  )
                case AcceptCmdNd( methodImplementationList ) =>
                    for( methImpl <- methodImplementationList )
                        createTypesFromDecl( methImpl )
                case WithCmdNd( lock, tpl,  guard, command, gpl ) =>
                    createTypesFromCommand( command  )
                case _ => ()
            }
        }
        createTypesFromDeclList( decls )
        
    }
}