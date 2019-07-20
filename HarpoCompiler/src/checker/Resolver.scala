package checker

import frontEnd.AST._ 
import frontEnd.FQN
import frontEnd.ErrorRecorder
import contracts.Contracts
import CheckerTypes._
import frontEnd.AST.PreCndNd;
import frontEnd.AST.PostCndNd;
import frontEnd.AST.GivesPerNd;
import frontEnd.AST.TakesPerNd;
import frontEnd.AST.BorrowsPerNd;

/* The goal of the resolver is to hunt down all NameNds and link them to the 
 * corresponding declaration.  There are a few other types of nodes that
 * also need to be linked to their declarations.
 */

private class Resolver( val errorRecorder : ErrorRecorder, symTab : SymbolTable )
extends Contracts {
 
    def resolve( decls : DeclList ) {

      /* Option type is a container for zero of one element of given type*/
        def resolveDeclList( decls : DeclList, containingFQN : FQN, containingDecl : Option[DeclNd] ) {
            for ( decl <- decls.decls )
              resolveDecl( decl, containingFQN, containingDecl )
        }

        def resolveDeclSet( decls : List[DeclNd], containingFQN : FQN, containingDecl : Option[DeclNd] ) {
            for ( decl <- decls )
              resolveDecl( decl, containingFQN, containingDecl )
        }

        def resolveDecl( decl : DeclNd, containingFQN : FQN, containingDecl : Option[DeclNd] ) {
            check(  containingDecl == None && containingFQN.names.length == 0 
                ||  containingDecl.isDefined && containingDecl.get.fqn == containingFQN )
                
            val fqn = containingFQN.append( decl.name )
            decl.parent = containingDecl
            decl match {
                case d : ClassDeclNd =>
                    resolveClassLike(d, containingFQN, containingDecl) 
                case d : IntfDeclNd =>
                    resolveClassLike(d, containingFQN, containingDecl) 
                case ClaimNd(pmn) => {
                  // TODO Compare Length - toDo("LocSetNd list and ExpNd list's lengths varies")
                  if (pmn.locExp.length == pmn.locSet.length){
                  for(lsn <- pmn.locSet) { resolveLocSetNd(lsn,containingFQN, containingDecl)}
                  for(len <- pmn.locExp) { resolvePermValue(len,containingFQN,containingDecl)}                  
                  }
                }
                case ClassInvNd(exp) =>
                  resolveExp(exp,containingFQN,containingDecl)
                case ObjDeclNd( isGhost:Boolean,isConst : Boolean, acc : Access, ty : TypeNd, init : InitExpNd) =>
                    resolveType( ty, containingFQN, containingDecl )
                    resolveInitExp( init, containingFQN, containingDecl )
                case ParamDeclNd(isGhost: Boolean, ty : TypeNd, paramCategory : ParamCategory) =>
                    resolveType( ty, containingFQN, containingDecl )
                case MethodDeclNd( acc : Access, params : List[ParamDeclNd], preCndList: List[PreCndNd], postCndList: List[PostCndNd], givesPerList: List[GivesPerNd], takesPerList: List[TakesPerNd], borrowsPerList: List[BorrowsPerNd]) =>
                    for( pdn <- params ) resolveDecl( pdn, fqn, Some(decl))
                    for( precn <- preCndList ) resolveMethodSpec( precn, fqn, Some(decl))
                    for( postcn <- postCndList ) resolveMethodSpec( postcn, fqn, Some(decl))
                    for( givesper <- givesPerList ) resolvePermissionNd( givesper, fqn, Some(decl))
                    for( takesper <- takesPerList ) resolvePermissionNd( takesper, fqn, Some(decl))
                    for( borrowsper <- borrowsPerList ) resolvePermissionNd( borrowsper, fqn, Some(decl))
                case ThreadDeclNd(claimList: List[ClaimNd], block : CommandNd) =>
                    {
                      resolveCommand( block, fqn, Some(decl))
                     for(clmNd <- claimList){ 
                       if (clmNd.pmn.locExp.length == clmNd.pmn.locSet.length){
                       for(len <- clmNd.pmn.locExp) { resolvePermValue(len,containingFQN,containingDecl)}
                       for(lsn <- clmNd.pmn.locSet) { resolveLocSetNd(lsn,fqn, Some(decl))}
                      }
                     }
                }
                case LocalDeclNd(isGhost, isConst, ty, init, stmt ) =>
                    resolveType( ty, containingFQN, containingDecl ) 
                    resolveExp( init, containingFQN, containingDecl )
                    resolveCommand( stmt, fqn, Some(decl) )
                case GenericParamDeclNd( ty : TypeNd) =>
                    resolveType( ty, containingFQN, containingDecl )
                case ForDecl(fvd) => resolveDecl(fvd,containingFQN, containingDecl)
                case ForVarDecl() => ()
                case MethodImplementationDeclNd( nameNd : NameNd,
                                       paramList : List[ParamDeclNd],
                                       guard : ExpNd,
                                       fstCmd : CommandNd,
                                       sndCmd : CommandNd ) => {
                        nameNd.decl = symTab.lookUpMember( containingFQN, nameNd.qn.last, nameNd.coord)
                        for( pdn <- paramList ) resolveDecl( pdn, fqn, Some(decl) )
                        resolveExp( guard, containingFQN, containingDecl )
                        resolveCommand( fstCmd, fqn, Some(decl) )
                        resolveCommand( sndCmd, containingFQN, containingDecl ) }
                case PrimitiveTypeDeclNd( qn : FQN) => {}
            }
        }
        
        def resolveClassLike( d : ClassLikeDeclNd, containingFQN : FQN, containingDecl : Option[DeclNd] ) {
          
            check(  containingDecl == None && containingFQN.names.length == 0 
                ||  containingDecl.isDefined && containingDecl.get.fqn == containingFQN )

            for( st <- d.superTypes ) 
                resolveType( st, containingFQN, containingDecl )
            //d.superTypeRoots = getAncestorsDeclNds(d) 
            for( gp <- d.genericParameters ) 
                resolveType( gp.ty, containingFQN, containingDecl ) 
            for( p <- d.constructorParams ) 
                resolveType( p.ty, containingFQN, containingDecl ) 
            resolveDeclSet( d.directMembers, containingFQN.append( d.name ), Some(d) ) 
        }
        
        def resolveType( ty : TypeNd, containingFQN : FQN, containingDecl : Option[DeclNd] ){ 
            check(  containingDecl == None && containingFQN.names.length == 0 
                ||  containingDecl.isDefined && containingDecl.get.fqn == containingFQN )

            ty match {
                case NoTypeNd() => {}
                case TopTypeNd() => {}
                case NamedTypeNd( name : NameNd ) =>
                    name.decl = symTab.lookUp( containingFQN, name )
                case ArrayTypeNd( baseType : TypeNd, bound : ExpNd ) =>
                    resolveType( baseType, containingFQN, containingDecl ) 
                    resolveExp( bound, containingFQN, containingDecl )
                case SpecializedTypeNd( genericType : TypeNd, ga : List[TypeNd] ) => 
                    resolveType( genericType, containingFQN, containingDecl ) 
                    for( arg <- ga ) resolveType( arg, containingFQN, containingDecl ) 
            }
        }
        
        def resolveInitExp( exp : InitExpNd, containingFQN : FQN, containingDecl : Option[DeclNd] ) {
            check(  containingDecl == None && containingFQN.names.length == 0 
                ||  containingDecl.isDefined && containingDecl.get.fqn == containingFQN )

            exp match {
                case ValueInitExpNd( exp : ExpNd ) =>
                    resolveExp( exp, containingFQN, containingDecl ) 
                case NewInitExpNd( ty : TypeNd, args : List[ExpNd] ) =>
                    resolveType( ty, containingFQN, containingDecl )
                    for( arg <- args ) resolveExp( arg, containingFQN, containingDecl )
                case ArrayInitExpNd( forDecl : ForDecl, bound : ExpNd, a : InitExpNd ) =>
                    val fvd = forDecl.fvd 
                    resolveExp( bound, containingFQN, containingDecl )
                    resolveInitExp( a, fvd.fqn, Some(fvd) ) 
                case IfInitExpNd( guard : ExpNd, a : InitExpNd, b : InitExpNd ) => 
                    resolveInitExp( a, containingFQN, containingDecl )
                    resolveInitExp( b, containingFQN, containingDecl )
                case WidenInitExpNd( exp ) =>
                    unreachable("WidenInitExpNd in resolver")
            }
        }

        def resolveMethodSpec( spec : MethodSpecNd, containingFQN : FQN, containingDecl : Option[DeclNd] ) {
            check(  containingDecl == None && containingFQN.names.length == 0 
                ||  containingDecl.isDefined && containingDecl.get.fqn == containingFQN )

            spec match {
                case PreCndNd (condition) =>
                    resolveExp(condition,containingFQN, containingDecl)
                    
                case PostCndNd (condition) =>
                    resolveExp(condition,containingFQN, containingDecl) 
            }
        }
        
        def resolvePermissionNd( pers : PermissionNd, containingFQN : FQN, containingDecl : Option[DeclNd] ) {
            check(  containingDecl == None && containingFQN.names.length == 0 
                ||  containingDecl.isDefined && containingDecl.get.fqn == containingFQN )

            pers match {             
                case GivesPerNd (pmn) => {
                  for(len <- pmn.locExp) { resolvePermValue(len,containingFQN,containingDecl)}
                  for(lsn <- pmn.locSet) { resolveLocSetNd(lsn,containingFQN, containingDecl)}
                 }
                case TakesPerNd (pmn) => {
                  for(len <- pmn.locExp) { resolvePermValue(len,containingFQN,containingDecl)}
                  for(lsn <- pmn.locSet) { resolveLocSetNd(lsn,containingFQN, containingDecl)}
                 }
                case BorrowsPerNd (pmn) => {
                  for(len <- pmn.locExp) { resolvePermValue(len,containingFQN,containingDecl)}
                  for(lsn <- pmn.locSet) { resolveLocSetNd(lsn,containingFQN, containingDecl)}
                 }
            }
        }
        def resolvePermValue (len: ExpNd, containingFQN: FQN, containingDecl: Option[DeclNd]){
          resolveExp(len,containingFQN,containingDecl)
        }
        def resolveLocSetNd (lsn: LocSetNd, containingFQN: FQN, containingDecl: Option[DeclNd])
        {
          lsn match { 
            case ObjectIdLSN(en) => {
              resolveExp(en, containingFQN, containingDecl)
            }
            case ArrayLSN(forDecl: ForDecl,offSet: ExpNd,bound: ExpNd, boundInclusive: Boolean ,locSet: LocSetNd) => {  
              val fvd = forDecl.fvd 
                    resolveLocSetNd( locSet, fvd.fqn, Some(fvd) )
                    resolveExp( offSet, containingFQN, containingDecl )
                    resolveExp( bound, containingFQN, containingDecl )                    
               }
        }
        }

        def resolveCommand( command : CommandNd, containingFQN : FQN, containingDecl : Option[DeclNd] ) {
            check(  containingDecl == None && containingFQN.names.length == 0 
                ||  containingDecl.isDefined && containingDecl.get.fqn == containingFQN )

            command match {
                case SkipCmdNd() => {} 
                
                case SeqCommandNd( fstCmd, sndCmd ) =>
                    resolveCommand( fstCmd, containingFQN, containingDecl )
                    resolveCommand( sndCmd, containingFQN, containingDecl )
                    
                case LocalDeclCmdNd( decl ) =>
                    resolveDecl( decl, containingFQN, containingDecl )
                    
                case AssignmentCmdNd( lhs, rhs ) =>
                    for( exp <- lhs ) resolveExp( exp, containingFQN, containingDecl )
                    for( exp <- rhs ) resolveExp( exp, containingFQN, containingDecl )
                    
                case CallCmdNd( method, argList ) =>
                    resolveExp( method, containingFQN, containingDecl )
                    for( arg <- argList ) resolveExp( arg, containingFQN, containingDecl )
                    
                case IfCmdNd( guard, thenCmd, elseCmd ) =>
                    resolveExp( guard, containingFQN, containingDecl )
                    resolveCommand( thenCmd, containingFQN, containingDecl )
                    resolveCommand( elseCmd, containingFQN, containingDecl )
                    
                case WhileCmdNd( guard, lil, body ) => {
                    resolveExp( guard, containingFQN, containingDecl )
                    resolveCommand( body, containingFQN, containingDecl )
                    resolveLoopInvariantList(lil, containingFQN, containingDecl)
                }  
                case ForCmdNd( decl, repetitions,lil, body ) => {
                    val forsFQN = decl.fqn
                    resolveDecl( decl, containingFQN, containingDecl )
                    resolveExp(repetitions, containingFQN, containingDecl )
                    resolveCommand( body, forsFQN, Some( decl ) )
                    resolveLoopInvariantList(lil, containingFQN, containingDecl)
                }   
                case CoForCmdNd( decl, repetitions,cl, body ) => {
                    val forsFQN = decl.fqn
                    resolveDecl( decl, containingFQN, containingDecl )
                    resolveExp(repetitions, containingFQN, containingDecl )
                    resolveCommand( body, forsFQN, Some(decl))
                    for (cn <- cl) {
                    for(len <- cn.pmn.locExp) { resolvePermValue(len,containingFQN,containingDecl)}
                    for(lsn <- cn.pmn.locSet) { resolveLocSetNd(lsn,containingFQN, containingDecl)}
                    }
                }
                    
                case CoCmdNd( cl, fstCmd, sndCmd ) => {
                    resolveCommand( fstCmd, containingFQN, containingDecl )
                    resolveCommand( sndCmd, containingFQN, containingDecl )
                    for (cn <- cl) {
                    for(len <- cn.pmn.locExp) { resolvePermValue(len,containingFQN,containingDecl)}
                    for(lsn <- cn.pmn.locSet) { resolveLocSetNd(lsn,containingFQN, containingDecl)}
                    }
                }   
                case AcceptCmdNd( methodImplementationList ) =>
                    for ( methImpl <- methodImplementationList ) 
                        resolveDecl( methImpl, containingFQN, containingDecl )
                 
                case WithCmdNd( lock, tpl, guard, command, gpl ) => {
                    resolveExp(guard,containingFQN,containingDecl)
                    resolveCommand( command, containingFQN, containingDecl )
                    for (pn <- tpl )
                      resolvePermissionNd(pn, containingFQN, containingDecl)
                    for (pn <- gpl )
                      resolvePermissionNd(pn, containingFQN, containingDecl)
                }  
                case AssertCmdNd (assertion) =>
                    resolveExp(assertion,containingFQN, containingDecl)
                
                case AssumeCmdNd (assumption) =>
                    resolveExp(assumption,containingFQN, containingDecl)
            }
        }
        
        def resolveExp( exp : ExpNd, containingFQN : FQN, containingDecl : Option[DeclNd] ) {        
          check(  containingDecl == None && containingFQN.names.length == 0 
                ||  containingDecl.isDefined && containingDecl.get.fqn == containingFQN )

            exp match {
                case NoExpNd() => {}
                case BooleanLiteralExpNd(b) => {}
                case IntLiteralExpNd(i) => {}
                case FloatLiteralExpNd(x) => {}
                case NameExpNd( name ) => 
                    name.decl = symTab.lookUp( containingFQN, name )
                case CanReadOp(locSet) => resolveLocSetNd(locSet, containingFQN, containingDecl)
                case CanWriteOp(locSet) => resolveLocSetNd(locSet, containingFQN, containingDecl)
                case PermissionOp(objId) => resolveExp(objId, containingFQN, containingDecl)
                case AccessOp(pm) => {
                 if (pm.locExp.length == pm.locSet.length){
                  for(lsn <- pm.locSet) { resolveLocSetNd(lsn,containingFQN, containingDecl)}
                  for(len <- pm.locExp) { resolvePermValue(len,containingFQN,containingDecl)}                  
                  }
                }
                case LengthOp(exp) => resolveExp(exp, containingFQN,containingDecl)
                case ForAllExp(forNameDeclList,exp_x,exp_y) => {
                  resolveExp(exp_x,containingFQN,containingDecl)
                  resolveExp(exp_y,containingFQN,containingDecl)
                }
                case BinaryOpExpNd( op, x, y ) =>
                    resolveExp( x, containingFQN, containingDecl )
                    resolveExp( y, containingFQN, containingDecl )
                case UnaryOpExpNd( op, x ) =>
                    resolveExp( x, containingFQN, containingDecl )
                case AsExpNd( x : ExpNd, ty : TypeNd ) =>
                    resolveExp( x, containingFQN, containingDecl ) 
                    resolveType( ty, containingFQN, containingDecl ) 
                case MemberExpNd( x, name ) =>
                    resolveExp( x, containingFQN, containingDecl )
                case ChainExpNd( ops, operands ) =>
                    for( x <- operands ) resolveExp( x, containingFQN, containingDecl )
                case FetchExpNd( exp ) => 
                    unreachable("FetchExpNd in resolver)")
            }
        }
        
        def resolveLock(lock: ExpNd, containingFQN : FQN, containingDecl : Option[DeclNd] ) {
          lock match {
            case NameExpNd(x) => {
              if (x == "this") {
                resolveExp (lock, containingDecl.get.fqn, containingDecl.get.parent)
              }
              else resolveExp(lock, containingFQN, containingDecl)
            }
            case _ => resolveExp(lock, containingFQN, containingDecl)
            
          }
        }
        def resolveClassInvariantList(classInvList : List[ClassInvNd], containingFQN: FQN, containingDecl: Option[DeclNd]){
          for(classInv <- classInvList)
            resolveExp(classInv.exp, containingFQN, containingDecl) 
        }        
        
        
        def resolveLoopInvariantList(loopInvList : List[LoopInvNd], containingFQN: FQN, containingDecl: Option[DeclNd]){
          for(loopInv <- loopInvList)
            resolveExp(loopInv.exp, containingFQN, containingDecl)
        }

        def resolveObjId( exp : ExpNd, containingFQN: FQN, containingDecl : Option[DeclNd] ) {
            exp match {
                case NameExpNd( name ) => name.decl = symTab.lookUp( containingFQN, name )
                case _ => {}
                    unreachable("FetchExpNd in resolver)")
            }
        }

        resolveDeclList( decls, new FQN(), None )
    }

}