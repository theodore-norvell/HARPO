package checker
import frontEnd.AST._
import frontEnd.FQN
import frontEnd.ErrorRecorder
import CheckerTypes._
import contracts.Contracts._

class ClassEnvironmentCreator( errorRecorder : ErrorRecorder ) {
//    
//    /** Create a mapping from type names to descriptions of the classes and interfaces they name.
//     * 
//     * Returns a ClassEnvironement, which is a map from fully qualified names to ClassIntfDescription.
//     * 
//     * @see{ClassIntfDescription}
//     */
//    def buildClassEnvironment( decls : DeclList ) : ClassEnvironment = {
//        type MutableCE = scala.collection.mutable.HashMap[ FQN, ClassIntfDescription ];
//        
//        val result : MutableCE = new MutableCE();
//
//        def buildClassEnvironment( decls : DeclList ) {
//            for ( decl <- decls.declarations ) {
//                decl match {
//                    case d : ClassLike =>
//                        val typeVars = extractTypeVarsAndBounds( d )
//                        val declarations = extractDeclarations( d )
//                        val superTypes = extractSuperTypes( d )
//                        val cid = ClassIntfDescription( typeVars, declarations, superTypes )
//                        result += ( ( d.fqn, cid ) )
//                        // TODO d.tipe = 
//                        // The next line isn't needed, but will be if we allow nested classes
//                        //     buildClassEnvironment( d.members )
//                    case ObjDeclNd( isConst, acc, ty, init )   => {}
//                    case PrimitiveTypeDeclNd(_) => {};
//                    case otherwise => unreachable()
//                }
//            }
//        }
//        buildClassEnvironment( decls ) 
//        result.toMap
//    }
//
//    private def extractTypeVarsAndBounds( decl : ClassLike ) = {
//        var typeVars : List[ TypeVar ] = Nil
//        for ( genPar <- decl.genericParameters ) {
//            
//            val bound : Type = genPar.ty.tipe match {
//                case Some(ty) => ty
//                case None => /* Error should already be reported*/ TopType()
//            }
//            typeVars = TypeVar( genPar.fqn, bound ) :: typeVars;
//        }
//        typeVars.reverse
//    }
//    
//    private def extractDeclarations( decl : ClassLike ) : Typothesis = {
//        var result = new Typothesis()
//        for( d <- decl.members.declarations ) {
//            d match {
//                case ObjDeclNd( isConst, acc, ty, _) =>
//                    if( ty.tipe == None ) {}
//                    else if( isPrimitiveType( ty.tipe.get ) ) {
//                        val base = ty.tipe.get.asInstanceOf[PrimitiveType]
//                        result = result + ((d.fqn, LocationType(base))) }
//                    else
//                        result = result + ((d.fqn, ty.tipe.get))
//                case MethodDeclNd( acc, params ) =>
//                    result = result + ((d.fqn, MethodType(extractParameters(params))) )
//                case _ => () } }
//        result
//    } 
//    
//    private def extractParameters( params : List[ParamDeclNd] ) : List[Parameter] =
//        params map {case p@ParamDeclNd( ty, cat ) =>
//                    if( ty.tipe == None ) {
//                        Parameter( p.fqn, p.coord, None, cat ) }
//                    else if( isPrimitiveType( ty.tipe.get ) ) {
//                        val base = ty.tipe.get.asInstanceOf[PrimitiveType]
//                        Parameter( p.fqn, p.coord, Some(LocationType(base)), cat ) }
//                    else
//                        Parameter( p.fqn, p.coord, None, cat )}
//
//    private def extractSuperTypes( decl : ClassLike ) : List[ClassIntfType] = {
//        var result = List[ClassIntfType]() ;
//        for( superTypeNd <- decl.superTypes ) {
//            for(  t <- superTypeNd.tipe ) {
//                t match {
//                    case t : ClassIntfType =>
//                        // TODO check the number of arguments 
//                        // equals the number of parameters.
//                        // and that superTypeNd represents an
//                        // interface.
//                        result = t::result 
//                    case _ =>
//                        errorRecorder.reportFatal("Supertype " +superTypeNd+ " is not an interface",
//                            superTypeNd.coord) } } }
//        result.reverse
//    }


}