package checker

import contracts.Contracts
import frontEnd.AST._
import frontEnd.QN
import frontEnd.ErrorRecorder
import CheckerTypes._
import frontEnd.AST.PreCndNd;
import frontEnd.AST.PostCndNd;

class TypeChecker (
    errorRecorder : ErrorRecorder, 
    //classEnv : ClassEnvironment,
    //symbolTable : SymbolTable,
    typeCreator : TypeCreator )
extends Contracts { 
  
    def typeCheck( exp : ExpNd ) : Option[Type] = {
        val ty : Option[Type]
        = exp match {
            case NoExpNd() => None 
            
            case IntLiteralExpNd( i ) => 
                if( i <= (1L<<31) ) Some(int32)
                else Some(int64)
                
            case FloatLiteralExpNd( x ) =>
                Some( real64 )
                
            case NameExpNd( name ) =>
                val decl = name.decl.getOrElse{
                    Contracts.unreachable("Name not resolved by type checking time.") }
                decl match {
                    case ObjDeclNd(isGhost, isConst, acc, ty, init) =>
                        errorRecorder.checkFatal( ty.tipe != None,
                                                 "The type of "+name+" can not be determined at this point.",
                                                  exp.coord)
                        ty.tipe
                    case LocalDeclNd(isGhost, isConst, ty, init, cmd) =>
                        errorRecorder.checkFatal( ty.tipe != None,
                                                 "The type of "+name+" can not be determined at this point.",
                                                  exp.coord)
                        check( ty.tipe != None)
                        ty.tipe
                    case ParamDeclNd( ty, cat) =>
                        check( ty.tipe != None )
                        ty.tipe
                    case node@MethodDeclNd( acc, params, preCndList: List[PreCndNd], postCndList: List[PostCndNd], givesPerList: List[GivesPerNd], takesPerList: List[TakesPerNd], borrowsPerList: List[BorrowsPerNd] ) =>
                        check( node.tipe != None )
                        node.tipe
                    case _ => {
                        errorRecorder.reportFatal(name + " does not represent an object or location.", exp.coord ) ;
                        None } }
  
            case exp@BinaryOpExpNd(op, x, y) =>
                binaryOpType( exp ) ;
                
            case exp@UnaryOpExpNd( op, x ) =>
                unaryOpType( exp ) ;
            
            case exp@AsExpNd( x, ty ) =>
                asExpType(exp) 
                
            case exp@MemberExpNd( x, name ) =>
               memberType( exp ) 
               
            case exp@ChainExpNd( ops, operands ) =>
                // Type check all operands
                val optTys = operands map typeCheck
                if( optTys.exists { x => x==None } ) { }
                else {                 
                    // Value convert all operands
                    exp.operands = operands map valueConvert
                    // Compute the common type
                    val optTys = exp.operands map {x => x.tipe}
                    val commonType = optTys.reduce( lub )
                    if( commonType == None )
                        errorRecorder.reportFatal( "Operands of comparison could not be converted to a common type.",  exp.coord  ) ;
                    else if( commonType == Some(bool)
                         &&  exp.ops.exists { op => (op != EqualOp && op != NotEqualOp) })  
                        errorRecorder.reportFatal( "Values of type bool can not be compared except for equality and inequality.", exp.coord  ) ;
                    else // Convert all operands to a commonType
                        exp.operands = for( x <- exp.operands )
                                       yield typeConvert(x, commonType) }
                Some(bool)
                
            case FetchExpNd( x ) =>
                unreachable("FetchExpNd in typechecker input")
        } ;        
        exp.tipe = ty ;
        return ty
    }

    // Still working on - Inaam
    private def typeCheck( init : InitExpNd  ) : Option[Type] = {
        val optTy : Option[Type] = init match {
            
            case init@ValueInitExpNd( exp ) =>
                typeCheck( exp )
                init.exp = valueConvert( exp )
                init.exp.tipe
                
            case NewInitExpNd( ty, args ) =>
                // TODO check that the args match the type's constructor
                ty.tipe 
            
            case ArrayInitExpNd( forDecl, bound, exp ) =>
                for( t <- typeCheck( exp ) ;
                     tb <- typeCheck( bound ) ;
                     x = errorRecorder.checkFatal(
                             isIntegralType( tb ),
                             "Bound should of integral type",
                             bound.coord ) ;
                     y = errorRecorder.checkFatal(
                             isElaborationTimeConstant( bound),
                             "Bound should be elaboration time constant",
                             bound.coord)
                    ) yield ArrayType( t, bound )
                
            case init@IfInitExpNd( guard, a, b) =>
                init.guard = convertGuard( guard )
                for( ta <- typeCheck( a ) ;
                     tb <- typeCheck( b ) ;
                     tc <- leastCommonType( ta, tb)
                   ) yield  tc 
                   
            case WidenInitExpNd( a ) =>
                unreachable("WidenInitExpNd in typechecker input")
        }
        init.tipe = optTy ;
        optTy 
    }
    
    def typeCheck( decls : DeclList ) {
        for( decl <- decls.decls ) typeCheck( decl ) 
    }
    
    def typeCheck( decls : List[DeclNd] ) {
        for( decl <- decls ) typeCheck( decl ) 
    }
    
    def typeCheck( decl : DeclNd ) {
        decl match {
            
            case decl@ClassDeclNd() => 
                typeCheck( decl.directMembers )
            
                
            case decl@IntfDeclNd() => 
                typeCheck( decl.directMembers )
                
                
            case decl@ObjDeclNd(isGhost, isConst : Boolean, acc : Access, ty : TypeNd, init : InitExpNd) => 
                // If the init expression is an expression, we may
                // need to convert a literal expression to an 'as' expresssion.
                init match {
                    case valNd@ValueInitExpNd( exp ) =>
                        valNd.exp = insertAsExpressionForLiterals( ty, exp )
                    case _ => {} }
                
                
                for( initType <- typeCheck( init ) ) {
                    // If the user gave no type, fill it in with the type of the initialization expression
                    ty match {
                        case NoTypeNd() => ty.tipe = Some( initType )
                        case _ => {} } }
        
                // If the declared or inferred type of the object is primitive,
                // make it a location type (even if it is constant).
                ty.tipe match {
                    case Some(tipe@PrimitiveType( _ )) =>
                        ty.tipe = Some( LocationType( tipe ) )
                    case _ => {} }
                
                val expectedType = ty.tipe match {
                    case Some( LocationType( tipe ) ) => Some( tipe )
                    case optType => optType }
                
                // Convert the initialization expression to match the expected type
                decl.init = typeConvertInitExpNd( init, expectedType )
            
            case decl@ClaimNd(objIds) => {
              for( id <- objIds ) typeCheckObjId( id )
            }
            case decl@ClassInvNd(exp) => {
              typeCheck( exp )
            } 
            case decl@ParamDeclNd( ty, cat ) =>
                // Promotions and checks were done in TypeCreator's pass.
                check( ty.tipe != None ) 
                
            case decl@MethodDeclNd( acc, params, preCndList: List[PreCndNd], postCndList: List[PostCndNd], givesPerList: List[givesPerNd], takesPerList: List[TakesPerNd], borrowsPerList: List[BorrowsPerNd] ) =>
                for( pdn <- params ) typeCheck( pdn )
                for( precn <- preCndList ) typeCheck( precn )
                for( postcn <- postCndList ) typeCheck( postcn )
                for( gpl <- givesPerList ) typeCheck( gpl )
                for( tpl <- takesPerList ) typeCheck( tpl)
                for( bpl <- borrowsPerList ) typeCheck( bpl )
                // TODO anything else to check?
                // TODO set the .tipe field.
                
            case decl@ThreadDeclNd(claim, block ) =>
                typeCheck( block ) 
                
            case decl@LocalDeclNd(isGhost, isConst, ty, initExp, cmd) =>
                val initExp$ = insertAsExpressionForLiterals(ty, initExp)
                decl.init = initExp$
                
                for( initType <- typeCheck(initExp$) ) {
                    ty match {
                        case NoTypeNd() => ty.tipe = Some( initType )
                        case _ => () } }
                
                // If the declared or inferred type of the object is primitive,
                // make it a location type (even if it is constant).
                ty.tipe match {
                  case Some(tipe@PrimitiveType( _ )) =>
                      ty.tipe = Some( LocationType( tipe ) )
                  case _ => {} }
              
                val expectedType = ty.tipe match {
                    case Some( LocationType( tipe ) ) => Some( tipe )
                    case optType => optType }
                
                // Convert the initialization expression to match the expected type
                decl.init = typeConvert( initExp$, expectedType )
                
                typeCheck( cmd ) 
                
            case decl@ GenericParamDeclNd( ty ) =>
                toDo("Deal with generic parameters in the  type checker")
                
            case PrimitiveTypeDeclNd( qn ) => {}
            
            case ForDecl( fvd ) => typeCheck( fvd )
                
            case ForVarDecl() =>
                unreachable( "ForVarDecl in typechecker" )
        
            case impl@MethodImplementationDeclNd( nameNd : NameNd,
                                       paramList : List[ParamDeclNd],
                                       guard : ExpNd,
                                       fstCmd : CommandNd,
                                       sndCmd : CommandNd ) => {
                for( pdn <- paramList  ) typeCheck( pdn )
                
                impl.guard = convertGuard( guard )
                
                check( nameNd.decl.isDefined,
                       "Method implementation name was not resolved by type checking phase" )
                val decl = nameNd.decl.get
                
                // The following command checks that the declaration and the method and
                // its implementation match.
                // TODO check that each declared method of a class is implemented exactly once.
                
                if( decl.isInstanceOf[MethodDeclNd]) {
                    val methDecl = decl.asInstanceOf[MethodDeclNd]
                    for( declType <- methDecl.tipe ;
                         implType <- impl.tipe
                    )   if( declType.paramList.size != implType.paramList.size) 
                            errorRecorder.checkFatal( false,
                                s"Implementation and declaration of method ${nameNd} have different numbers of arguments.",
                                nameNd.coord )
                        else
                            for( (p,q) <- declType.paramList zip implType.paramList ) {
                                if( p.ty != q.ty )
                                    errorRecorder.checkFatal( declType==implType,
                                        s"Implementation and declaration of parameter ${q.name.last} have different types.",
                                        q.coord )
                                if( p.direction != q.direction )
                                    errorRecorder.checkFatal( declType==implType,
                                        s"Implementation and declaration of parameter ${q.name.last} have different modes.",
                                        q.coord )
                                if( p.name.last != q.name.last )
                                    errorRecorder.checkFatal( declType==implType,
                                        s"Parameter ${q.name.last} should be named '${p.name.last}'.",
                                        q.coord )  } }
                else
                    errorRecorder.reportFatal(
                        nameNd + " does not resolve to a method declaration",
                        nameNd.coord )
                        
                typeCheck( fstCmd )
                typeCheck( sndCmd ) }  
        }
    }
    
    def typeCheck(mSpecNd: MethodSpecNd){
      mSpecNd match{
        case mSpecNd@PreCndNd(preCnd) => preCndCheck(preCnd)
        case mSpecNd@PostCndNd(postCnd) => postCndCheck(postCnd)
      }
    }
    
    private def preCndCheck(pre: ExpNd)={
      val pTy= typeCheck(pre)
      val result = valueConvert(pre)
      for (ty <- result.tipe)
        errorRecorder.checkFatal(ty==bool, " 'pre' condition must be boolean.", pre.coord)
      result
    }
    
    private def postCndCheck(post: ExpNd)={
      val pTy= typeCheck(post)
      val result = valueConvert(post)
      for (ty <- result.tipe)
        errorRecorder.checkFatal(ty==bool, " 'post' condition must be boolean.", post.coord)
      result
    }
    
    def typeCheck(mPerNd: MethodPerNd){
      mPerNd match{
        case mPerNd@GivesPerNd(objId)=> typeCheckObjId(objId)
        case mPerNd@TakesPerNd(objId)=> typeCheckObjId(objId)
        case mPerNd@BorrowsPerNd(objId)=> typeCheckObjId(objId)
      }
      
    }
    
    def typeCheckObjId(objId: ExpNd){
      objId match {
        case NameExpNd(i) => convertId(objId)
        case _ => println("Type Not Allowed")
      }
    }
     //Permission Nodes
     private def convertId( id : ExpNd ) = {
        val gTy = typeCheck(id)
        val result = valueConvert( id )
        for( ty <- result.tipe )
            errorRecorder.checkFatal(ty != None,
                              "Object Id must be on primitive object.",
                              id.coord)
        result
    }
     
    def typeCheck( command : CommandNd ) {
        command match {
            
            case SkipCmdNd() => {}
            
            case SeqCommandNd( fstCmd, sndCmd ) =>
                typeCheck( fstCmd )
                typeCheck( sndCmd )
                
            case LocalDeclCmdNd( decl ) =>
                typeCheck( decl)
                
            case cmd@AssignmentCmdNd( lhss, rhss ) =>
                cmd.rhs = typeCheckAssignments( lhss, rhss)
                
            case cmd@CallCmdNd( method, argList ) =>
                val methodType = typeCheck( method )
                val argTypes = argList map typeCheck
                if( ! methodType.isEmpty && argTypes.forall(_.isDefined) ) {
                    val Some(mty) = methodType
                    mty match {
                        case MethodType(paramList) =>
                            if( argList.length == paramList.length ) 
                                cmd.argList = for( (p,a) <- paramList zip argList ) yield typeCheckAnArgument( a, p )
                            else {
                                val were = if(paramList.length == 1) "was" else "were"
                                val are = if( argList.length == 1 ) "is" else "are"
                                errorRecorder.reportFatal("Incorrect number of arguments. "
                                            + s"${paramList.length} ${were} expected, but there ${are} ${argList.length}.",
                                            command.coord ) }
                        case _ => errorRecorder.reportFatal(s"Type error in call command. Expected a method, but found a ${mty}",
                                            method.coord )
                    }
                }
            case cmd@IfCmdNd( guard, thenCmd, elseCmd ) =>
                cmd.guard = convertGuard( guard )
                typeCheck( thenCmd )
                typeCheck( elseCmd )
                
            case cmd@WhileCmdNd( guard, body ) =>
                cmd.guard = convertGuard( guard )
                typeCheck( body )
          
            // make new case of assert command
                
            case cmd@AssertCmdNd(assertion) =>
                cmd.assertion = convertAssertion(assertion)
            
            case cmd@AssumeCmdNd(assumption) =>
                cmd.assumption = convertAssumption(assumption)
                
            case ForCmdNd( forDecl, repetitions, body ) =>
                toDo( "For commands in type checker")
                
            case CoForCmdNd( forDecl, repetitions, body ) =>
                toDo( "Co-for commands in type checker")
                
            case CoCmdNd( fstCmd, sndCmd ) =>
                typeCheck( fstCmd )
                typeCheck( sndCmd )
                
            case AcceptCmdNd( methodImplementationList ) =>
                for ( methImpl <- methodImplementationList ) {
                    typeCheck( methImpl ) }
                
            case withCmd@WithCmdNd( lock, guard, command ) =>
                withCmd.guard = convertGuard( guard )
                toDo( "With commands in type checker")
        }
    }
    

    private def convertGuard( guard : ExpNd ) = {
        val gTy = typeCheck(guard)
        val result = valueConvert( guard )
        for( ty <- result.tipe )
            errorRecorder.checkFatal(ty==bool,
                              "Guard expression must be boolean.",
                              guard.coord) 
        result
    }
    // ---- Checking the assertion expression.
    
    private def convertAssertion( assertion : ExpNd ) = {
        val gTy = typeCheck(assertion)
        val result = valueConvert( assertion )
        for( ty <- result.tipe )
            errorRecorder.checkFatal(ty==bool,
                              "assertion expression must be boolean.",
                              assertion.coord) 
        result
    }
        
    private def convertAssumption( assumption : ExpNd ) = {
        val gTy = typeCheck(assumption)
        val result = valueConvert( assumption )
        for( ty <- result.tipe )
            errorRecorder.checkFatal(ty==bool,
                              "assume expression must be boolean.",
                              assumption.coord) 
        result
    }
              
    private def insertAsExpressionForLiterals( ty : TypeNd, exp : ExpNd ) : ExpNd =
        // If the declaration is explicitly typed...
        ty match {
            case NoTypeNd() =>exp
            case _ => 
                exp match {
                    // and the initialization is an integer literal
                    case lit@IntLiteralExpNd(longValue) =>
                        // ...and the object is explicitly typed as an Int8, Int16, Int32, Real16, or Real32
                        ty.tipe match {
                            case Some(LocationType(tipe)) =>
                                if( tipe == int8 || tipe == int16
                                    || tipe == real16 || tipe == real32) {
                                    // the expression is changed to an "as" expression.
                                    AsExpNd( lit, ty )( exp.coord ) }
                                else exp
                            case _ => exp }
                    // or the initialization expression is a float literal 
                    case lit@FloatLiteralExpNd( dblValue ) =>
                        // ...and the object is explicitly typed as an Real16, or Real32
                        ty.tipe match {
                            case Some(LocationType(tipe)) =>
                                if( tipe==real16 || tipe == real32) {
                                    // the expression is changed to an "as" expression.
                                    AsExpNd( lit, ty )( exp.coord ) }
                                else exp
                            case _ => exp }
                    case _ => exp } }
    
    private def typeCheckAssignments( lhss : Seq[ExpNd], rhss : Seq[ExpNd] )
    : Seq[ExpNd] = {
        check( lhss.length == rhss.length ) // Checked in first pass.
        val lhsTypes = lhss map typeCheck 
        rhss map typeCheck ;
        // The following loop checks for valid LHSs and converts the RHSs
        // to the base type of the LHS if possible.
        lhsTypes zip rhss zip lhss map {
            case ((Some(LocationType( base )), rhs), lhs) =>
                // No need to check that base is primitive, since
                // Location types can only be constructed for primitive types
                typeConvert(valueConvert(rhs), Some(base))
            case ((Some(_), rhs), lhs) =>
                errorRecorder.reportFatal(
                        "Left hand side of assignment does not represent a location.",
                        lhs.coord)
                rhs
            case ((None, rhs), lhs) =>
                rhs }
    }
    
    private def typeCheckAnArgument( a : ExpNd, p : Parameter ) : ExpNd = {
        val LocationType(paramBaseType) = p.ty 
        p.direction match {
            case InParamCategory =>
                val a1 = valueConvert(a) 
                check( a1.tipe.isDefined ) // Because value convert should not introduce a None for the type.
                if( a1.tipe.get == paramBaseType || isWiderThan(paramBaseType, a1.tipe.get) ) {
                    val a2 = typeConvert(a1, Some(paramBaseType) )
                    a2  }
                else {
                    errorRecorder.reportFatal(
                            s"Argument for parameter ${p.name} should the of type ${paramBaseType} or"
                            + s" wider, but is of type ${a1.tipe}.", a.coord) ;
                    a }
                    
            case OutParamCategory =>
                a.tipe.get match {
                    case LocationType( argBaseType ) =>
                        if(argBaseType==paramBaseType || isWiderThan( argBaseType, paramBaseType) ) {
                            a }
                        else {
                            errorRecorder.reportFatal(
                                s"Argument for parameter ${p.name} should the of type ${paramBaseType} or"
                                + s" narrower, but is of type ${argBaseType}.", a.coord) 
                            a }
                    case _ => 
                        errorRecorder.reportFatal(
                                s"Argument for 'out' parameter ${p.name} should refer to a location.", a.coord) ;
                        a }
                
            case ObjParamCategory =>
                Contracts.unreachable("obj param category")
                a
        }
    }
    
    private def binaryOpType( exp : BinaryOpExpNd )
    : Option[Type] = {
        val BinaryOpExpNd( op, x, y ) = exp 
        for( xTy <- typeCheck(x) ;
             yTy <- typeCheck(y) ;
             ty <- binaryOpType$( exp, x, y, xTy, yTy, op )
        ) yield ty }
    
    private def binaryOpType$( exp : BinaryOpExpNd, x : ExpNd, y : ExpNd, xTy : Type, yTy : Type, op : BinaryOperator ) 
    : Option[ Type ] =
        op match {  
            case ImpliesOp | EquivOp | OrOp | AndOp => 
                exp.x = valueConvert( x )
                exp.y = valueConvert( y )
                if( exp.x.tipe == None || exp.y.tipe == None )
                    None
                else if( exp.x.tipe.get != bool || exp.y.tipe.get != bool ) {
                    errorRecorder.reportFatal( "The " + op + "operation applies only to values of type Bool.", exp.coord )
                    None }
                else
                    Some(bool)
                
            case AddOp | SubOp | MulOp =>
                exp.x = valueConvert( x )
                exp.y = valueConvert( y )
                if( exp.x.tipe == None || exp.y.tipe == None )
                    None
                else {
                    val (xt, yt) = (exp.x.tipe.get, exp.y.tipe.get)
                    val result = lub$( xt, yt )
                    if( result == None ){
                        errorRecorder.reportFatal( s"The ${op} operation does not apply to values of type "+
                                                   s"${xt} and ${yt}.", exp.coord )
                        None }
                    else  if( ! isArithmeticType( result.get ) ) {
                        errorRecorder.reportFatal( s"The ${op} operation is applied to values of types"
                                    + s" ${xt} and ${yt}, but should only be applied to values of arithmetic type.",
                                    exp.coord )
                        None }
                    else {
                        exp.x = typeConvert( exp.x, result )
                        exp.y = typeConvert( exp.y, result )
                        result } }
                
            case SlashDivOp =>
                exp.x = valueConvert( x )
                exp.y = valueConvert( y )
                if( exp.x.tipe == None || exp.y.tipe == None )
                    None
                else {
                    val (xt, yt) = (exp.x.tipe.get, exp.y.tipe.get)
                    // Calculate the narrowest common real type.
                    val result = lub( lub$( xt, yt ), Some(real16) )
                    if( result == None ) {
                        errorRecorder.reportFatal( s"The ${op} operation does not apply to values of type "+
                                                   s"${xt} and ${yt}.", exp.coord )
                        None }
                    else {
                        exp.x = typeConvert( exp.x, result )
                        exp.y = typeConvert( exp.y, result )
                        result } }
                
            case WordDivOp | RemOp =>
                exp.x = valueConvert( x )
                exp.y = valueConvert( y )
                if( exp.x.tipe == None || exp.y.tipe == None )
                    None
                else {
                    val (xt, yt) = (exp.x.tipe.get, exp.y.tipe.get)
                    val result = lub$( xt, yt )
                    if( result == None ) {
                        errorRecorder.reportFatal( s"The ${op} operation does not apply to values of type "+
                                                   s"${xt} and ${yt}.", exp.coord )
                        None }
                    else if( ! isIntegralType( result.get ) ) {
                        errorRecorder.reportFatal( s"The ${op} operation is applied to values of types"
                                    + s" ${xt} and ${yt}, but should only be applied to values of integral type.",
                                    exp.coord )
                        None }
                    else {
                        exp.x = typeConvert( exp.x, result )
                        exp.y = typeConvert( exp.y, result )
                        result } }
                
            case IndexOp =>
                exp.y = valueConvert( y )
                if( exp.x.tipe == None || exp.y.tipe == None )
                    None
                else {
                    val (xt, yt) = (exp.x.tipe.get, exp.y.tipe.get)
                    if( ! isIntegralType( yt ) ) {
                        errorRecorder.reportFatal( s"Array index is of type ${xt}, but should be of integral type",
                                exp.coord )
                        None }
                    else {
                        xt match {
                            case ArrayType( base, bound ) =>
                                Some(base)
                            case _ =>
                                errorRecorder.reportFatal( "Indexing applies only to arrays.", exp.coord )
                                None } } }
        }
    
    private def unaryOpType( exp : UnaryOpExpNd )
    : Option[Type] =
        typeCheck(exp.x) flatMap {xTy =>
            exp.op match {    
                case NegativeOp => 
                    exp.x = valueConvert(exp.x) ;
                    if( exp.x.tipe == None ) 
                        None
                    else if( ! isArithmeticType(exp.x.tipe.get) ) {
                        errorRecorder.reportFatal( s"The ${exp.op} operation applies only to values of arithmetic type", exp.coord )
                        None }
                    else exp.x.tipe
                    
                case NotOp =>
                    exp.x = valueConvert(exp.x) ;
                    if( exp.x.tipe == None ) 
                        None
                    else if( exp.x.tipe.get != bool ) {
                        errorRecorder.reportFatal( s"The  ${exp.op} operation applies only to boolean values", exp.coord )
                        None }
                    else exp.x.tipe
                } }
    
    private def asExpType( exp : AsExpNd ) : Option[Type] = {
        var result : Option[Type] = None ;
        typeCheck(exp.x) 
        val tipe = typeCreator.extractTypeFromTypeNode(exp.ty)
        exp.ty.tipe = Some(tipe) ;
        for( xTy <- exp.x.tipe ) {
            if( !isArithmeticType(xTy)) {
                errorRecorder.reportFatal( "The as operation applies only to arithmetic values.", exp.coord )
            } else if( !isArithmeticType(tipe) ) {
                errorRecorder.reportFatal( "The as operation can only convert to arithmetic types.", exp.coord )
            } else {
                result = Some(tipe) }
        }
        result
    }
    
    private def memberType( exp : MemberExpNd ) : Option[Type] = {
        val MemberExpNd( x, name ) = exp
        val xOptTy = typeCheck(x)
        xOptTy flatMap { xTy => 
            xTy match {
                case ClassIntfType( classLikeDeclNd ) =>
                    val optDecl = classLikeDeclNd.findMemberIndirect( name )
                    optDecl match {
                        case None =>
                            errorRecorder.reportFatal(
                                s"Type ${classLikeDeclNd.fqn} has no member named ${name}",
                                exp.coord)
                            None
                        case Some( ObjDeclNd(isGhost, isConst, acc, ty, init) ) =>
                            // TODO check accessibility
                            ty.tipe
                        case Some( memberDecl@MethodDeclNd( acc, params, preCndList: List[PreCndNd], postCndList: List[PostCndNd], givesPerList: List[GivesPerNd], takesPerList: List[TakesPerNd], borrowsPerList: List[BorrowsPerNd]) ) =>
                            // TODO check accessibility
                            memberDecl.tipe
                        case Some( ParamDeclNd( ty, paramCategory) ) =>
                            // TODO add accessibility to constructor parameters and
                            // check accessibility
                            ty.tipe
                        case _ =>
                            errorRecorder.reportFatal(
                                "Member must be a field, constructor parameter or method",
                                exp.coord)
                            None
                    }
                case _ =>
                    errorRecorder.reportFatal( "Only classes and interfaces have members.", exp.coord )
                    None
            }
        }
    }
  
    private def valueConvert( exp : ExpNd ) : ExpNd = 
        exp.tipe match {
            case Some(LocationType( base )) =>
                val exp$ = FetchExpNd( exp )( exp.coord )
                exp$.tipe = Some(base) 
                exp$
            case _ => exp }
    
    private def typeConvert( exp : ExpNd, optType : Option[Type] ) : ExpNd = 
        (exp.tipe, optType) match {
            case(Some(tyFrom), Some(tyTo)) =>
                if( tyFrom==tyTo) {
                    exp }
                else if( isWiderThan( tyTo, tyFrom ) ) {
                    val exp$ = AsExpNd( exp, nodeFor(tyTo) )( exp.coord )
                    exp$.tipe = Some(tyTo)
                    exp$ }
                else {
                    // At the moment the only nontrivial conversion is widening.
                    // Anything else indicates an internal error.
                    errorRecorder.reportFatal( "Can not convert " +tyFrom+ " to " +tyTo+ ".", exp.coord );
                    exp }
            case _ =>
                exp
        }
    
    private def typeConvertInitExpNd( exp : InitExpNd, optType : Option[Type] ) : InitExpNd = 
        (exp.tipe, optType) match {
            case(Some(tyFrom), Some(tyTo)) =>
                if( tyFrom==tyTo) {
                    exp }
                else if( isWiderThan( tyTo, tyFrom ) ) {
                    val exp$ = WidenInitExpNd( exp )( exp.coord )
                    exp$.tipe = Some(tyTo)
                    exp$ }
                else {
                    // At the moment the only nontrivial conversion is widening.
                    // Anything else indicates an internal error.
                    errorRecorder.reportFatal( "Can not convert " +tyFrom+ " to " +tyTo+ ".", exp.coord );
                    exp }
            case _ =>
                exp
        }
    
    private def isArithmeticType( ty : Type ) =
        ty match {
            case `int8` | `int16` | `int32` | `int64` 
                | `real16` | `real32` | `real64` => true
            case _ => false }
    
    private def isIntegralType( t : Type ) : Boolean = {
        t match {
            case `int8`| `int16` | `int32` | `int64` => true ;
            case _ => false ;
        }
    }
    
    private val lubTable : Array[Array[Option[PrimitiveType]]] = 
        Array(         //  Int8          Int16         Int32        Int64        Real16        Real32        Real64        Bool
        /*Int8*/    Array( Some(int8),   Some(int16),  Some(int32), Some(int64), Some(real16), Some(real32), Some(real64), None ),
        /*Int16*/   Array( Some(int16),  Some(int16),  Some(int32), Some(int64), Some(real32), Some(real32), Some(real64), None ),
        /*Int32*/   Array( Some(int32),  Some(int32),  Some(int32), Some(int64), Some(real64), Some(real64), Some(real64), None ),
        /*Int64*/   Array( Some(int64),  Some(int64),  Some(int64), Some(int64), None,         None,         None,         None ),
        /*Real16*/  Array( Some(real16), Some(real32), Some(real64),None,        Some(real16), Some(real32), Some(real64), None ),
        /*Real32*/  Array( Some(real32), Some(real32), Some(real64),None,        Some(real32), Some(real32), Some(real64), None ),
        /*Real64*/  Array( Some(real64), Some(real64), Some(real64),None,        Some(real64), Some(real64), Some(real64), None ),
        /*Bool*/    Array( None,         None,         None,        None,        None,         None,         None,         None ))
     
    private def lub$( xty : Type, yty : Type ) : Option[Type] = {
        if ( isPrimitiveType( xty ) && isPrimitiveType( yty ) ) {
            val xpty = xty.asInstanceOf[ PrimitiveType ];
            val ypty = yty.asInstanceOf[ PrimitiveType ];
            lubTable( xpty.index )( ypty.index ); }
        else None
    }
        
    private def lub( xoptty : Option[Type], yoptty : Option[Type]) : Option[Type] = {
        if ( xoptty == None || yoptty == None )
            None
        else
            lub$( xoptty.get, yoptty.get)
    }
    
    private val isWiderThanTable : Array[Array[Boolean]] = 
        // NB. This relation is nonreflexive but transitive
        Array(         //  Int8   Int16  Int32  Int64  Real16 Real32 Real64 Bool
        /*Int8*/    Array( false, false, false, false, false, false, false, false ),
        /*Int16*/   Array( true,  false, false, false, false, false, false, false ),
        /*Int32*/   Array( true,  true,  false, false, false, false, false, false ),
        /*Int64*/   Array( true,  true,  true,  false, false, false, false, false ),
        /*Real16*/  Array( true,  false, false, false, false, false, false, false ),
        /*Real32*/  Array( true,  true,  false, false, true,  false, false, false ),
        /*Real64*/  Array( true,  true,  true,  false, true,  true,  false, false ),
        /*Bool*/    Array( false, false, false, false, false, false, false, false ) )
        
    /** Is xTy wider than yTy ? Note that if either is not primitive, the result is false. */
    private def isWiderThan( xty : Type, yty : Type ) : Boolean = {
        if ( isPrimitiveType( xty ) && isPrimitiveType( yty ) ) {
            val xpty = xty.asInstanceOf[ PrimitiveType ]
            val ypty = yty.asInstanceOf[ PrimitiveType ]
            isWiderThanTable( xpty.index )( ypty.index ) }
        else false
    } 
    
    private def leastCommonType( ta : Type, tb : Type ) : Option[Type] = {
        assert(false, "TODO") ;// TODO
        None
    }
    
    private def isElaborationTimeConstant( exp : ExpNd ) : Boolean = {
        assert(false, "TODO") ;// TODO
        false
    }
}