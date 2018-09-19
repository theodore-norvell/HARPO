package frontEnd

import scala.collection.mutable.ArrayBuffer  
import scala.collection.mutable.ListBuffer;
import frontEnd.AST._ 

class Builder( val errorRecorder : ErrorRecorder ) {
 

/******************/
/** Declarations **/
/******************/
    
  def makeCoord( file : String, line: Int, col : Int) =  Coord(file, line, col ) ;
  
  def declList() =  new DeclList()
  
  def classDeclNd(name : String, coord : Coord) =  ClassDeclNd()( name, coord, errorRecorder )
  
  def makeObjInitClaimNd(name: String,locList: ExpList, coord: Coord)= new ObjInitClaimNd(locList.toList)( name, coord)
  
  def intfDeclNd(name : String, coord : Coord) =  IntfDeclNd()( name, coord, errorRecorder )

  def methodDeclNd( name : String, acc : Access, paramList : ParamList, preCndList: PreCndList, postCndList: PostCndList, givesPerList: GivesPerList, takesPerList: TakesPerList, borrowsPerList: BorrowsPerList, coord : Coord ) =
    {MethodDeclNd( acc, paramList.toList, preCndList.toList, postCndList.toList, givesPerList.toList, takesPerList.toList, borrowsPerList.toList )( name, coord )}
  
  
    
  class ParamList extends ArrayBuffer[ParamDeclNd] 
  
  def paramList() =  new ParamList() ;
  
  def add( l : ParamList,  d : ParamDeclNd ) { l += d }
  
  // Method Specification annotations
  // Pre Condition Specification
  class PreCndList extends ArrayBuffer[PreCndNd]

  def preCndList() = new PreCndList()

  def add(l: PreCndList, d: PreCndNd){l+=d}
  
  def makePre(condition: ExpNd, coord : Coord) = new PreCndNd(condition)(coord)
  
  // Post Condition Specification
  class PostCndList extends ArrayBuffer[PostCndNd]
  
  def postCndList() = new PostCndList()

  def add(l: PostCndList, d: PostCndNd){l+=d;}
  
  def makePost(condition: ExpNd, coord : Coord) = new PostCndNd(condition)(coord)
  
  
  // Gives Condition Specification
  class GivesPerList extends ArrayBuffer[GivesPerNd]
  
  def givesPerList() = new GivesPerList()

  def add(l: GivesPerList, d: GivesPerNd){l+=d;}
  
  def makeGives(objId: ExpNd, coord : Coord) = new GivesPerNd(objId)(coord)
  

  // Takes Condition Specification
  class TakesPerList extends ArrayBuffer[TakesPerNd]
  
  def takesPerList() = new TakesPerList()

  def add(l: TakesPerList, d: TakesPerNd){l+=d;}
  
  def makeTakes(objId: ExpNd, coord : Coord) = new TakesPerNd(objId)(coord)
  
  
  // Borrows Condition Specification
  class BorrowsPerList extends ArrayBuffer[BorrowsPerNd]
  
  def borrowsPerList() = new BorrowsPerList()

  def add(l: BorrowsPerList, d: BorrowsPerNd){l+=d;}
  
  def makeBorrows(objId: ExpNd, coord : Coord) = new BorrowsPerNd(objId)(coord)

  
  
  
  var next = 0 
  
  def threadDeclNd(thrClaim : ThrClaimNd, bl : CommandNd, coord : Coord ) = {
    val name = "t#" + next ; next += 1 
     ThreadDeclNd(thrClaim, bl)( name, coord ) 
  }
  
  
   def makeThrClaimNd(name: String,locList: ExpList, coord: Coord)= new ThrClaimNd(locList.toList)( name, coord)
  
  def objDeclNd(isGhost:Boolean,isConst : Boolean, name : String, acc : Access, ty : TypeNd, init : InitExpNd, coord : Coord )
  =  ObjDeclNd(isGhost,isConst, acc, ty, init )( name, coord) 
  
  def paramDeclNd( name : String, ty : TypeNd, paramCategory : ParamCategory, coord : Coord) 
  =  ParamDeclNd( ty, paramCategory )( name, coord )
  
  
/************/
/** Types  **/
/************/
  
  def namedTypeNd( name : NameNd, coord : Coord) =  NamedTypeNd( name )( coord )

  // Adding ghost types
  def ghostTypeNd( name : NameNd, coord : Coord) = NamedTypeNd(name)(coord)
  
  def arrayTypeNd( baseTy : TypeNd, bound : ExpNd, coord : Coord ) =  ArrayTypeNd( baseTy, bound )( coord )
  
  def specializedTypeNd( baseTy : TypeNd, ga : GenericArgs, coord : Coord ) =  SpecializedTypeNd( baseTy, ga toList )( coord )
  
  class GenericArgs extends ArrayBuffer[TypeNd]
  	
  def genArgs() = new GenericArgs()
  
  def add( ga : GenericArgs, ty : TypeNd) { ga += ty }
 
/******************/
/** Commands    **/
/******************/
  
  def skip(coord : Coord) = new SkipCmdNd()(coord)
  
  def isSkip(p : CommandNd) = p match{ case SkipCmdNd() => true ; case _ => false }
  
  def seq( p : CommandNd, q : CommandNd, coord : Coord ) = new SeqCommandNd( p, q )( coord) 
  
  def localDecl( isGhost: Boolean, isConst : Boolean, name : String, ty : TypeNd, x : ExpNd, p : CommandNd, coord : Coord )
  = { val decl = LocalDeclNd( isGhost,isConst, ty, x, p)( name, coord )
    new LocalDeclCmdNd( decl )( coord )
  }
  
  def assignment( lhs : ExpList, rhs : ExpList, coord : Coord )
  = { val (lhsLen, rhsLen) = (lhs.length, rhs.length)
      val min = if( lhsLen < rhsLen ) lhsLen else rhsLen
      if( lhsLen != rhsLen )
          errorRecorder.reportFatal("Left and right hand sides of := are not the same length", coord)
      new AssignmentCmdNd( lhs.toList.take(min), rhs.toList.take(min) )( coord )
  }
       
  
  def call( method : ExpNd, args : ExpList, coord : Coord ) =  {
      method  match {
          case MemberExpNd( x, name ) => new CallCmdNd( method, args.toList )( coord)
          case NameExpNd( name ) => new CallCmdNd( method, args.toList )( coord)
          case _ => errorRecorder.reportFatal("Expected a method name", coord)
                    errorRecorder.bailOut() ;
      }
      
  }
  
  def makeIf( guard : ExpNd, p : CommandNd, q : CommandNd, coord : Coord )
  = new IfCmdNd( guard, p, q )( coord) 
  
  def makeWhile( guard : ExpNd, p : CommandNd, coord : Coord ) = new WhileCmdNd( guard, p )( coord) 
  
  def makeFor( name : String, x : ExpNd, p : CommandNd, coord : Coord ) = {
      val fvd = new ForVarDecl()(name, coord)
      val forDecl = new ForDecl( fvd )( nextForName(), coord) 
      new ForCmdNd( forDecl, x, p )( coord ) }
  
  def makeCo( name : String, x : ExpNd, p : CommandNd, coord : Coord ) = {
      val fvd = new ForVarDecl()(name, coord)
      val forDecl = new ForDecl( fvd )( nextForName(), coord) 
      new CoForCmdNd( forDecl, x, p )( coord ) ;
  }
  
  def makeCo( p : CommandNd, q : CommandNd, coord : Coord ) = new CoCmdNd( p, q )( coord ) 
  
  def makeWith( x : ExpNd, y : ExpNd, p : CommandNd, coord : Coord ) = new WithCmdNd( x, y, p )( coord ) 
  
  def makeAccept( mil : MethodImplementationList, coord : Coord ) = new AcceptCmdNd( mil toList )( coord ) 
  
  def methodImpl( nameNd : NameNd, paramList : ParamList, guard : ExpNd, fstCommand : CommandNd, sndCommand : CommandNd, coord : Coord )
  = new MethodImplementationDeclNd(nameNd, paramList toList, guard, fstCommand, sndCommand
                                  )( nameNd.qn.last, coord )
                     
  // Trying to make the assert command in builder class
  // What I understood, parser is calling builder method and builder methods(respective) 
  // are creating trees from/with case classes in AST                          
  
  def makeAssert(assertion : ExpNd, coord : Coord) = new AssertCmdNd(assertion)(coord)
  
  def makeAssume(assumption: ExpNd, coord : Coord) = new AssumeCmdNd(assumption)(coord)
                                  
  class MethodImplementationList extends ArrayBuffer[MethodImplementationDeclNd]
  
  def methodImplementationList() = new MethodImplementationList
  
  def add( ps : MethodImplementationList, p : MethodImplementationDeclNd) { ps += p }
  
/********************/
/** Expressions    **/
/********************/
  def noExp(coord : AST.Coord) = new NoExpNd()( coord ) 
  
  def nameExp( name : NameNd, coord : AST.Coord ) = NameExpNd( name )(coord)
  
  def realLiteralExp( str : String, coord : AST.Coord ) = {
      val pattern = """([0-9,_]*)([.][0-9,_]*)?([eE][+-]?.*)?""".r
      val ePattern = """[eE]([+-]?)(.*)""".r
      val pattern(intPart, fractPart, expPart) = str ;
      var b : Double = parseInt(intPart, 10, coord) 
      if( fractPart != null ) {
          var a : Double = parseInt(fractPart.substring(1), 10, coord) 
          val underscores = fractPart count (c => c=='_')
          b = b + a * Math.pow(10.0, -(fractPart.length()-underscores-1) )
      }
      if( expPart != null ) {
          val ePattern(sign, eInt) = expPart
          var e = parseInt( eInt, 10, coord )
          if( sign == "-" )
              e = -e 
          b = b * Math.pow(10.0,e) 
      }
      new FloatLiteralExpNd( b )(coord)
  }
  
  def intLiteralExp( base : Int, str : String, coord : AST.Coord ) = {
      var i = parseInt(str, base, coord) 
      new IntLiteralExpNd( i )(coord)
  }

  def parseInt(str: String, base: Int, coord: frontEnd.AST.Coord) = {
    var i : Long = 0 
    for( c <- str ) {
        if(c != '_') {
            if( 'a'<= c && c <= 'f' ) i = base*i + (c - 'a' + 10)
            else if( 'A'<= c && c <= 'F' ) i = base*i + (c - 'A' + 10)
            else if ('0' <= c && c <= '9' ) i = base*i + (c - '0') 
            else contracts.Contracts.unreachable() 
            if( i < 0 ) {
                errorRecorder.reportFatal("In literal "+ str +" is too big.", coord)} }
        }
    i
  }
  
  def indexExp( x : ExpNd, y : ExpNd, coord : AST.Coord ) = new BinaryOpExpNd( IndexOp, x, y)(coord)
  
  def memberExp( x : ExpNd, name : String, coord : AST.Coord ) = new MemberExpNd( x, name )(coord)
  
  def asExp( x : ExpNd, ty : TypeNd, coord : AST.Coord) = new AsExpNd( x, ty )(coord)
  
  def binaryOp( op : String, x : ExpNd, y : ExpNd, coord : AST.Coord ) = {
    op match {
      case  "=>" =>   BinaryOpExpNd(  ImpliesOp, x, y)(coord)
      case  "<=" =>   BinaryOpExpNd(  ImpliesOp, y, x)(coord)
      case  "<=>" =>   BinaryOpExpNd(  EquivOp, x, y)(coord)
      case  "or" =>   BinaryOpExpNd(  OrOp, x, y)(coord)
      case  "and" =>   BinaryOpExpNd(  AndOp, x, y)(coord)
      case  "+" =>   BinaryOpExpNd(  AddOp, x, y)(coord)
      case  "-" =>   BinaryOpExpNd(  SubOp, x, y)(coord)
      case  "*" =>   BinaryOpExpNd(  MulOp, x, y)(coord)
      case  "/" =>   BinaryOpExpNd(  SlashDivOp, x, y)(coord)
      case  "div" =>   BinaryOpExpNd(  WordDivOp, x, y)(coord)
      case  "mod" =>   BinaryOpExpNd(  RemOp, x, y)(coord)
    }
  }
  
  def unaryOp( op : String, x : ExpNd, coord : AST.Coord ) = {
    op match {
      case  "not" =>   UnaryOpExpNd(  NotOp, x )(coord) 
      case  "-" =>   UnaryOpExpNd(  NegativeOp, x ) (coord)
    }
  }
  
  case class ComparisonList( ops : List[ChainingOperator], operands : List[ExpNd] )
  
  def startComparison( y : ExpNd ) =  ComparisonList( Nil, y :: Nil ) 
  
  def comparisonOp( op : String, x : ComparisonList, y : ExpNd ) = {
    val operator =
    op match {
      case  "=" =>   EqualOp  
      case  "~=" =>   NotEqualOp
      case  "<" =>   LessOp  
      case  "_<" =>   LessOrEqualOp
      case  ">" =>   GreaterOp  
      case  ">_" =>   GreaterOrEqualOp
    }
    val ComparisonList(ops, operands) = x ;
    ComparisonList( operator::ops, y::operands)
  }
  
  def finishComparisonOp(x : ComparisonList, coord : AST.Coord) = {
    val ComparisonList(ops, operands) = x ;
    ChainExpNd( ops.reverse, operands.reverse)(coord) }
       
/*************************/
/** Expression lists    **/
/*************************/
  
  class ExpList extends ArrayBuffer[ExpNd]
  
  def expList() = new ExpList 
  
  def expList(x:ExpNd) = {val result = new ExpList; result += x ; result} 
  
  def add( el : ExpList, exp : ExpNd ) { el += exp }
 
/*************************/
/** Init Expressions    **/
/*************************/
  
  def valueInitExp( exp : ExpNd, coord : Coord )
  = new ValueInitExpNd( exp )( coord )
  
  def newInitExp( ty : TypeNd, args : ExpList, coord : Coord )
  = new NewInitExpNd( ty, args toList)( coord )
 
  def arrayInitExp( name : String, bound : ExpNd, a : InitExpNd, coord : Coord ) = {
        val fvd = new ForVarDecl()(name, coord)
        val forDecl = new ForDecl( fvd )( nextForName(), coord) 
        new ArrayInitExpNd( forDecl, bound, a )( coord ) }
  
  var forCounter = 0 ;
  
  def nextForName() = { forCounter += 1 ; "#f" + forCounter }
  
  def ifInitExp( guard : ExpNd, a : InitExpNd, b : InitExpNd, coord : Coord )
  = new IfInitExpNd( guard, a, b )( coord )
  
/***************/
/** Names     **/
/***************/
  
  def simpleName( str : String, coord : Coord ) : NameNd =
  	new NameNd( new RQN(str) )( coord )  
	
/******************/
/** Constants    **/
/******************/
  
  def publicAccess() = PublicAccess 
  
  def privateAccess() =  PrivateAccess
  
  def inParamCategory() =  InParamCategory
  
  def outParamCategory() =  OutParamCategory
  
  def objParamCategory() =  ObjParamCategory
  
  def ghostInParamCategory() =  GhostInParamCategory
  
  def ghostOutParamCategory() =  GhostOutParamCategory
  
  def ghostObjParamCategory() =  GhostObjParamCategory
  
  def noTypeNd(coord : Coord) =  NoTypeNd()( coord )
  
  def topTypeNd(coord : Coord) =  TopTypeNd()( coord )
}