package frontEnd

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer;
import frontEnd.AST._
import scala.collection.mutable.Set;
class Builder(val errorRecorder: ErrorRecorder) {

/******************/
  /** Declarations **/
/******************/

  def makeCoord(file: String, line: Int, col: Int) = Coord(file, line, col);

  def declList() = new DeclList()

  def classDeclNd(name: String, coord: Coord) = ClassDeclNd()(name, coord, errorRecorder)

  var invariantId = 0;
  def makeClassInvariant(exp: ExpNd, coord: Coord) = {
    val name = "*inv*" + invariantId; invariantId += 1;
    new ClassInvNd(exp)(name, coord)
  }

  def intfDeclNd(name: String, coord: Coord) = IntfDeclNd()(name, coord, errorRecorder)

  // permission map
  var claimId = 0;
  def makeClaimNd(pm: PermissionMapNd, coord: Coord) = {
    val name = "ClaimNd#" + claimId; claimId += 1;
    new ClaimNd(pm)(name, coord)
  }
  
  def makeThis(str : String , coord: Coord) = new ThisExpNd(str)(coord)

  // Object Permission Map list without the permission value

  class LocSetList extends ArrayBuffer[LocSetNd]

  def locSetList() = new LocSetList;

  def locSetList(x: LocSetNd) = { val result = new LocSetList; result += x; result }

  def add(lsl: LocSetList, lsn: LocSetNd) { lsl += lsn; }

  def makeObjectIdLSN(objId: ExpNd, coord: AST.Coord) = new ObjectIdLSN(objId)(coord)

  def makeArrayLSN(name: String, offSet: ExpNd, bound: ExpNd,boundInclusive: Boolean, locSet: LocSetNd, coord: Coord) = {
    val fvd = new ForVarDecl()(name, coord)
    val forDecl = new ForDecl(fvd)(nextForName(), coord)
    new ArrayLSN(forDecl: ForDecl,offSet: ExpNd,bound: ExpNd, boundInclusive: Boolean, locSet: LocSetNd)(coord)
  }

/*************************/
  /** PermissionMapNd lists **/
/*************************/

  class PermissionMapList extends ArrayBuffer[PermissionMapNd]

  def permissionMapList() = new PermissionMapList

  def permissionMapList(x: PermissionMapNd) = { val result = new PermissionMapList; result += x; result }

  def add(pml: PermissionMapList, pmn: PermissionMapNd) { pml += pmn }

  var PermMapId = 0;
  def makePermissionMapNd(lsl: LocSetList, el: ExpList, coord: AST.Coord) = {
    val oldId = PermMapId;
    val name = "PermMapNd#" + (PermMapId + 1);
    PermMapId += 1;
    new PermissionMapNd(lsl.toList, el.toList, name, coord);
  }

  def makeDefaultPermissionValue(coord: AST.Coord) = {
    var b: Double = 1.0;
    new FloatLiteralExpNd(b)(coord)
  }

  def methodDeclNd(name: String, acc: Access, paramList: ParamList, preCndList: PreCndList, postCndList: PostCndList, givesPerList: GivesPerList, takesPerList: TakesPerList, borrowsPerList: BorrowsPerList, coord: Coord) =
    { MethodDeclNd(acc, paramList.toList, preCndList.toList, postCndList.toList, givesPerList.toList, takesPerList.toList, borrowsPerList.toList)(name, coord) }

  class ParamList extends ArrayBuffer[ParamDeclNd]

  def paramList() = new ParamList();

  def add(l: ParamList, d: ParamDeclNd) { l += d }

  // Method Specification annotations
  
  // Pre Condition Specification
  class PreCndList extends ArrayBuffer[PreCndNd]

  def preCndList() = new PreCndList()

  def add(l: PreCndList, d: PreCndNd) { l += d }

  def makePre(condition: ExpNd, coord: Coord) = new PreCndNd(condition)(coord)

  // Post Condition Specification
  class PostCndList extends ArrayBuffer[PostCndNd]

  def postCndList() = new PostCndList()

  def add(l: PostCndList, d: PostCndNd) { l += d; }

  def makePost(condition: ExpNd, coord: Coord) = new PostCndNd(condition)(coord)

  // Gives Condition Specification
  class GivesPerList extends ArrayBuffer[GivesPerNd]

  def givesPerList() = new GivesPerList()

  def add(l: GivesPerList, d: GivesPerNd) { l += d; }

  def makeGives(pmn: PermissionMapNd, coord: Coord) = new GivesPerNd(pmn)(coord)

  // Takes Condition Specification
  class TakesPerList extends ArrayBuffer[TakesPerNd]

  def takesPerList() = new TakesPerList()

  def add(l: TakesPerList, d: TakesPerNd) { l += d; }

  def makeTakes(pmn: PermissionMapNd, coord: Coord) = new TakesPerNd(pmn)(coord)

  // Borrows Condition Specification
  class BorrowsPerList extends ArrayBuffer[BorrowsPerNd]

  def borrowsPerList() = new BorrowsPerList()

  def add(l: BorrowsPerList, d: BorrowsPerNd) { l += d; }

  def makeBorrows(pmn: PermissionMapNd, coord: Coord) = new BorrowsPerNd(pmn)(coord)

  //Building Claim Specification

  class ClaimList extends ArrayBuffer[ClaimNd]

  def claimList() = new ClaimList()

  def add(l: ClaimList, d: ClaimNd) { l += d }

  // Loop Invariant
  class LoopInvList extends ArrayBuffer[LoopInvNd]

  def loopInvList() = new LoopInvList()

  def add(l: LoopInvList, d: LoopInvNd) { l += d }

  // Condition 'acc'

  class ObjectIdList extends ArrayBuffer[ExpNd]

  def objectIdList() = new ObjectIdList()

  def add(l: ObjectIdList, x: ExpNd) { l += x }

  //'CanRead' Operation

  def makeCanReadOp(x: LocSetNd, coord: AST.Coord) = new CanReadOp(x)(coord)

  //'CanWrite' Operation

  def makeCanWriteOp(x: LocSetNd, coord: AST.Coord) = new CanWriteOp(x)(coord)

  //Get amount of 'Permission' Operation

  def makePermissionOp(x: ExpNd, coord: AST.Coord) = new PermissionOp(x)(coord)
  
  def makeAccessOp(p : PermissionMapNd, coord: AST.Coord) = new AccessOp(p)(coord)
  
  def makeLengthOp(x: ExpNd, coord: AST.Coord) = new LengthOp(x)(coord)
  
  
  class ForNameList extends ArrayBuffer[String]

  def forNameList() = new ForNameList;

  def forNameList(x: String) = { val result = new ForNameList; result += x; result }

  def add(fnl: ForNameList, fn: String) { fnl += fn; }
  
  class ForDeclList extends ArrayBuffer[ForDecl] {
      def add(l: ForDeclList, d: ForDecl) { l += d }
  }
  
  def makeForAllExp (fnl: ForNameList,x:ExpNd,y: ExpNd, coord: AST.Coord) = {
    val forDeclList = new ForDeclList()
    for(forName <- fnl) {
    val fvd = new ForVarDecl()(forName, coord)
    val forDecl = new ForDecl(fvd)(nextForName(), coord)
    forDeclList.add(forDeclList,forDecl)
    }
    new ForAllExp(forDeclList.toList,x,y)(coord)
  }
  
  var next = 0

  def threadDeclNd(claimList: ClaimList, bl: CommandNd, coord: Coord) = {
    val name = "t#" + next; next += 1
    ThreadDeclNd(claimList.toList, bl)(name, coord)
  }
  def objDeclNd(isGhost: Boolean, isConst: Boolean, name: String, acc: Access, ty: TypeNd, init: InitExpNd, coord: Coord) = ObjDeclNd(isGhost, isConst, acc, ty, init)(name, coord)
  
  def makeforVarDecl(name : String, ty: TypeNd , coord: Coord) = ForVarDecl ()(name, coord)

  def paramDeclNd(isGhost: Boolean, name: String, ty: TypeNd, paramCategory: ParamCategory, coord: Coord) = ParamDeclNd(isGhost, ty, paramCategory)(name, coord)

/************/
  /** Types  **/
/************/

  def namedTypeNd(name: NameNd, coord: Coord) = NamedTypeNd(name)(coord)

  // Adding ghost types
  def ghostTypeNd(name: NameNd, coord: Coord) = NamedTypeNd(name)(coord)

  def arrayTypeNd(baseTy: TypeNd, bound: ExpNd, coord: Coord) = ArrayTypeNd(baseTy, bound)(coord)

  def specializedTypeNd(baseTy: TypeNd, ga: GenericArgs, coord: Coord) = SpecializedTypeNd(baseTy, ga toList)(coord)

  class GenericArgs extends ArrayBuffer[TypeNd]

  def genArgs() = new GenericArgs()

  def add(ga: GenericArgs, ty: TypeNd) { ga += ty }

/******************/
  /** Commands    **/
/******************/

  def skip(coord: Coord) = new SkipCmdNd()(coord)

  def isSkip(p: CommandNd) = p match { case SkipCmdNd() => true; case _ => false }

  def seq(p: CommandNd, q: CommandNd, coord: Coord) = new SeqCommandNd(p, q)(coord)

  def localDecl(isGhost: Boolean, isConst: Boolean, name: String, ty: TypeNd, x: ExpNd, p: CommandNd, coord: Coord) = {
    val decl = LocalDeclNd(isGhost, isConst, ty, x, p)(name, coord)
    new LocalDeclCmdNd(decl)(coord)
  }

  def assignment(lhs: ExpList, rhs: ExpList, coord: Coord) = {
    val (lhsLen, rhsLen) = (lhs.length, rhs.length)
    val min = if (lhsLen < rhsLen) lhsLen else rhsLen
    if (lhsLen != rhsLen)
      errorRecorder.reportFatal("Left and right hand sides of := are not the same length", coord)
    new AssignmentCmdNd(lhs.toList.take(min), rhs.toList.take(min))(coord)
  }

  def call(method: ExpNd, args: ExpList, coord: Coord) = {
    method match {
      case MemberExpNd(x, name) => new CallCmdNd(method, args.toList)(coord)
      case NameExpNd(name) => new CallCmdNd(method, args.toList)(coord)
      case _ =>
        errorRecorder.reportFatal("Expected a method name", coord)
        errorRecorder.bailOut();
    }

  }

  def makeIf(guard: ExpNd, p: CommandNd, q: CommandNd, coord: Coord) = new IfCmdNd(guard, p, q)(coord)

  def makeWhile(guard: ExpNd, wlil: LoopInvList, p: CommandNd, coord: Coord) = new WhileCmdNd(guard, wlil.toList, p)(coord)

  def makeFor(name: String, x: ExpNd, flil: LoopInvList, p: CommandNd, coord: Coord) = {
    val fvd = new ForVarDecl()(name, coord)
    val forDecl = new ForDecl(fvd)(nextForName(), coord)
    new ForCmdNd(forDecl, x, flil.toList, p)(coord)
  }

  var loopInvId = 0;
  def makeLoopInvariant(exp: ExpNd, coord: Coord) = {
    val name = "*inv*" + loopInvId; loopInvId += 1;
    new LoopInvNd(exp)(name, coord)
  }

  def makeCo(name: String, x: ExpNd, cl: ClaimList, p: CommandNd, coord: Coord) = {
    val fvd = new ForVarDecl()(name, coord)
    val forDecl = new ForDecl(fvd)(nextForName(), coord)
    new CoForCmdNd(forDecl, x, cl.toList, p)(coord);
  }

  def makeCo(cl: ClaimList, p: CommandNd, q: CommandNd, coord: Coord) = new CoCmdNd(cl.toList, p, q)(coord)

  def makeWith(lock: ExpNd, tpl: TakesPerList, y: ExpNd, p: CommandNd, gpl: GivesPerList, coord: Coord) = new WithCmdNd(lock, tpl.toList, y, p, gpl.toList)(coord)

  def makeAccept(mil: MethodImplementationList, coord: Coord) = new AcceptCmdNd(mil toList)(coord)

  def methodImpl(nameNd: NameNd, paramList: ParamList, guard: ExpNd, fstCommand: CommandNd, sndCommand: CommandNd, coord: Coord) = new MethodImplementationDeclNd(nameNd, paramList toList, guard, fstCommand, sndCommand)(nameNd.qn.last, coord)

  def makeAssertCmd(assertion: ExpNd, coord: Coord) = new AssertCmdNd(assertion)(coord)

  def makeAssumeCmd(assumption: ExpNd, coord: Coord) = new AssumeCmdNd(assumption)(coord)

  class MethodImplementationList extends ArrayBuffer[MethodImplementationDeclNd]

  def methodImplementationList() = new MethodImplementationList

  def add(ps: MethodImplementationList, p: MethodImplementationDeclNd) { ps += p }

/********************/
/** Expressions    **/
/********************/
  def noExp(coord: AST.Coord) = new NoExpNd()(coord)

  def nameExp(name: NameNd, coord: AST.Coord) = NameExpNd(name)(coord)

  def realLiteralExp(str: String, coord: AST.Coord) = {
    val pattern = """([0-9,_]*)([.][0-9,_]*)?([eE][+-]?.*)?""".r
    val ePattern = """[eE]([+-]?)(.*)""".r
    val pattern(intPart, fractPart, expPart) = str;
    var b: Double = parseInt(intPart, 10, coord)
    if (fractPart != null) {
      var a: Double = parseInt(fractPart.substring(1), 10, coord)
      val underscores = fractPart count (c => c == '_')
      b = b + a * Math.pow(10.0, -(fractPart.length() - underscores - 1))
    }
    if (expPart != null) {
      val ePattern(sign, eInt) = expPart
      var e = parseInt(eInt, 10, coord)
      if (sign == "-")
        e = -e
      b = b * Math.pow(10.0, e)
    }
    new FloatLiteralExpNd(b)(coord)
  }

  def intLiteralExp(base: Int, str: String, coord: AST.Coord) = {
    var i = parseInt(str, base, coord)
    new IntLiteralExpNd(i)(coord)
  }
  
  def booleanLiteralExp(str: String, coord: AST.Coord) = {
    new BooleanLiteralExpNd(str)(coord)
  }

  def parseInt(str: String, base: Int, coord: frontEnd.AST.Coord) = {
    var i: Long = 0
    for (c <- str) {
      if (c != '_') {
        if ('a' <= c && c <= 'f') i = base * i + (c - 'a' + 10)
        else if ('A' <= c && c <= 'F') i = base * i + (c - 'A' + 10)
        else if ('0' <= c && c <= '9') i = base * i + (c - '0')
        else contracts.Contracts.unreachable()
        if (i < 0) {
          errorRecorder.reportFatal("In literal " + str + " is too big.", coord)
        }
      }
    }
    i
  }

  def indexExp(x: ExpNd, y: ExpNd, coord: AST.Coord) = new BinaryOpExpNd(IndexOp, x, y)(coord)

  def memberExp(x: ExpNd, name: String, coord: AST.Coord) = new MemberExpNd(x, name)(coord)

  def asExp(x: ExpNd, ty: TypeNd, coord: AST.Coord) = new AsExpNd(x, ty)(coord)

  def binaryOp(op: String, x: ExpNd, y: ExpNd, coord: AST.Coord) = {
    op match {
      case "=>" => BinaryOpExpNd(ImpliesOp, x, y)(coord)
      case "<=" => BinaryOpExpNd(ImpliesOp, y, x)(coord)
      case "<=>" => BinaryOpExpNd(EquivOp, x, y)(coord)
      case "or" => BinaryOpExpNd(OrOp, x, y)(coord)
      case "and" => BinaryOpExpNd(AndOp, x, y)(coord)
      case "+" => BinaryOpExpNd(AddOp, x, y)(coord)
      case "-" => BinaryOpExpNd(SubOp, x, y)(coord)
      case "*" => BinaryOpExpNd(MulOp, x, y)(coord)
      case "/" => BinaryOpExpNd(SlashDivOp, x, y)(coord)
      case "div" => BinaryOpExpNd(WordDivOp, x, y)(coord)
      case "mod" => BinaryOpExpNd(RemOp, x, y)(coord)
    }
  }
 
  def unaryOp(op: String, x: ExpNd, coord: AST.Coord) = {
    op match {
      case "not" => UnaryOpExpNd(NotOp, x)(coord)
      case "-" => UnaryOpExpNd(NegativeOp, x)(coord) 
      case "'" => UnaryOpExpNd(PrimeOp, x)(coord)
    }
  }

  case class ComparisonList(ops: List[ChainingOperator], operands: List[ExpNd])

  def startComparison(y: ExpNd) = ComparisonList(Nil, y :: Nil)

  def comparisonOp(op: String, x: ComparisonList, y: ExpNd) = {
    val operator =
      op match {
        case "=" => EqualOp
        case "~=" => NotEqualOp
        case "<" => LessOp
        case "_<" => LessOrEqualOp
        case ">" => GreaterOp
        case ">_" => GreaterOrEqualOp
      }
    val ComparisonList(ops, operands) = x;
    ComparisonList(operator :: ops, y :: operands)
  }

  def finishComparisonOp(x: ComparisonList, coord: AST.Coord) = {
    val ComparisonList(ops, operands) = x;
    ChainExpNd(ops.reverse, operands.reverse)(coord)
  }

/*************************/
  /** Expression lists    **/
/*************************/

  class ExpList extends ArrayBuffer[ExpNd]

  def expList() = new ExpList

  def expList(x: ExpNd) = { val result = new ExpList; result += x; result }

  def add(el: ExpList, exp: ExpNd) { el += exp }

/*************************/
  /** Init Expressions    **/
/*************************/

  def valueInitExp(exp: ExpNd, coord: Coord) = new ValueInitExpNd(exp)(coord)

  def newInitExp(ty: TypeNd, args: ExpList, coord: Coord) = new NewInitExpNd(ty, args toList)(coord)

  def arrayInitExp(name: String, bound: ExpNd, a: InitExpNd, coord: Coord) = {
    val fvd = new ForVarDecl()(name, coord)
    val forDecl = new ForDecl(fvd)(nextForName(), coord)
    new ArrayInitExpNd(forDecl, bound, a)(coord)
  }

  var forCounter = 0;

  def nextForName() = { forCounter += 1; "#f" + forCounter }

  def ifInitExp(guard: ExpNd, a: InitExpNd, b: InitExpNd, coord: Coord) = new IfInitExpNd(guard, a, b)(coord)

/***************/
  /** Names     **/
/***************/

  def simpleName(str: String, coord: Coord): NameNd =
    new NameNd(new RQN(str))(coord)

/******************/
  /** Constants    **/
/******************/

  def publicAccess() = PublicAccess

  def privateAccess() = PrivateAccess

  def inParamCategory() = InParamCategory

  def outParamCategory() = OutParamCategory

  def objParamCategory() = ObjParamCategory

  def noTypeNd(coord: Coord) = NoTypeNd()(coord)

  def topTypeNd(coord: Coord) = TopTypeNd()(coord)
}