package checker

import frontEnd.AST._
import frontEnd.{ RQN, FQN }
import frontEnd.ErrorRecorder
import CheckerTypes._
import contracts.Contracts
import scala.collection.mutable
import frontEnd.AST.PreCndNd;
import frontEnd.AST.PostCndNd;
import frontEnd.AST.GivesPerNd;
import frontEnd.AST.BorrowsPerNd;
import frontEnd.AST.TakesPerNd;

class SymbolTableMaker(errorRecorder: ErrorRecorder)
  extends Contracts {

  /**
   * Create a symbol table mapping fully qualified names to declarations.
   *
   *  This operates recursively, so the symbol table will also contain mappings for nexted declarations.
   *
   *  As a side effect, the FQN field of the declarations is set to its FQN.
   */
  def createSymbolTable(decls: DeclList): SymbolTable = {
    var next = 0;

    val hash = new mutable.HashMap[FQN, DeclNd]

    val directMembersMap = new mutable.HashMap[FQN, Map[String, DeclNd]]

    val allMembersMap = new mutable.HashMap[FQN, Map[String, DeclNd]]

    val directAncestors = new mutable.HashMap[FQN, Set[FQN]]

    val allAncestors = new mutable.HashMap[FQN, Set[FQN]]

    def addFQN(fqn: FQN, decl: DeclNd) {
      if (hash.contains(fqn)) {
        errorRecorder.reportFatal("The name " + fqn.toString + " is already in use.", decl.coord)
      }
      decl.setFQN(fqn)
      hash += ((fqn, decl))
    }

    def buildSTFromDeclList(decls: DeclList, containingFQN: FQN) {
      //Console.out.println( s"Building from ${decls.declarations.size} declarations.")
      for (decl <- decls.decls) {
        //Console.out.println( s"  >decl is ${decl.name}.")
        buildSTFromDecl(decl, containingFQN)
      }
    }

    def buildSTFromDeclSet(decls: List[DeclNd], containingFQN: FQN) {
      //Console.out.println( s"Building from ${decls.declarations.size} declarations.")
      for (decl <- decls) {
        //Console.out.println( s"  >decl is ${decl.name}.")
        buildSTFromDecl(decl, containingFQN)
      }
    }

    def buildSTFromDecl(decl: DeclNd, containingFQN: FQN) {
      val fqn = containingFQN.append(decl.name)
      addFQN(fqn, decl)
      decl match {
        case d: ClassDeclNd =>
          buildSTFromClassLike(d, containingFQN)
        case d: IntfDeclNd =>
          buildSTFromClassLike(d, containingFQN)
        case ObjDeclNd(isGhost: Boolean, isConst: Boolean, acc: Access, ty: TypeNd, init: InitExpNd) =>
          buildSTFromInitExp(init, containingFQN)
        case ClaimNd(perMapNd) => buildSTFromClaimNd(perMapNd,containingFQN)
        case ClassInvNd(exp) => buildSTFromClassInv(exp,containingFQN)
        case ParamDeclNd(isGhost: Boolean,ty: TypeNd, paramCategory: ParamCategory) => () // we don't need to make symbol table for parameter declarations
        case MethodDeclNd(_, params, preCndList, postCndList, givesPerList, takesPerList, borrowsPerList) =>
          for (p <- params) buildSTFromDecl(p, fqn)
          for (spec <- preCndList) buildSTfromMethodSpecList(spec, fqn)
          for (spec <- postCndList) buildSTfromMethodSpecList(spec, fqn)
          for (givesper <- givesPerList) buildSTfromMethodPerList(givesper, fqn)
          for (takesper <- takesPerList) buildSTfromMethodPerList(takesper, fqn)
          for (borrowsper <- borrowsPerList) buildSTfromMethodPerList(borrowsper, fqn)
        case ThreadDeclNd(claimNdList, block) => {
          buildSTfromCommand(block, fqn)
          buildSTFromClaimNdList(claimNdList,fqn)
        }
        case LocalDeclNd(isGhost, isConst, ty, init, stmt) =>
          buildSTfromCommand(stmt, fqn)
        case GenericParamDeclNd(ty: TypeNd) => ()
        case ForDecl(fvd) => buildSTFromDecl(fvd, fqn)
        case ForVarDecl() => ()
        case MethodImplementationDeclNd(nameNd: NameNd,
          paramList: List[ParamDeclNd],
          guard: ExpNd,
          fstCmd: CommandNd,
          sndCmd: CommandNd) => {
          for (pdn <- paramList) buildSTFromDecl(pdn, fqn)
          buildSTfromCommand(fstCmd, fqn)
          buildSTfromCommand(sndCmd, containingFQN)
        }
        case PrimitiveTypeDeclNd(fqn: FQN) => ()
      }
    }

    def buildSTFromClassLike(d: ClassLike, containingFQN: FQN) {
      for (gp <- d.genericParameters)
        buildSTFromDecl(gp, containingFQN)

      for (p <- d.constructorParams)
        buildSTFromDecl(p, containingFQN)

      buildSTFromDeclSet(d.directMembers, containingFQN.append(d.name))

      val members: mutable.HashMap[String, DeclNd] = new mutable.HashMap[String, DeclNd]
      for (memberDecl <- d.directMembers) {
        members += ((memberDecl.name, memberDecl))
      }
      directMembersMap += ((d.fqn, members.toMap[String, DeclNd]))

    }

    def buildSTFromInitExp(d: InitExpNd, fqn: FQN) {
      d match {
        case ValueInitExpNd(exp: ExpNd) => ()
        case NewInitExpNd(ty: TypeNd, args: List[ExpNd]) => ()
        case ArrayInitExpNd(forDecl: ForDecl, bound: ExpNd, a: InitExpNd) => {
          buildSTFromDecl(forDecl, fqn)
          buildSTFromInitExp(a, forDecl.fvd.fqn)
        }
        case IfInitExpNd(guard: ExpNd, a: InitExpNd, b: InitExpNd) => {
          buildSTFromInitExp(a, fqn)
          buildSTFromInitExp(b, fqn)
        }
        case WidenInitExpNd(a: InitExpNd) => // What is this widenInitExpNd
          unreachable()
      }
    }

    def buildSTFromClaimNdList(claimNds: List[ClaimNd],containingFQN: FQN) {
      for (claim <- claimNds) {
        buildSTFromClaimNd(claim.pmn,containingFQN)
      }
    }
    
    def buildSTFromClaimNd(perMapNd: PermissionMapNd,containingFQN: FQN) {
      ()
    }
    
    def buildSTFromClassInv(exp: ExpNd,containingFQN: FQN) {
      ()
    }

    def buildSTfromMethodSpecList(msn: MethodSpecNd, containingFQN: FQN) {
      msn match {
        case PreCndNd(condition) => ()
        case PostCndNd(condition) => ()
      }
    }

    def buildSTfromMethodPerList(mpn: MethodPerNd, containingFQN: FQN) {
      mpn match {
        case GivesPerNd(pmn) => ()
        case TakesPerNd(pmn) => ()
        case BorrowsPerNd(pmn) => ()
      }
    }

    def buildSTfromCommand(command: CommandNd, containingFQN: FQN) {
      command match {
        case SkipCmdNd() =>
          ()
        case SeqCommandNd(fstCmd, sndCmd) =>
          buildSTfromCommand(fstCmd, containingFQN)
          buildSTfromCommand(sndCmd, containingFQN)
        case LocalDeclCmdNd(decl) =>
          buildSTFromDecl(decl, containingFQN)
        case AssignmentCmdNd(lhs, rhs) =>
          ()
        case CallCmdNd(method, argList) =>
          ()
        case IfCmdNd(guard, thenCmd, elseCmd) =>
          buildSTfromCommand(thenCmd, containingFQN)
          buildSTfromCommand(elseCmd, containingFQN)
        case WhileCmdNd(guard, body) =>
          buildSTfromCommand(body, containingFQN)

        //---- Adding assert case, 'skip' in case of C
        case AssertCmdNd(assertion) => ()
        case AssumeCmdNd(assertion) => ()
        
        case ForCmdNd(forDecl, repetitions, body) => {
          buildSTFromDecl(forDecl, containingFQN)
          buildSTfromCommand(body, forDecl.fvd.fqn)
        }
        case CoForCmdNd(forDecl, repetitions, body) => {
          buildSTFromDecl(forDecl, containingFQN)
          buildSTfromCommand(body, forDecl.fvd.fqn)
        }
        case CoCmdNd(fstCmd, sndCmd) => {
          buildSTfromCommand(fstCmd, containingFQN)
          buildSTfromCommand(sndCmd, containingFQN)
        }
        case AcceptCmdNd(methodImplementationList) =>
          for (methImpl <- methodImplementationList) {
            buildSTFromDecl(methImpl, containingFQN)
          }
        case WithCmdNd(lock, guard, command) =>
          buildSTfromCommand(command, containingFQN)
      }
    }

    def computeAnscestors(decls: DeclList): Boolean = {
      // Pre: hash has been computed
      // Post: Result is false if the ancestor relation is circular. Otherwise true.

      def getRoot(tyNd: TypeNd): Option[NameNd] = tyNd match {
        case NamedTypeNd(nameNd) => Some(nameNd)
        case SpecializedTypeNd(genericType, _) => getRoot(genericType)
        case otherwise => {
          errorRecorder.reportFatal("Only interfaces and instances of interfaces "
            + "can be supertypes", tyNd.coord);
          None
        }

      }

      def computeDirectAncestors() {
        for (d <- decls.decls) {
          d match {
            case d: ClassLike =>
              val set = new mutable.HashSet[FQN];
              for (
                tyNd <- d.superTypes;
                nameNd <- getRoot(tyNd)
              ) {
                val qn = nameNd.qn
                // We treat the name as an FQN even if it is not one in the code.
                // Really we should resolve the name, but we haven't got a
                // symbol table yet to do that with!
                val fqn = qn match {
                  case rqn: RQN => new FQN().append(rqn)
                  case fqn: FQN => fqn
                }
                hash.get(fqn) match {
                  case None =>
                    errorRecorder.reportFatal(
                      "Super type " + qn + "not found",
                      nameNd.coord)
                  case Some(idn: IntfDeclNd) =>
                    set += idn.fqn
                  case Some(_) =>
                    errorRecorder.reportFatal("Only interfaces and instances of interfaces "
                      + "can be supertypes", nameNd.coord);
                }
              }
              directAncestors += ((d.fqn, set.toSet[FQN]))
            case _ => ()
          }
        }
      }

      def topSort(): Option[Seq[FQN]] = {
        // Pre directAnscestors has been computed
        // Note direct ancestors defines a graph in which there is an
        //   end from each u to each node v in directAnscestors of u.
        // Definition: u precedes v in a graph if there is a
        //   path from u to v and no path from v to u.
        // Post The result is a sequence of all nodes
        //   For all nodes u, v: if u precedes v in the graph
        //   then u follows v in the result.

        val whiteNodes = new mutable.HashSet[FQN]
        whiteNodes ++= directAncestors.keys
        val greyNodes = new mutable.HashSet[FQN]

        var result = new mutable.ArrayBuffer[FQN]
        var circular = false

        def dfs(u: FQN) {
          whiteNodes -= u
          greyNodes += u
          for (v <- directAncestors(u))
            if (whiteNodes.contains(v))
              dfs(v)
            else if (greyNodes.contains(v)) {
              circular = true
              errorRecorder.reportFatal(
                "There is a cycle of super types involving types "
                  + hash(u).fqn + " and " + hash(v).fqn + ".",
                hash(u).coord)
            }

          greyNodes -= u
          result += u
        }

        while (!whiteNodes.isEmpty) {
          val qn = whiteNodes.head
          dfs(qn)
        }
        if (circular) None else Some(result)
      }

      def computeIndirectAncestors(): Boolean = {
        var circular: Boolean = false
        topSort() match {
          case Some(topologicalOrder) =>
            for (u <- topologicalOrder) {
              val allAncestorsOf_u = new mutable.HashSet[FQN]()
              allAncestorsOf_u ++= directAncestors(u)
              for (v <- directAncestors(u)) allAncestorsOf_u ++= allAncestors(v)
              allAncestors += ((u, allAncestorsOf_u.toSet[FQN]))
            }
            true
          case None =>
            false
        }

      }

      computeDirectAncestors()
      computeIndirectAncestors()
    }

    def computeAllMembersMap(decls: DeclList) {
      for (decl <- decls.decls) {
        decl match {
          case decl: ClassLike =>
            val allMembers_decl = new mutable.HashMap[String, DeclNd];
            allMembers_decl ++= directMembersMap(decl.fqn)
            for (
              superQN <- allAncestors(decl.fqn);
              (str0, mem0) <- directMembersMap(superQN)
            ) {
              if (allMembers_decl.keySet.contains(str0)
                && mem0.fqn != allMembers_decl(str0).fqn)
                errorRecorder.reportFatal(
                  "Inherited member " + mem0.fqn +
                    " conflicts with member " + allMembers_decl(str0).fqn +
                    " in declaraion of " + decl.fqn,
                  decl.coord)
              else
                allMembers_decl += ((str0, mem0))
            }
            allMembersMap += ((decl.fqn, allMembers_decl.toMap[String, DeclNd]))
          case _ => ()
        }
      }
    }

    buildSTFromDeclList(decls, new FQN())
    val ok = computeAnscestors(decls)
    if (ok) {
      computeAllMembersMap(decls)
      new SymbolTable(
        errorRecorder,
        hash.toMap[FQN, DeclNd],
        allMembersMap.toMap[FQN, Map[String, DeclNd]])
    } else
      null // This only happens when inheritance is circular.
    // Caller had better quit if any fatal errors found.
  }

}