package boogieBackEnd;
import frontEnd.AST;
import frontEnd.AST._;
import util.Format;
import util.OutputBuilder;
import contracts.Contracts;
import scala.collection.mutable.StringBuilder;
import frontEnd.AST.ClassInvNd;
import frontEnd.AST.LoopInvNd;
import scala.collection.mutable.ArrayBuffer;

private class ClassCodeGen(val dlNd: DeclNd, outputBuilder: OutputBuilder) {

  //global translation context has most generic form of heap and object reference,
  //local translation contexts are made while changing to particular Heap and object reference i.e preHeap, tempHeap, oldHeap, and *_this, *_that

  private var transContext = new TransContext("Heap", "This_" + dlNd.fqn.toString)

  private val expObj = new ExpCodeGen;

  private var nameExp = ArrayBuffer[String]();

  private var classInvList = List[ClassInvNd]();

  private var classClaimList = List[ClaimNd]();

  private var globalObjectsList = List[ObjDeclNd]();

  private var lockExpList = List[String]() //lock expression must be an object implementing interface lock

  def getOutputBuilder() : OutputBuilder = outputBuilder; 
  
  def classCodeGen(): Unit = {

    classIdentifierCodeGen()

    objectIdentifierCodeGen()

    constructorProcedureCodeGen()

    threadProcedureCodeGen()
    
  }
  
  // Class Identifier Code Generation
  private def classIdentifierCodeGen() {
    outputBuilder.newLine
    outputBuilder.put("const unique " + dlNd.fqn.toString + ":ClassName;")
  }

  //Object Identifier Code Generation

  private def objectIdentifierCodeGen() {
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
      mem match {
        case ObjDeclNd(isGhost: Boolean,
          isConst: Boolean,
          acc: Access,
          ty: TypeNd,
          init: InitExpNd) => {
          globalObjectsList :+ mem
          val objType: String = TypeCodeGen(ty)
          outputBuilder.newLine
          outputBuilder.put("const unique " + mem.fqn.toString + ": Field " + objType + ";")
        }
        case _ => {}
      }
  }

  // Constructor Procedure Code Generation

  private def constructorProcedureCodeGen() {

    outputBuilder.newLine
    outputBuilder.put("procedure " + dlNd.fqn.toString + ".constructor" + "(" + transContext.objRef + ":Ref)") // dlNd.fqn is class name
    outputBuilder.newLine
    outputBuilder.put("requires dtype(" + transContext.objRef + ") <: " + dlNd.fqn.toString + ";")
    outputBuilder.newLine
    outputBuilder.put("modifies " + transContext.getHeap() + ";")
    outputBuilder.newLine
    outputBuilder.put("{")
    outputBuilder.newLine
    outputBuilder.indent
    outputBuilder.put("var Permission : PermissionType where (forall <x> r: Ref, f: Field x :: Permission[r,f] == 0.0);")
    outputBuilder.newLine
    outputBuilder.put("var oldHeap:HeapType;")
    outputBuilder.newLine
    outputBuilder.put("havoc " + transContext.getHeap() + ";")
    //Class Claim Code Generation
    classClaimCodeGen()
    //Objects initialization Code Generation, It Initializes the Heap
    objectsInitCodeGen()
    //Class Invariant Code
    classInvCodeGen()
    //Closing of Constructor Procedure
    outputBuilder.newLine
    outputBuilder.dedent
    outputBuilder.put("}")

  }

  //Thread Procedure Code Generation

  private def threadProcedureCodeGen() {
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
      mem match {
        case ThreadDeclNd(claimList: List[ClaimNd], block: CommandNd) => {
          outputBuilder.newLine
          outputBuilder.put("procedure " + dlNd.name + "." + mem.name + " (" + transContext.objRef + " : Ref)")
          outputBuilder.newLine
          outputBuilder.put("requires dtype(" + transContext.objRef + ") <: " + dlNd.fqn.toString + ";")
          outputBuilder.newLine
          outputBuilder.put("modifies " + transContext.getHeap() + ";")
          outputBuilder.newLine
          outputBuilder.put("{")
          outputBuilder.newLine
          outputBuilder.indent
          outputBuilder.put("var Permission: PermissionType where (forall<x> r: Ref, f: Field x :: Permission[r,f] == 0.0);")
          outputBuilder.newLine
          outputBuilder.put("var oldPermission: PermissionType where (forall<x> r: Ref, f: Field x :: oldPermission[r,f] == 0.0);")
          outputBuilder.newLine
          outputBuilder.put("var lockPermission: PermissionType where (forall<x> r: Ref, f: Field x :: lockPermission[r,f] == 0.0);")
          outputBuilder.newLine
          outputBuilder.put("var oldHeap, preHeap, tempHeap: HeapType;")
          outputBuilder.newLine

          // get all the lock variables  declared here
          //Thread Claim Code Generation
          for (claim <- claimList) {
            claimCodeGen(claim)
          }

          commandCodeGen(block) //Thread Body Code Generation

          outputBuilder.newLine
          outputBuilder.dedent
          outputBuilder.put("}//end of Thread Procedure")
        }

        case _ => {}
      }
  }

  //Class Claim Code Generation

  private def classClaimCodeGen() {
    transContext.set("Permission", transContext.objRef)
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ClaimNd(pmn) =>
          classClaimList :+ mem
          val perm_pairs = pmn.pm;
          var result = "";
          for ((loc, amnt) <- perm_pairs) {
            loc match {
              case ObjectIdLSN(nameExp) => {
                val fqn = dlNd.name + "." + nameExp.name.qn.toString
                val amount: String = expObj.simpleExpCodeGen(amnt);
                outputBuilder.newLine
                outputBuilder.put("//Claim")
                outputBuilder.newLine
                outputBuilder.put(transContext.getHeap() + "[" + transContext.getObjRef() + "," + fqn + "] := " + transContext.getHeap() + "[" + transContext.getObjRef() + "," + fqn + "] + " + amount + ";")
              }
              case _ => contracts.Contracts.toDo("Location Set Node with Array")
            }
          }
        case _ =>
      }
    }
    outputBuilder.newLine
    outputBuilder.setError("Maximum Permission Amount Must Not Exceed \'1.0\'", dlNd.coord)
    outputBuilder.put("assert (forall<x> r: Ref, f: Field x :: Permission[r,f] <= 1.0);")
    outputBuilder.newLine
    outputBuilder.clearError
    transContext.reset()
  }

  //Object Initialization Code Generation

  private def objectsInitCodeGen() {
    var objDecl, objInits = ""
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ObjDeclNd(isGhost: Boolean, isConst: Boolean, acc: Access, ty: TypeNd, init: InitExpNd) => {
          objDecl = objDeclCodeGen(isConst, acc, ty, mem.fqn.toString)
          val objType: String = TypeCodeGen(ty)
          val initExp: String = expObj.initExpCodeGen(init)
          outputBuilder.newLine
          outputBuilder.put("//Initialize Heap")
          outputBuilder.newLine
          outputBuilder.put(transContext.heap + "[" + transContext.objRef + "," + mem.fqn.toString + "] := " + initExp + ";")
          // building the expression not required for this subroutine, This subroutine contains the initialization of objects
          outputBuilder.newLine
        }
        case _ => ""
      }
    }
  }

  private def commandCodeGen(cmd: CommandNd) {

    cmd match {

      case SkipCmdNd() => { //Do Nothing
      }

      case SeqCommandNd(fstCmd, sndCmd) => {
        outputBuilder.indent
        commandCodeGen(fstCmd)
        commandCodeGen(sndCmd)
        outputBuilder.dedent
      }

      case LocalDeclCmdNd(decl) => {
        val objType: String = TypeCodeGen(decl.ty)
        outputBuilder.newLine
        outputBuilder.put("Heap[" + transContext.objRef + "," + decl.fqn.toString() + "] := " + expObj.simpleExpCodeGen(decl.init) + ";")
        outputBuilder.newLine
      }

      case AssignmentCmdNd(lhs, rhs) => {

        // Sum up all locked Permissions first, later put it in assignment defindness
        // Assert Write Permission on LHS and ReadPermission on LHS
        //isAssignmentDefined(lhs.toList, rhs.toList)

        outputBuilder.newLine
        outputBuilder.put("//Check Assignment Defindness")
        outputBuilder.newLine
        transContext.reset()
        val localTransContext = transContext;
        localTransContext.setHeap("Permission")
        val expPairs = lhs zip rhs
        for ((lhs, rhs) <- expPairs) {
          val lhs_result = expObj.buildWritingPerExp(lhs, localTransContext)
          outputBuilder.setError("Do not have enough permission(s) on LHS of assignment", lhs.coord)
          if (!(lockExpList.isEmpty)) {
            for (lockPer <- lockExpList) {
              outputBuilder.put("assert ")
              outputBuilder.put(lockPer.toString() + " + ")
            }
            outputBuilder.put(lhs_result + ";") // Two asserts instead of one
            outputBuilder.newLine
            outputBuilder.clearError
          } else {
            outputBuilder.put("assert " + lhs_result + ";") // Two asserts instead of one
            outputBuilder.newLine
            outputBuilder.clearError
          }
          nameExp = new ArrayBuffer[String]()
          val nameExps = expObj.nameExpCodeGen(rhs, nameExp)
          if (!(nameExps.isEmpty)) {
            outputBuilder.newLine
            outputBuilder.setError("Permission amount should be greater than 0.0", rhs.coord)

            if (!(lockExpList.isEmpty)) {
              outputBuilder.newLine
              outputBuilder.put("assert ")
              val lockExp$ = lockExpList.dropRight(1);
              for (lockPer <- lockExp$)
                outputBuilder.put(lockPer.toString() + "> 0.0" + " && ")
              outputBuilder.put(lockExpList.last + " > 0.0;")
              outputBuilder.newLine
              outputBuilder.put("assert ")
              val nameExp$ = nameExps.dropRight(1)
              for (name <- nameExp$)
                outputBuilder.put("Permission[" + localTransContext.getObjRef() + "," + name + "]" + " > 0" + "&&")
              outputBuilder.put("Permission[" + localTransContext.getObjRef() + "," + nameExps.last + "] > 0.0;")
              outputBuilder.newLine
              outputBuilder.clearError
            } else {
              outputBuilder.newLine
              outputBuilder.put("assert ")
              val nameExp$ = nameExps.dropRight(1)
              for (name <- nameExp$)
                outputBuilder.put("Permission[" + localTransContext.getObjRef() + "," + name + "]")
              outputBuilder.put("Permission[" + localTransContext.getObjRef() + "," + nameExps.last + "] > 0.0;")
              outputBuilder.newLine
              outputBuilder.clearError
            }
          }
        }
        //val rhs_result = expObj.buildReadingPerExp(rhs, localTransContext)
        // one glitch while translating the IntLiteralExpressionNode
        outputBuilder.newLine
        outputBuilder.put("//Check Assignment Defindness Ends")

        // assignment translation context
        transContext.reset()
        outputBuilder.newLine
        outputBuilder.put("//Assignment Command")
        outputBuilder.newLine
        for (l_exp <- lhs.init) {
          outputBuilder.put(expObj.buildBoogieExp(l_exp, transContext)) // build with TransContext
          if (!(l_exp.equals(lhs.last))) // Not to put comma for last expression in sequence
            outputBuilder.put(",")
        }
        outputBuilder.put(expObj.buildBoogieExp(lhs.last, transContext))
        outputBuilder.put(" := ")

        for (r_exp <- rhs.init) {
          outputBuilder.put(expObj.buildBoogieExp(r_exp, transContext))
          if (!(r_exp.equals(rhs.last))) // Not to put comma for last expression in sequence
            outputBuilder.put(",")
        }
        outputBuilder.put(expObj.buildBoogieExp(rhs.last, transContext))
        outputBuilder.put(";")
        outputBuilder.newLine
        outputBuilder.put("//Assignment Command Ends")
      }

      case CallCmdNd(method, argList) => {

      }

      case IfCmdNd(guard, thenCmd, elseCmd) => { //set translation context for guard

        outputBuilder.indent
        // Translate body of if
        outputBuilder.dedent
        //else
        outputBuilder.indent
        // Translate Body of else
        //assert guard is defined
        outputBuilder.newLine
        outputBuilder.put("//assert guard is defined")
        outputBuilder.newLine
        outputBuilder.setError("Guard is not defined, do not have anough permission(s)", guard.coord)
        outputBuilder.put("assert " + expObj.buildReadingPerExp(guard, transContext))
        transContext.reset()
        outputBuilder.put("if("+ expObj.buildBoogieExp(guard, transContext)+")")
        outputBuilder.put("{")
        outputBuilder.indent
        commandCodeGen(thenCmd)
        outputBuilder.put("}")
        outputBuilder.dedent
        outputBuilder.put("else")
        outputBuilder.put("{")
        outputBuilder.indent
        commandCodeGen(elseCmd)
        outputBuilder.dedent
        outputBuilder.put("}")

      }

      case WhileCmdNd(guard, lil, body) => { //TODO use the Loop Invariant

        var loopInvList = lil;

        guard match {
          case BooleanLiteralExpNd(b) => {
            outputBuilder.newLine
            outputBuilder.put("while( " + expObj.simpleExpCodeGen(guard) + ")")
          }
          case _ => {           // build guard expression, definedness of exxpression
            outputBuilder.newLine
            outputBuilder.put("while(" + expObj.buildBoogieExp(guard, transContext) + ")")
          }
        }
        if (lil.isEmpty) {
          transContext.reset()
          transContext.setHeap("Permission")
          outputBuilder.newLine
          outputBuilder.put("//invariant forall<x> r: Ref, f: Field x :: 0.0 <= " + transContext.getHeap() + "[r,f] <= 1.0;")
          transContext.reset()
        } else {
          for (loopInv <- lil) { loopInvCodeGen(loopInv) } // Invariant Df[guard]
        }

        outputBuilder.newLine
        outputBuilder.put("{")
        outputBuilder.indent
        commandCodeGen(body)
        outputBuilder.newLine
        outputBuilder.dedent
        outputBuilder.put("}")
      }

      case ForCmdNd(decl, repetitions, lil, body) => {
        outputBuilder.indent
        //Translate body of For loop
        outputBuilder.dedent
      }

      case CoForCmdNd(decl, repetitions, claimList, body) => {
        outputBuilder.indent
        //Translate body of Co For Command
        outputBuilder.dedent
      }

      case CoCmdNd(claimList, fstCmd, sndCmd) => {
        outputBuilder.indent
        //Translate fstCmd
        //Translate sndCmd
        outputBuilder.dedent
      }

      case AcceptCmdNd(methodImplementationList) => {
        for (mi <- methodImplementationList) {
          outputBuilder.newLine
          outputBuilder.put("goto " + mi.nameNd.toString + ";")
          outputBuilder.newLine
          outputBuilder.put(mi.nameNd.toString + ":")

          val methDecl = mi.nameNd.decl.get
          methDecl match {
            case MethodDeclNd(acc,
              paramList,
              preCndList,
              postCndList,
              givesPerList,
              takesPerList,
              borrowsPerList) => {
              // consider the parameters and borrows
              outputBuilder.indent
              methTakesPerCodeGen(acc, methDecl.fqn.toString(), paramList, takesPerList)
              methPreCondCodeGen(acc, methDecl.fqn.toString(), paramList, preCndList)
              outputBuilder.newLine
              outputBuilder.put("//Method Implementation")
              //Figure out the need of parameter list
              outputBuilder.indent
              commandCodeGen(mi.fstCmd)
              outputBuilder.dedent
              methPostCondCodeGen(acc, methDecl.fqn.toString(), paramList, postCndList)
              methGivesPerCodeGen(acc, methDecl.fqn.toString(), paramList, givesPerList)
              commandCodeGen(mi.sndCmd)
              outputBuilder.dedent
              // Go to End? end of Randezevous, omit in case of last branch
              outputBuilder.newLine
              outputBuilder.put("goto end;")
              outputBuilder.newLine
              outputBuilder.put("end:")

            }
            case _ => "" // Check TypeChecker line 279-280
          }
        }
      }

      case WithCmdNd(lock, tpl, guard, command, gpl) => {

        val lockTransContext = new TransContext("LockPermission", expObj.getNamefromLockExp(lock))
        transContext.reset()
        transContext.setHeap("Permission")
        outputBuilder.indent
        //Lock Translation
        outputBuilder.newLine;
        outputBuilder.put("var " + lockTransContext.getObjRef() + ": Ref;")
        outputBuilder.newLine
        outputBuilder.put(lockTransContext.getObjRef() + ":=" + transContext.getObjRef() + ";"); // is not it breakable?
        outputBuilder.newLine
        outputBuilder.put("preHeap := Heap;")
        outputBuilder.newLine
        outputBuilder.put("havoc tempHeap;") // havocs the locations where thread has no permission
        outputBuilder.newLine
        outputBuilder.put("havoc LockPermission;")
        transContext.reset()

        //Get permissions from invariant.
        //Collect all the locks and while assignment sum up all the lock expressions

        // assert object invariant defindness and assume object invariant
        assumeClassInv(lock)

        for (classInv <- dlNd.asInstanceOf[ClassLike].directMembers) {
          classInv match {
            case ClassInvNd(exp) => {
              lockExpList = lockExpList :+ (lockTransContext.getHeap() + "[" + lockTransContext.getObjRef() + "," + transContext.getObjRef() + "]")
            }
            case _ => ""
          }
          //ArrayBuffer, ListBuffer
        }
        //assuming all the locks sum is b/w 0.0 and 1.0
        lockExpList :+ "assume (forall<x> r: Ref, f: Field x :: 0.0 <= " + lockTransContext.getHeap() + "[r,f] <= 1.0)"

        if (isGuardPresent(guard)) {

          transContext.setHeap("Permission");

          //assert guard is defined
          outputBuilder.newLine
          outputBuilder.put("//assert guard is defined")
          outputBuilder.newLine
          outputBuilder.setError("Guard is not defined, do not have anough permission(s)", guard.coord)
          outputBuilder.put("assert " + expObj.buildReadingPerExp(guard, transContext))

          transContext.reset()
          //assume the guard
          transContext.setHeap("Heap")
          val guardExp = expObj.buildBoogieExp(guard, transContext)
          outputBuilder.newLine
          outputBuilder.put("//assume the guard expression")
          outputBuilder.newLine
          outputBuilder.put("assume " + guardExp)
          transContext.reset()
        }

        // takes permission list
        for (tp <- tpl) {
          for ((lsn, amnt) <- tp.pmn.pm) {
            lsn match {
              case ObjectIdLSN(nameExp) => {
                val amount: String = expObj.simpleExpCodeGen(amnt)

              }
              case _ => contracts.Contracts.toDo("Array Location Set Node")
            }
          }
        }
        // Body
        outputBuilder.indent
        commandCodeGen(command)
        outputBuilder.dedent

        assertClassInv(lock)

        //gives permission list

        for (gp <- tpl) {
          for ((lsn, amnt) <- gp.pmn.pm) {
            lsn match {
              case ObjectIdLSN(nameExp) => {
                val amount: String = expObj.simpleExpCodeGen(amnt)
                // not required this point
              }
              case _ => contracts.Contracts.toDo("Array Location Set Node")
            }
          }
        }
        outputBuilder.dedent
      }

      case AssertCmdNd(exp) => {
        transContext.reset()
        transContext.setHeap("Permission")
        val expReadingPerCode = expObj.buildReadingPerExp(exp, transContext)
        outputBuilder.newLine
        outputBuilder.setError("Do not have enough permission(s)", exp.coord)
        outputBuilder.put("assert " + expReadingPerCode)
        outputBuilder.newLine
        outputBuilder.clearError
        transContext.reset()
        transContext.setHeap("Heap")
        val expCode = expObj.buildBoogieExp(exp, transContext)
        outputBuilder.newLine
        outputBuilder.setError("Assertion might not hold", exp.coord)
        outputBuilder.put("assert " + expCode)
        outputBuilder.newLine
        outputBuilder.clearError
      }

      case AssumeCmdNd(exp) => {
        transContext.reset()
        transContext.setHeap("Permission")
        val expReadingPerCode = expObj.buildReadingPerExp(exp, transContext)
        outputBuilder.newLine
        outputBuilder.setError("Do not have enough permission(s)", exp.coord)
        outputBuilder.put("assert " + expReadingPerCode)
        outputBuilder.newLine
        outputBuilder.clearError
        transContext.reset()
        transContext.setHeap("Heap")
        val expCode = expObj.buildBoogieExp(exp, transContext)
        outputBuilder.newLine
        outputBuilder.put("assume " + expCode)
        outputBuilder.newLine
        outputBuilder.clearError
      }
      case _ => {}
    }
  }

  //Method's 'pre' Condition Code Generation
  def methPreCondCodeGen(acc: Access, name: String, params: List[ParamDeclNd], preCnds: List[PreCndNd]) {
    transContext.reset()
    outputBuilder.newLine
    outputBuilder.put("//Pre Condition(s)")
    outputBuilder.newLine
    outputBuilder.put("oldHeap := Heap;")
    outputBuilder.newLine
    outputBuilder.put("havoc Heap;")
    for (prc <- preCnds) {
      transContext.setHeap("Heap")
      outputBuilder.newLine
      outputBuilder.put("assume " + expObj.buildBoogieExp(prc.condition, transContext) + ";")
      outputBuilder.newLine
    }
  }

  //Method's 'takes' Specification Code Generation
  def methTakesPerCodeGen(acc: Access, name: String, params: List[ParamDeclNd], takesPers: List[TakesPerNd]) {
    transContext.reset()
    outputBuilder.newLine
    outputBuilder.put("//Taking Permission(s)")
    for (tp <- takesPers)
      for ((lsn, exp) <- tp.pmn.pm)
        lsn match {
          case ObjectIdLSN(nameExp) => {
            val amount: String = expObj.simpleExpCodeGen(exp)
            outputBuilder.newLine
            outputBuilder.put("oldPermission := Permission;")
            outputBuilder.newLine
            outputBuilder.put("if(Permission[" + transContext.objRef + "," + lsn.getName().decl.get.fqn.toString() + "] == 0.0)")
            outputBuilder.newLine
            outputBuilder.put("{")
            outputBuilder.newLine
            outputBuilder.put("havoc tempHeap;")
            outputBuilder.newLine
            outputBuilder.put("Heap[" + transContext.objRef + "," + dlNd.name + "." + lsn.getName() + "] := tempHeap[" + transContext.objRef + "," + dlNd.name + "." + lsn.getName() + "];")
            outputBuilder.newLine
            outputBuilder.put("Permission[" + transContext.objRef + "," + dlNd.name + "." + lsn.getName() + "] := " + "Permission[" + transContext.objRef + "," + dlNd.name + "." + lsn.getName() + "]+" + amount + ";")
            outputBuilder.newLine
            outputBuilder.put("}")
          }
          case _ => contracts.Contracts.toDo("Array Location Set Node")
        }
  }

  //Method's 'post' Condition Code Generation
  def methPostCondCodeGen(acc: Access, name: String, params: List[ParamDeclNd], postCnds: List[PostCndNd]) {
    transContext.reset()
    outputBuilder.newLine
    outputBuilder.put("//Post Condition(s)")
    for (poc <- postCnds) {
      val exp = poc.condition;
      val tempObj = new ExpCodeGen;
      outputBuilder.newLine
      nameExp = new ArrayBuffer[String]()
      val nameExps = tempObj.nameExpCodeGen(exp, nameExp)
      for (name <- nameExps) {
        outputBuilder.newLine
        outputBuilder.setError("Permission amount should be greater than 0.0", poc.condition.coord)
        outputBuilder.put("assert Permission[" + transContext.objRef + "," + name + "] > 0.0;")
        outputBuilder.newLine
        outputBuilder.clearError
      }
      outputBuilder.newLine
      outputBuilder.setError("Post Condition does not satisfy", poc.condition.coord)
      outputBuilder.put("assert " + expObj.buildBoogieExp(poc.condition, transContext) + ";") // assume and assert
      outputBuilder.newLine
      outputBuilder.clearError
    }
  }

  //Method's 'gives' Specification Code Generation

  def methGivesPerCodeGen(acc: Access, name: String, params: List[ParamDeclNd], givesPers: List[GivesPerNd]) {
    outputBuilder.newLine
    outputBuilder.put("//Giving Permissions(s)")
    transContext.reset()
    transContext.setHeap("Permission")
    for (tp <- givesPers)
      for ((lsn, amnt) <- tp.pmn.pm)
        lsn match {
          case ObjectIdLSN(nameExp) => {
            val amount = expObj.simpleExpCodeGen(amnt)
            // Assert at least permission, and then the subtract the permission
            outputBuilder.newLine
            //assert the amount of permission at least the amount going to subtract
            outputBuilder.setError("Can not give permission(s)", lsn.getName().coord)
            outputBuilder.put("assert " + expObj.buildBoogieExp(nameExp, transContext) + " >= " + amount + ";")
            // Subtract the Permission
            outputBuilder.newLine
            outputBuilder.put(expObj.buildBoogieExp(nameExp, transContext) + " := " + expObj.buildBoogieExp(nameExp, transContext) + " - " + amount + ";");
            outputBuilder.newLine
            outputBuilder.clearError
          }
          case _ => contracts.Contracts.toDo("Array Location Set Node")
        }
  }

  def assertClassInvariant() {
    outputBuilder.newLine;
    outputBuilder.put("//Asserting class invariant for Locked object")
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ClassInvNd(exp) => {
          val invString = expObj.InvExpCodeGen(exp, mem.fqn.toString, transContext)
          outputBuilder.newLine
          outputBuilder.setError("Invariant does not hold", exp.coord)
          outputBuilder.put("assert " + invString)
          outputBuilder.newLine
          outputBuilder.clearError
        }

        case _ => {}
      }
    }
  }

  def assumeClassInvariant() {
    outputBuilder.newLine;
    outputBuilder.put("//Asserting class invariant for Locked object")
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ClassInvNd(exp) => {
          val invString = expObj.InvExpCodeGen(exp, mem.fqn.toString, transContext)
          outputBuilder.newLine
          outputBuilder.put("assume " + invString)
          outputBuilder.newLine
        }

        case _ => {}
      }
    }
  }

  //Thread Claim Code Generation

  def claimCodeGen(claimNd: ClaimNd) {
    val perm_pairs = claimNd.pmn.pm;
    var result = "";
    for ((loc, amnt) <- perm_pairs) {
      var name = "";
      loc match {
        case ObjectIdLSN(nen) => {
          name = dlNd.name + "." + nen.name.toString();
        }
        case _ => contracts.Contracts.toDo("Location Set Node with Array")
      }
      val amount: String = expObj.simpleExpCodeGen(amnt)
      outputBuilder.newLine
      outputBuilder.put("//Claim")
      outputBuilder.newLine
      outputBuilder.put("Permission[" + transContext.objRef + "," + name + "] := Permission[" + transContext.objRef + "," + name + "] + " + amount + ";")
    }
  }

  private def classInvCodeGen() {
    outputBuilder.newLine
    outputBuilder.put("//Class Invariant")
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
      mem match {
        case ClassInvNd(exp) => {
          classInvList :+ mem;
          val invString = expObj.InvExpCodeGen(exp, dlNd.fqn.toString, transContext)
          outputBuilder.newLine
          outputBuilder.setError("Invariant does not hold", exp.coord)
          outputBuilder.put("assert " + invString + ";")
          outputBuilder.newLine
          outputBuilder.clearError
        }
        case _ => {}
      }
  }

  private def loopInvCodeGen(loopInv: LoopInvNd) {
    //Check Loop Invariant is Defined
    transContext.reset()
    outputBuilder.newLine
    outputBuilder.put("//loop Invariant")
    val invString = expObj.InvExpCodeGen(loopInv.exp, dlNd.fqn.toString, transContext)
    outputBuilder.newLine
    outputBuilder.setError("Invariant does not hold", loopInv.exp.coord)
    outputBuilder.put("assert " + invString)
    outputBuilder.newLine
    outputBuilder.clearError
  }

  def isAssignmentDefined(lhsList: List[ExpNd], rhsList: List[ExpNd]) {
    outputBuilder.newLine
    outputBuilder.put("//Check Assignment Defindness")
    transContext.reset()
    val localTransContext = transContext;
    localTransContext.setHeap("Permission")
    val expPairs = lhsList zip rhsList
    for ((lhs, rhs) <- expPairs) {
      val lhs_result = expObj.buildWritingPerExp(lhs, localTransContext)
      outputBuilder.newLine
      outputBuilder.setError("Do not have enough permission(s) on LHS of assignment", lhs.coord)
      outputBuilder.put("assert " + lhs_result) // Two asserts instead of one
      outputBuilder.newLine
      outputBuilder.clearError
      nameExp = new ArrayBuffer[String]()

      val nameExps = expObj.nameExpCodeGen(rhs, nameExp)
      for (name <- nameExps) {
        outputBuilder.newLine
        outputBuilder.setError("Permission amount should be greater than 0.0", rhs.coord)
        outputBuilder.put("assert Permission[" + localTransContext.getObjRef() + "," + name + "] > 0.0;")
        outputBuilder.newLine
        outputBuilder.clearError
      }
    }
    //val rhs_result = expObj.buildReadingPerExp(rhs, localTransContext)
    // one glitch while translating the IntLiteralExpressionNode
    outputBuilder.newLine
    outputBuilder.put("//Check Assignment Defindness Ends")

  }

  def isGuardPresent(exp: ExpNd): Boolean = {

    exp match {
      case NameExpNd(name: NameNd) => true

      case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => true

      case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => true

      case MemberExpNd(x: ExpNd, name: String) => true

      case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => true

      case FetchExpNd(x: ExpNd) => true

      case AsExpNd(x: ExpNd, _) => true

      case CanReadOp(x) => true

      case CanWriteOp(y) => true

      case PermissionOp(x) => true

      case _ => false
    }

  }
  def assumeClassInv(lock: ExpNd) { // For locked objects
    val lockContext = new TransContext("LockPermission", expObj.getNamefromLockExp(lock));
    transContext.reset()
    val baseContext = new TransContext(transContext.getHeap(), transContext.getObjRef())
    for (classInv <- dlNd.asInstanceOf[ClassLike].directMembers) {
      classInv match {
        case ClassInvNd(exp) => {
          // assert defindness
          outputBuilder.newLine
          outputBuilder.put("//Assert defindness of object invariant and assume object invariance")
          outputBuilder.newLine
          nameExp = new ArrayBuffer[String]()
          val nameExps = expObj.nameExpCodeGen(exp, nameExp)
          if (!(nameExps.isEmpty)) {
            outputBuilder.setError("Does not have enough permission(0)", exp.coord)
            outputBuilder.newLine
            outputBuilder.put("assert ");
            for (name <- nameExps)
              outputBuilder.put(lockContext.heap + "[" + lockContext.getObjRef() + "," + name + "] > 0.0;") // generating && true extra , need to fix this glitch
            outputBuilder.newLine
            outputBuilder.clearError
          }

          //assume invariant
          transContext.reset()
          outputBuilder.newLine
          outputBuilder.put("assume " + expObj.InvExpCodeGen(exp, classInv.fqn.toString, lockContext, transContext) + ";") // due to lock we need different invariant generation
          outputBuilder.newLine

        }
        case _ =>
      }
    }
  }

  def assertClassInv(lock: ExpNd) { //For Locked objects
    val lockContext = new TransContext("LockPermission", expObj.getNamefromLockExp(lock));
    transContext.reset()
    val baseContext = new TransContext(transContext.getHeap(), transContext.getObjRef())
    for (classInv <- dlNd.asInstanceOf[ClassLike].directMembers) {
      classInv match {
        case ClassInvNd(exp) => {
          // assert defindness
          outputBuilder.newLine
          outputBuilder.put("//Assert defindness of object invariant and assert object invariance")
          outputBuilder.newLine
          nameExp = new ArrayBuffer[String]()
          val nameExps = expObj.nameExpCodeGen(exp, nameExp)
          if (!(nameExps.isEmpty)) {
            outputBuilder.setError("Does not have enough permission(0)", exp.coord)
            outputBuilder.newLine
            outputBuilder.put("assert ");
            for (name <- nameExps)
              outputBuilder.put(lockContext.heap + "[" + lockContext.getObjRef() + "," + name + "] > 0.0;") // generating && true extra , need to fix this glitch
            outputBuilder.newLine
            outputBuilder.clearError
          }
          //assert invariant
          transContext.reset()
          outputBuilder.newLine
          outputBuilder.put("assert " + expObj.InvExpCodeGen(exp, classInv.fqn.toString, lockContext, transContext) + ";") // due to lock we need different invariant generation
          outputBuilder.newLine
        }
        case _ =>
      }
    }
  }

  def objDeclCodeGen(isConst: Boolean, acc: Access, ty: TypeNd, fqn: String): String = {
    val objType: String = TypeCodeGen(ty)
    val objCode = "\nconst unique " + fqn + ":" + "Field " + objType + ";"
    return objCode
  }

}
