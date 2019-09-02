package boogieBackEnd;
import frontEnd.AST;
import frontEnd.AST._;
import util.Format;
import util.OutputBuilder;
import contracts.Contracts;
import checker.CheckerTypes._
import scala.collection.mutable.StringBuilder;
import frontEnd.AST.ClassInvNd;
import frontEnd.AST.LoopInvNd;
import scala.collection.mutable.ArrayBuffer;

private class ClassCodeGen(val dlNd: DeclNd, outputBuilder: OutputBuilder) {

  //global translation context has most generic form of heap and object reference,
  //local translation contexts are made while changing to particular Heap and object reference i.e preHeap, tempHeap, oldHeap, and *_this, *_that

  private var heapTransContext = new TransContext("Heap", "This." + dlNd.fqn.toString)

  private var arrayHeapTransContext = new TransContext("ArrayHeap", "This." + dlNd.fqn.toString)

  private var nameExp = ArrayBuffer[String](); // Collect Names used in Expression

  /* Collect the invariant,local Declarations, class claim and global objects
     from List, to avoid traversing the list and finding node at various subroutines of code generation */

  private var classInvList = List[ClassInvNd]();

  private var localDeclNdList = List[LocalDeclNd]();

  private var classClaimList = List[ClaimNd]();

  private var globalObjectsList = List[ObjDeclNd]();

  private var methodDeclList = List[MethodDeclNd]();

  private var newObject = 0;

  private var lockExpList = List[String]() //lock expression must be an object implementing interface lock

  def getOutputBuilder(): OutputBuilder = outputBuilder;

  private var savedLine = outputBuilder.saveLine();

  def classCodeGen(): Unit = {

    classIdentifierCodeGen()

    objectIdentifierCodeGen()

    constructorProcedureCodeGen()

    threadProcedureCodeGen()

  }

  // Class Identifier Code Generation
  private def classIdentifierCodeGen() {

    val interfaceName = dlNd.asInstanceOf[ClassLike].superTypes;
    if (!(interfaceName.isEmpty)) {
      outputBuilder.putln("const unique " + dlNd.fqn.toString + ":ClassName <: " + interfaceName(0) + ";")
    } else {
      outputBuilder.putln("const unique " + dlNd.fqn.toString + ":ClassName;")
    }
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
          ty.tipe.get match {
            case ArrayLocationType(baseType) => {
              outputBuilder.putln("const unique " + mem.fqn + ":" + "Field(ArrayRef " + objType + ");")
            }
            case _ => {
              outputBuilder.putln("const unique " + mem.fqn + ":" + "Field " + objType + ";")
            }
          }
          if (isConst) {
            outputBuilder.putln("axiom " + mem.fqn + " == " + ExpCodeGen.initExpCodeGen(init, heapTransContext) + ";") //TODO evaluate the expression and get a final value
          }
        }
        case decl @ MethodDeclNd(acc, params, preCnds, postCnds, givesPers, takesPers, borrowsPers) => {
          paramDeclCodeGen(params)
        }
        case ThreadDeclNd(claimList: List[ClaimNd], block: CommandNd) => {
          collectLocalVarDecls(block)
          for (localVarDecl <- localDeclNdList) {
            promoteLocalVarDeclToClassField(localVarDecl)
          }
        }
        case a @ ClassDeclNd() => paramDeclCodeGen(a.constructorParams.toList) // Reachable?
        case _ => {}
      }

    def collectLocalVarDecls(block: CommandNd) {

      block match {

        case SeqCommandNd(fstCmd: CommandNd, sndCmd: CommandNd) => {
          collectLocalVarDecls(fstCmd)
          collectLocalVarDecls(sndCmd)
        }

        case LocalDeclCmdNd(decl: LocalDeclNd) => localDeclNdList :+ decl

        case AssignmentCmdNd(lhs, rhs) => None // Assignment is not the object initialization case

        case CallCmdNd(method: ExpNd, argList) => None

        case IfCmdNd(guard: ExpNd, thenCmd: CommandNd, elseCmd: CommandNd) => {
          collectLocalVarDecls(thenCmd)
          collectLocalVarDecls(elseCmd)

        }

        case WhileCmdNd(guard: ExpNd, lil: List[LoopInvNd], body: CommandNd) => {
          collectLocalVarDecls(body)
        }

        case ForCmdNd(decl: ForDecl, repetitions: ExpNd, lil: List[LoopInvNd], body: CommandNd) => {
          localDeclNdList :+ decl.fvd
          collectLocalVarDecls(body)
        }

        case CoForCmdNd(decl: ForDecl, repetitions: ExpNd, cl: List[ClaimNd], body: CommandNd) => {
          collectLocalVarDecls(body)
        }

        case CoCmdNd(cl: List[ClaimNd], fstCmd: CommandNd, sndCmd: CommandNd) => {
          collectLocalVarDecls(fstCmd)
          collectLocalVarDecls(sndCmd)
        }

        case AcceptCmdNd(methodImplementationList: List[MethodImplementationDeclNd]) => {
          for (mi <- methodImplementationList) {
            collectLocalVarDecls(mi.fstCmd)
            collectLocalVarDecls(mi.sndCmd)
          }

        }

        case WithCmdNd(lock: ExpNd, tpl: List[TakesPerNd], guard: ExpNd, command: CommandNd, gpl: List[GivesPerNd]) => {
          collectLocalVarDecls(command)
        }

        case _ => None

      }
    }

    def promoteLocalVarDeclToClassField(decl: LocalDeclNd) {

      val objType: String = TypeCodeGen(decl.ty)
      decl.ty.tipe.get match {
        case ArrayLocationType(baseType) => {
          outputBuilder.putln("const unique " + decl.fqn.toString() + ":" + "Field(ArrayRef " + objType + ");")
        }
        case _ => {
          outputBuilder.putln("const unique " + decl.fqn.toString() + ":" + "Field " + objType + ";")
        }
      }
      if (decl.isConst) {
        outputBuilder.putln("axiom " + decl.fqn.toString() + " == " + ExpCodeGen.simpleExpCodeGen(decl.init) + ";") //TODO evaluate the expression and get a final value
      }
    }
  }
  // Constructor Procedure Code Generation

  private def constructorProcedureCodeGen() {

    //TODO havoc the heap with arbitrary values - Done
    //TODO Add permissions to the input parameters of the constructor
    //TODO Check the sum of claims on each field is not more than one -> while claiming assert the final value of permission after addition
    //TODO Initialize the fields of the class with their initial values - Done
    //TODO Increase the permission of the lock based on the permission map in claim specification - Done
    //TODO Constructor Specifications
    val classDeclNd = dlNd.asInstanceOf[ClassDeclNd]

    outputBuilder.putln("procedure " + dlNd.fqn.toString + ".constructor" + "(" + heapTransContext.objRef + ":Ref)") // dlNd.fqn is class name
    outputBuilder.putln("requires dtype(" + heapTransContext.objRef + ") <: " + dlNd.fqn.toString + ";")
    outputBuilder.putln("modifies " + heapTransContext.getHeap() + ";")
    outputBuilder.putln("modifies " + arrayHeapTransContext.getHeap() + ";")
    outputBuilder.putln("modifies Permission, ArrayPermission, LockPermission, ArrayLockPermission;")
    outputBuilder.putln("{")
    outputBuilder.indent
    outputBuilder.putln("var oldHeap, preHeap, tempHeap:HeapType;")
    outputBuilder.putln("havoc " + heapTransContext.getHeap() + ";")
    //Class Claim Code Generation
    classClaimCodeGen()
    //Objects initialization Code Generation, It Initializes the Heap
    objectsInitCodeGen(classDeclNd)
    //Class Invariant Code
    classInvCodeGen()
    //Closing of Constructor Procedure
    outputBuilder.dedent
    outputBuilder.putln("}")

  }

  //Thread Procedure Code Generation

  private def threadProcedureCodeGen() {
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
      mem match {
        case ThreadDeclNd(claimList: List[ClaimNd], block: CommandNd) => {
          outputBuilder.putln("procedure " + dlNd.name + "." + mem.name + " (" + heapTransContext.objRef + " : Ref)")
          outputBuilder.putln("requires dtype(" + heapTransContext.objRef + ") <: " + dlNd.fqn.toString + ";")
          outputBuilder.putln("modifies " + heapTransContext.getHeap() + ";")
          outputBuilder.putln("modifies LockPermission;")
          outputBuilder.putln("{")
          outputBuilder.indent
          outputBuilder.putln("var local_Permission : PermissionType where (forall <x> r: Ref, f: Field x :: Permission[r,f] == 0.0);")
          outputBuilder.putln("var local_oldPermission: PermissionType;")
          outputBuilder.putln("var LockPermission : PermissionType where (forall <x> r: Ref, f: Field x :: LockPermission[r,f] == 0.0);")
          outputBuilder.putln("var local_ArrayPermission : ArrayPermissionType where (forall <x> r: ArrayRef x, f: int :: ArrayPermission[r,f] == 0.0);")
          outputBuilder.putln("var local_OldArrayPermission: PermissionType;")
          outputBuilder.putln("var ArrayLockPermission : ArrayPermissionType where (forall <x> r: ArrayRef x, f: int :: ArrayLockPermission[r,f] == 0.0);")
          outputBuilder.putln("var oldHeap, preHeap, tempHeap: HeapType;")
          outputBuilder.putln("var oldArrayHeap, preArrayHeap, tempArrayHeap: ArrayHeapType;")

          savedLine = outputBuilder.saveLine() // save current line of outputBuilder

          // get all the lock variables  declared here
          //Thread Claim Code Generation
          for (claim <- claimList)
            claimCodeGen(claim)
          commandCodeGen(block) //Thread Body Code Generation
          outputBuilder.dedent
          outputBuilder.putln("}//end of Thread Procedure")
        }

        case _ => {}
      }

    def commandCodeGen(cmd: CommandNd) {

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
          isExpressionDefined(decl.init)
          outputBuilder.newLine
          outputBuilder.put("Heap[" + heapTransContext.objRef + "," + decl.fqn.toString() + "] := " + ExpCodeGen.buildBoogieExp(decl.init, heapTransContext) + ";")
          outputBuilder.newLine
          commandCodeGen(decl.cmd)
        }

        case AssignmentCmdNd(lhs, rhs) => {

          // Sum up all locked Permissions first, later put it in assignment defindness
          // Assert Write Permission on LHS and ReadPermission on LHS
          //isAssignmentDefined(lhs.toList, rhs.toList)

          outputBuilder.newLine
          outputBuilder.put("//Check Assignment Defindness")
          val expPairs = lhs zip rhs
          for ((lhs, rhs) <- expPairs) {
            outputBuilder.newLine
            outputBuilder.put("//Expression Definedness Start")
            isExpressionDefined(lhs)
            isExpressionDefined(rhs)
            outputBuilder.newLine
            outputBuilder.put("//Expression Definedness Ends")
          }
          outputBuilder.newLine
          heapTransContext.reset()
          val localTransContext = heapTransContext;
          localTransContext.setHeap("Permission")

          for ((lhs, rhs) <- expPairs) {
            val lhs_result = ExpCodeGen.buildWritingPerExp(lhs, localTransContext)
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
            val nameExps = ExpCodeGen.nameExpCodeGen(rhs, nameExp)
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
                  outputBuilder.put("Permission[" + localTransContext.getObjRef() + "," + name + "] > 0.0" + " && ")
                outputBuilder.put("Permission[" + localTransContext.getObjRef() + "," + nameExps.last + "] > 0.0;")
                outputBuilder.newLine
                outputBuilder.clearError
              }
            }
          }
          //val rhs_result = ECG.assertReadingPermission(rhs, localTransContext)
          // one glitch while translating the IntLiteralExpressionNode
          outputBuilder.newLine
          outputBuilder.put("//Check Assignment Defindness Ends")

          // assignment translation context
          heapTransContext.reset()
          outputBuilder.newLine
          outputBuilder.put("//Assignment Command")
          outputBuilder.newLine
          for (l_exp <- lhs.init) {
            outputBuilder.put(ExpCodeGen.buildBoogieExp(l_exp, heapTransContext)) // build with TransContext
            if (!(l_exp.equals(lhs.last))) // Not to put comma for last expression in sequence
              outputBuilder.put(",")
          }
          outputBuilder.put(ExpCodeGen.buildBoogieExp(lhs.last, heapTransContext))
          outputBuilder.put(" := ")

          for (r_exp <- rhs.init) {
            outputBuilder.put(ExpCodeGen.buildBoogieExp(r_exp, heapTransContext))
            if (!(r_exp.equals(rhs.last))) // Not to put comma for last expression in sequence
              outputBuilder.put(",")
          }
          outputBuilder.put(ExpCodeGen.buildBoogieExp(rhs.last, heapTransContext))
          outputBuilder.put(";")
          outputBuilder.newLine
          outputBuilder.put("//Assignment Command Ends")
        }

        case CallCmdNd(method, argList) => {
          outputBuilder.putln("preHeap := Heap;")
          outputBuilder.putln("preArrayHeap := ArrayHeap;")
          for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
            mem match {
              case decl @ MethodDeclNd(acc, params, preCnds, postCnds, givesPers, takesPers, borrowsPers) => {
                methodDeclList :+ decl
              }
              case _ =>
            }
          }
          for (decl <- methodDeclList) outputBuilder.putln("Method Declarations " + decl.name)
          //Arguments Definedness
          for (arg <- argList) {
            checkArgumentsDefindness(arg)
          }
          
          def checkArgumentsDefindness(arg: ExpNd) {
            arg match {
              case NameExpNd(nameNd) => {
                outputBuilder.goToAfter(savedLine)
                outputBuilder.putln("var param_" + newObject + ":= Ref;")
                outputBuilder.goToEnd()
                outputBuilder.putln("param_" + newObject + ExpCodeGen.buildBoogieExp(arg, heapTransContext))
                newObject = newObject + 1
              }
              case IntLiteralExpNd(i) => isExpressionDefined(arg)
              case FloatLiteralExpNd(d) => isExpressionDefined(arg)
              case FetchExpNd(x) => checkArgumentsDefindness(arg)
              case _ => contracts.Contracts.toDo("Add Members arguments and Expressions")
            }
          }

        }

        case IfCmdNd(guard, thenCmd, elseCmd) => { //set translation context for guard
          // Translate Body of else
          //assert guard is defined
          outputBuilder.putln("//assert guard is defined")
          isExpressionDefined(guard)
          outputBuilder.setError("Guard is not defined, do not have anough permission(s)", guard.coord)
          outputBuilder.put(ExpCodeGen.assertReadingPermission(guard, heapTransContext))
          heapTransContext.reset()
          outputBuilder.putln("if(" + ExpCodeGen.buildBoogieExp(guard, heapTransContext) + ")")
          outputBuilder.putln("{")
          outputBuilder.indent
          commandCodeGen(thenCmd)
          outputBuilder.putln("}")
          outputBuilder.dedent
          elseCmd match {
            case SkipCmdNd() => {}
            case _ => {
              outputBuilder.putln("else")
              outputBuilder.putln("{")
              outputBuilder.indent
              commandCodeGen(elseCmd)
              outputBuilder.dedent
              outputBuilder.putln("}")
            }
          }

        }

        case WhileCmdNd(guard, lil, body) => { //TODO use the Loop Invariant

          var loopInvList = lil;

          isExpressionDefined(guard)

          guard match {
            case BooleanLiteralExpNd(b) => {
              outputBuilder.newLine
              outputBuilder.put("while( " + ExpCodeGen.simpleExpCodeGen(guard) + ")")
            }
            case _ => { // build guard expression, definedness of expression
              outputBuilder.newLine
              outputBuilder.put("while(" + ExpCodeGen.buildBoogieExp(guard, heapTransContext) + ")")
            }
          }
          if (lil.isEmpty) {
            heapTransContext.reset()
            heapTransContext.setHeap("Permission")
            outputBuilder.newLine
            outputBuilder.put("invariant (forall<x> r: Ref, f: Field x :: 0.0 <= " + heapTransContext.getHeap() + "[r,f]" + " && " + heapTransContext.getHeap() + "[r,f] <= 1.0);")
            heapTransContext.reset()
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
          val counter = heapTransContext.getHeap() + "[" + heapTransContext.getObjRef() + "," + decl.fqn.toString() + "]";
          val bound = ExpCodeGen.buildBoogieExp(repetitions, heapTransContext)
          outputBuilder.indent
          outputBuilder.put("oldHeap := Heap;");
          outputBuilder.newLine
          outputBuilder.put("oldArrayHeap := ArrayHeap;")
          outputBuilder.newLine
          outputBuilder.put(s"while ( ${counter} < ${bound} )")
          outputBuilder.newLine
          for (loopInv <- lil) loopInvCodeGen(loopInv)
          outputBuilder.put("{")
          outputBuilder.indent
          commandCodeGen(body)
          outputBuilder.newLine
          outputBuilder.put(s"${counter} := ${counter} + 1;")
          outputBuilder.newLine
          outputBuilder.dedent
          outputBuilder.put("}")

          //Translate body of For loop

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

          val lockTransContext = new TransContext("LockPermission", ExpCodeGen.getNamefromLockExp(lock))
          heapTransContext.reset()
          heapTransContext.setHeap("Permission")
          outputBuilder.indent
          //Lock Translation

          outputBuilder.goToAfter(savedLine) // goto after saved line
          outputBuilder.newLine;
          outputBuilder.put("var " + lockTransContext.getObjRef() + ": Ref;")
          outputBuilder.newLine
          outputBuilder.goToEnd()
          outputBuilder.newLine
          outputBuilder.put(lockTransContext.getObjRef() + ":=" + heapTransContext.getObjRef() + ";"); // is not it breakable?
          outputBuilder.newLine
          // goto last line of the outputBuilder

          outputBuilder.put("preHeap := Heap;")
          outputBuilder.newLine
          outputBuilder.put("havoc tempHeap;") // havocs the locations where thread has no permission
          outputBuilder.newLine
          outputBuilder.put("havoc LockPermission;")
          heapTransContext.reset()

          //Get permissions from invariant.
          //Collect all the locks and while assignment sum up all the lock expressions

          // assert object invariant defindness and assume object invariant
          lock match {
            case ThisExpNd(str) => {
              for (classInv <- dlNd.asInstanceOf[ClassLike].directMembers)
                classInv match {
                  case ClassInvNd(exp) => {
                    val expList$ = ArrayBuffer[String]();
                    lockExpList = ExpCodeGen.lockedInvExpCodeGen(exp, lockTransContext, expList$)
                  }
                  case _ => ""
                }
            }
            case NameExpNd(nameNd) => {
              ExpCodeGen.buildBoogieExp(lock, heapTransContext)
            }
            case _ => {}
          }
          lockedClassInv(lock, "assume")

          //assuming all the locks sum is b/w 0.0 and 1.0
          lockExpList :+ "assume (forall<x> r: Ref, f: Field x :: 0.0 <= " + lockTransContext.getHeap() + "[r,f] <= 1.0)"

          if (isGuardPresent(guard)) {

            heapTransContext.setHeap("Permission");

            //assert guard is defined
            outputBuilder.newLine
            outputBuilder.put("//assert guard is defined")
            outputBuilder.newLine
            outputBuilder.setError("Guard is not defined, do not have anough permission(s)", guard.coord)
            outputBuilder.put(ExpCodeGen.assertReadingPermission(guard, heapTransContext))
            outputBuilder.newLine
            outputBuilder.clearError

            heapTransContext.reset()
            //assume the guard
            heapTransContext.setHeap("Heap")
            val guardExp = ExpCodeGen.buildBoogieExp(guard, heapTransContext)
            outputBuilder.newLine
            outputBuilder.put("//assume the guard expression")
            outputBuilder.newLine
            outputBuilder.put("assume " + guardExp)
            heapTransContext.reset()
          }

          // takes permission list
          for (tp <- tpl) {
            for ((lsn, amnt) <- tp.pmn.pm) {
              lsn match {
                case ObjectIdLSN(nameExp) => {
                  val amount: String = ExpCodeGen.simpleExpCodeGen(amnt)

                }
                case _ => contracts.Contracts.toDo("Array Location Set Node")
              }
            }
          }
          // Body
          outputBuilder.indent
          commandCodeGen(command)
          outputBuilder.dedent

          lockedClassInv(lock, "assert")

          //gives permission list

          for (gp <- tpl) {
            for ((lsn, amnt) <- gp.pmn.pm) {
              lsn match {
                case ObjectIdLSN(nameExp) => {
                  val amount: String = ExpCodeGen.simpleExpCodeGen(amnt)
                  // not required this point
                }
                case _ => contracts.Contracts.toDo("Array Location Set Node")
              }
            }
          }
          outputBuilder.dedent
        }

        case AssertCmdNd(exp) => {
          outputBuilder.newLine
          outputBuilder.put("//Expression Definedness Start")
          isExpressionDefined(exp)
          outputBuilder.newLine
          outputBuilder.put("//Expression Definedness Ends")
          heapTransContext.reset()
          heapTransContext.setHeap("Permission")
          val nameList = new ArrayBuffer[String];
          val nameList$ = ExpCodeGen.nameExpCodeGen(exp, nameList)
          if (nameList.nonEmpty) {
            outputBuilder.newLine
            outputBuilder.setError("Do not have enough permission(s)", exp.coord)
            outputBuilder.put(ExpCodeGen.assertReadingPermission(exp, heapTransContext))
            outputBuilder.newLine
            outputBuilder.clearError
          }

          heapTransContext.reset()
          heapTransContext.setHeap("Heap")
          val expCode = ExpCodeGen.buildBoogieExp(exp, heapTransContext)
          outputBuilder.newLine
          outputBuilder.setError("Assertion might not hold", exp.coord)
          outputBuilder.put("assert " + expCode + ";")
          outputBuilder.newLine
          outputBuilder.clearError
          heapTransContext.reset()
        }

        case AssumeCmdNd(exp) => {
          outputBuilder.newLine
          outputBuilder.put("//Expression Definedness Start")
          isExpressionDefined(exp)
          outputBuilder.newLine
          outputBuilder.put("//Expression Definedness Ends")
          heapTransContext.reset()
          heapTransContext.setHeap("Permission")
          outputBuilder.newLine
          outputBuilder.setError("Do not have enough permission(s)", exp.coord)
          outputBuilder.put(ExpCodeGen.assertReadingPermission(exp, heapTransContext))
          outputBuilder.newLine
          outputBuilder.clearError
          heapTransContext.reset()
          heapTransContext.setHeap("Heap")
          outputBuilder.newLine
          outputBuilder.put("assume " + ExpCodeGen.buildBoogieExp(exp, heapTransContext) + ";")
          outputBuilder.newLine
          outputBuilder.clearError
          heapTransContext.reset()
        }
        case _ => {}
      }
    }

    def isExpressionDefined(exp: ExpNd) {
      exp match {
        case NameExpNd(name: NameNd) => {
          val decl$ = name.decl.get
          decl$ match {
            case ObjDeclNd(isGhost, isConst, acc, ty, init) => {
              val hasType = getBaseOfPrimitiveType(ty.tipe.get);
              outputBuilder.setError("Expresion is not defined", decl$.coord)
              outputBuilder.putln("assert is" + hasType + "(" + heapTransContext.getHeap() + "[" + heapTransContext.getObjRef() + "," + decl$.fqn.toString() + "]" + ");")
              outputBuilder.clearError
            }
            case ParamDeclNd(isGhost, ty, paramCategory) => {
              val hasType = getBaseOfPrimitiveType(ty.tipe.get);
              outputBuilder.setError("Expresion is not defined", decl$.coord)
              outputBuilder.putln("assert is" + hasType + "(" + heapTransContext.getHeap() + "[" + heapTransContext.getObjRef() + "," + decl$.fqn.toString() + "]" + ");")
              outputBuilder.clearError
            }
            case LocalDeclNd(isGhost, isConst, ty, init, cmd) => {
              val hasType = getBaseOfPrimitiveType(ty.tipe.get);
              outputBuilder.setError("Expresion is not defined", decl$.coord)
              outputBuilder.putln("assert is" + hasType + "(" + heapTransContext.getHeap() + "[" + heapTransContext.getObjRef() + "," + decl$.fqn.toString() + "]" + ");")
              outputBuilder.clearError
            }
            case ThreadDeclNd(claimList, block) => {
              isCommandDefined(block)
            }
            case _ =>
          }
        }
        case MemberExpNd(x, name) => {
          isExpressionDefined(x)
          outputBuilder.goToAfter(savedLine)
          outputBuilder.newLine
          outputBuilder.putln("var that_" + newObject + ": Ref;")
          outputBuilder.putln("that_" + newObject + " := " + ExpCodeGen.buildBoogieExp(x, heapTransContext))
          newObject = newObject + 1
          outputBuilder.goToEnd()
        }

        case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => {
          val denom_exp = ExpCodeGen.buildBoogieExp(y, heapTransContext);
          op match {
            case SlashDivOp => outputBuilder.putln(s"assert ${denom_exp} != 0;")
            case WordDivOp => outputBuilder.putln(s"assert ${denom_exp} != 0;")
            case RemOp => outputBuilder.putln(s"assert ${denom_exp} != 0;")
            case _ => 
          }
          isExpressionDefined(x)
          isExpressionDefined(y)
        }

        case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => {
          isExpressionDefined(x)
        }

        case MemberExpNd(x: ExpNd, name: String) => {
          isExpressionDefined(x)
        }

        case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => {
          for (x <- operands) isExpressionDefined(x)
        }
        case FetchExpNd(x: ExpNd) => isExpressionDefined(x)

        case AsExpNd(x: ExpNd, _) => isExpressionDefined(x)

        case intLiteral @ IntLiteralExpNd(i) => {

          val hasType = getBaseOfPrimitiveType(intLiteral.tipe.get);
          outputBuilder.newLine
          outputBuilder.setError("Expresion is not defined", intLiteral.coord)
          outputBuilder.put("assert is" + getBaseOfPrimitiveType(intLiteral.tipe.get) + "(" + i + ");")
          outputBuilder.newLine
          outputBuilder.clearError
        }

        case floatLiteral @ FloatLiteralExpNd(l) => {
          val hasType = getBaseOfPrimitiveType(floatLiteral.tipe.get);
          outputBuilder.newLine
          outputBuilder.setError("Expresion is not defined", floatLiteral.coord)
          outputBuilder.put("assert is" + getBaseOfPrimitiveType(floatLiteral.tipe.get) + "(" + l + ");")
          outputBuilder.newLine
          outputBuilder.clearError
        }

        case boolLiteral @ BooleanLiteralExpNd(b) => {
          val hasType = getBaseOfPrimitiveType(boolLiteral.tipe.get);
          outputBuilder.newLine
          outputBuilder.setError("Expresion is not defined", boolLiteral.coord)
          outputBuilder.put("assert is" + getBaseOfPrimitiveType(boolLiteral.tipe.get) + "(" + b + ");")
          outputBuilder.newLine
          outputBuilder.clearError
        }
        case _ =>
      }

    }

    def isCommandDefined(cmd: CommandNd) {

      cmd match {

        case SeqCommandNd(fstCmd: CommandNd, sndCmd: CommandNd) => {
          isCommandDefined(fstCmd)
          isCommandDefined(sndCmd)
        }

        case LocalDeclCmdNd(decl: LocalDeclNd) => {
          //TODO
        }

        case AssignmentCmdNd(lhs, rhs) => {
          //TODO
        }

        case CallCmdNd(method, argList) => {
          for (arg <- argList) isExpressionDefined(arg)
        }

        case IfCmdNd(guard: ExpNd, thenCmd: CommandNd, elseCmd: CommandNd) => {
          //TODO
        }

        case WhileCmdNd(guard: ExpNd, lil: List[LoopInvNd], body: CommandNd) => {
          //TODO
        }

        case ForCmdNd(decl: ForDecl, repetitions: ExpNd, lil: List[LoopInvNd], body: CommandNd) => {
          //TODO
        }

        case CoForCmdNd(decl: ForDecl, repetitions: ExpNd, cl: List[ClaimNd], body: CommandNd) => {
          //TODO
        }

        case CoCmdNd(cl: List[ClaimNd], fstCmd: CommandNd, sndCmd: CommandNd) => {
          //TODO
        }

        case AcceptCmdNd(methodImplementationList: List[MethodImplementationDeclNd]) => {
          //TODO
        }

        case WithCmdNd(lock: ExpNd, tpl: List[TakesPerNd], guard: ExpNd, command: CommandNd, gpl: List[GivesPerNd]) => {
          //TODO
        }

        case _ => None

      }

    }

    def getBaseOfPrimitiveType(type$: Type): String = {
      type$ match {
        case PrimitiveType(base) => {
          //          println("The base Type for Expression Defindness :" + base.toString())
          base.toString()
        }
        case LocationType(primitiveType) => getBaseOfPrimitiveType(primitiveType)
        case _ => contracts.Contracts.toDo("Only Primtive and Location Types are allowed yet")
      }

    }

  }

  //Class Claim Code Generation

  private def classClaimCodeGen() {
    heapTransContext.set("Permission", heapTransContext.objRef)
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ClaimNd(pmn) =>
          classClaimList :+ mem
          val perm_pairs = pmn.pm;
          for ((loc, amnt) <- perm_pairs) {
            loc match {
              case ObjectIdLSN(nameExp) => {
                val fqn = NameManager.getFQN(loc)
                val amount: String = ExpCodeGen.simpleExpCodeGen(amnt);
                outputBuilder.putln("//Claim")
                outputBuilder.setError("Permission Ammount is not valid: it must be 0.0 _< PermValue _< 1.0", amnt.coord)
                outputBuilder.putln("assert IsValidPermission(" + amount + ");")
                outputBuilder.clearError
                outputBuilder.putln(heapTransContext.getHeap() + "[" + heapTransContext.getObjRef() + "," + fqn + "] := " + heapTransContext.getHeap() + "[" + heapTransContext.getObjRef() + "," + fqn + "] + " + amount + ";")
                outputBuilder.setError("Claimed permission amount makes total permission invalid ", mem.coord)
                outputBuilder.putln("assert " + "0.0 <= " + heapTransContext.getHeap() + "[" + heapTransContext.getObjRef() + "," + fqn + "]" + " && " + heapTransContext.getHeap() + "[" + heapTransContext.getObjRef() + "," + fqn + "]" + "<=" + "1.0;")
                outputBuilder.clearError
              }
              case _ => contracts.Contracts.toDo("Location Set Node with Array")
            }
          }
        case _ =>
      }
    }
    outputBuilder.newLine
    outputBuilder.setError("Maximum Permission Amount Must Not Exceed \'1.0\'", dlNd.coord)
    outputBuilder.put("assert (forall<x> r: Ref, f: Field x :: 0.0 <= Permission[r,f] && Permission[r,f] <= 1.0);")
    outputBuilder.newLine
    outputBuilder.clearError
    heapTransContext.reset()
  }

  //Object Initialization Code Generation

  private def objectsInitCodeGen(classDeclNd: ClassDeclNd) {
    var objDecl, objInits = ""
    for (mem <- classDeclNd.directMembers) {
      mem match {
        case ObjDeclNd(isGhost: Boolean, isConst: Boolean, acc: Access, ty: TypeNd, init: InitExpNd) => {
          objDecl = objDeclCodeGen(isConst, acc, ty, mem.fqn.toString)
          val objType: String = TypeCodeGen(ty)
          val initExp: String = ExpCodeGen.initExpCodeGen(init, heapTransContext)
          outputBuilder.putln("//Initialize Heap")
          outputBuilder.putln(heapTransContext.heap + "[" + heapTransContext.objRef + "," + mem.fqn.toString + "] := " + initExp + " ;")
          // building the expression not required for this subroutine, This subroutine contains the initialization of objects
        }
        case _ => ""
      }
    }
    if (!classDeclNd.constructorParams.isEmpty) {
      //TODO need value received while creation
      heapTransContext.setHeap("Permission")

      for (param <- classDeclNd.constructorParams.toList) {
        val paramType: String = TypeCodeGen(param.ty)
        outputBuilder.newLine
        outputBuilder.put(heapTransContext.heap + "[" + heapTransContext.objRef + "," + param.fqn.toString() + "] := TODO value recived while creation;")
        // building the expression not required for this subroutine, This subroutine contains the initialization of objects
        outputBuilder.newLine

      }
    }

  }

  //Method's Parameters Code Generation

  def paramDeclCodeGen(paramList: List[ParamDeclNd]) {
    //TODO Object in|out Category
    //TODO Ghost Objects
    for (obj <- paramList) {
      val objType: String = TypeCodeGen(obj.ty)
      obj.ty.tipe.get match {
        case ArrayLocationType(baseType) => {
          outputBuilder.putln("const unique " + obj.fqn + ":" + "Field(ArrayRef " + objType + ");")
        }
        case _ => {
          outputBuilder.putln("const unique " + obj.fqn + ":" + "Field " + objType + ";")
        }
      }
    }
  }

  //Method's 'pre' Condition Code Generation
  def methPreCondCodeGen(acc: Access, name: String, params: List[ParamDeclNd], preCnds: List[PreCndNd]) {
    heapTransContext.reset()
    outputBuilder.newLine
    outputBuilder.put("//Pre Condition(s)")
    outputBuilder.newLine
    outputBuilder.put("oldHeap := Heap;")
    outputBuilder.newLine
    outputBuilder.put("havoc Heap;")
    for (prc <- preCnds) {
      heapTransContext.setHeap("Heap")
      outputBuilder.newLine
      outputBuilder.put("assume " + ExpCodeGen.buildBoogieExp(prc.condition, heapTransContext) + ";")
      outputBuilder.newLine
    }
  }

  //Method's 'takes' Specification Code Generation
  def methTakesPerCodeGen(acc: Access, name: String, params: List[ParamDeclNd], takesPers: List[TakesPerNd]) {
    heapTransContext.reset()
    outputBuilder.newLine
    outputBuilder.put("//Taking Permission(s)")
    for (tp <- takesPers)
      for ((lsn, exp) <- tp.pmn.pm)
        lsn match {
          case ObjectIdLSN(nameExp) => {
            val amount: String = ExpCodeGen.simpleExpCodeGen(exp)
            outputBuilder.newLine
            outputBuilder.put("oldPermission := Permission;")
            outputBuilder.newLine
            outputBuilder.put("if(Permission[" + heapTransContext.objRef + "," + NameManager.getFQN(lsn) + "] == 0.0)")
            outputBuilder.newLine
            outputBuilder.put("{")
            outputBuilder.newLine
            outputBuilder.put("havoc tempHeap;")
            outputBuilder.newLine
            outputBuilder.put("Heap[" + heapTransContext.objRef + "," + NameManager.getFQN(lsn) + "] := tempHeap[" + heapTransContext.objRef + "," + NameManager.getFQN(lsn) + "];")
            outputBuilder.newLine
            outputBuilder.put("Permission[" + heapTransContext.objRef + "," + NameManager.getFQN(lsn) + "] := " + "Permission[" + heapTransContext.objRef + "," + NameManager.getFQN(lsn) + "]+" + amount + ";")
            outputBuilder.newLine
            outputBuilder.put("}")
          }
          case _ => contracts.Contracts.toDo("Array Location Set Node")
        }
  }

  //Method's 'post' Condition Code Generation
  def methPostCondCodeGen(acc: Access, name: String, params: List[ParamDeclNd], postCnds: List[PostCndNd]) {
    heapTransContext.reset()
    outputBuilder.newLine
    outputBuilder.put("//Post Condition(s)")
    for (poc <- postCnds) {
      val exp = poc.condition;
      outputBuilder.newLine
      nameExp = new ArrayBuffer[String]()
      val nameExps = ExpCodeGen.nameExpCodeGen(exp, nameExp)
      for (name <- nameExps) {
        outputBuilder.newLine
        outputBuilder.setError("Permission amount should be greater than 0.0", poc.condition.coord)
        outputBuilder.put("assert Permission[" + heapTransContext.objRef + "," + name + "] > 0.0;")
        outputBuilder.newLine
        outputBuilder.clearError
      }
      outputBuilder.newLine
      outputBuilder.setError("Post Condition does not satisfy", poc.condition.coord)
      outputBuilder.put("assert " + ExpCodeGen.buildBoogieExp(poc.condition, heapTransContext) + ";") // assume and assert
      outputBuilder.newLine
      outputBuilder.clearError

    }
  }
  //Method's 'gives' Specification Code Generation

  def methGivesPerCodeGen(acc: Access, name: String, params: List[ParamDeclNd], givesPers: List[GivesPerNd]) {
    outputBuilder.newLine
    outputBuilder.put("//Giving Permissions(s)")
    heapTransContext.reset()
    heapTransContext.setHeap("Permission")
    for (tp <- givesPers)
      for ((lsn, amnt) <- tp.pmn.pm)
        lsn match {
          case ObjectIdLSN(nameExp) => {
            val amount = ExpCodeGen.simpleExpCodeGen(amnt)
            // Assert at least permission, and then the subtract the permission
            outputBuilder.newLine
            //assert the amount of permission at least the amount going to subtract
            outputBuilder.setError("Can not give permission(s)", lsn.getCoord())
            outputBuilder.put("assert " + ExpCodeGen.buildBoogieExp(nameExp, heapTransContext) + " >= " + amount + ";")
            // Subtract the Permission
            outputBuilder.newLine
            outputBuilder.put(ExpCodeGen.buildBoogieExp(nameExp, heapTransContext) + " := " + ExpCodeGen.buildBoogieExp(nameExp, heapTransContext) + " - " + amount + ";");
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
          val invString = ExpCodeGen.InvExpCodeGen(exp, heapTransContext)
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

  //Thread Claim Code Generation

  def claimCodeGen(claimNd: ClaimNd) {
    val perm_pairs = claimNd.pmn.pm;
    var result = "";
    for ((loc, amnt) <- perm_pairs) {
      var name = "";
      loc match {
        case ObjectIdLSN(exp) => {
          name = NameManager.getFQN(exp)
        }
        case ArrayLSN(forDecl, offSet, bound, boundInclusive, locSet) => {
          //TODO
        }
        case _ => contracts.Contracts.toDo("Location Set Node with Array")
      }
      val amount: String = ExpCodeGen.simpleExpCodeGen(amnt)
      outputBuilder.newLine
      outputBuilder.put("//Claim")
      outputBuilder.newLine
      outputBuilder.put("Permission[" + heapTransContext.objRef + "," + name + "] := Permission[" + heapTransContext.objRef + "," + name + "] + " + amount + ";")
    }
  }

  private def classInvCodeGen() {
    outputBuilder.putln("//Class Invariant")
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
      mem match {
        case ClassInvNd(exp) => {
          classInvList :+ mem;
          val invString = ExpCodeGen.InvExpCodeGen(exp, heapTransContext)
          outputBuilder.setError("Invariant does not hold", exp.coord)
          outputBuilder.putln("assert " + invString + ";")
          outputBuilder.clearError
        }
        case _ => {}
      }
  }

  private def lockForceClassInvariant(stmt: String, context: TransContext) {
    outputBuilder.newLine
    outputBuilder.put("//Class Invariant")
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
      mem match {
        case ClassInvNd(exp) => {
          val invString = ExpCodeGen.InvExpCodeGen(exp, context)
          outputBuilder.newLine
          outputBuilder.setError("Invariant does not hold", exp.coord)
          outputBuilder.put(stmt + invString + ";")
          outputBuilder.newLine
          outputBuilder.clearError
        }
        case _ => {}
      }
  }

  private def loopInvCodeGen(loopInv: LoopInvNd) {
    //Check Loop Invariant is Defined
    heapTransContext.reset()
    outputBuilder.newLine
    outputBuilder.put("//loop Invariant")
    val invString = ExpCodeGen.InvExpCodeGen(loopInv.exp, heapTransContext)
    outputBuilder.newLine
    outputBuilder.setError("Invariant does not hold", loopInv.exp.coord)
    outputBuilder.put("assert " + invString)
    outputBuilder.newLine
    outputBuilder.clearError
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
  def lockedClassInv(lock: ExpNd, stmt: String) { // For locked objects
    val lockContext = new TransContext("LockPermission", ExpCodeGen.getNamefromLockExp(lock));
    heapTransContext.reset()
    val baseContext = new TransContext(heapTransContext.getHeap(), heapTransContext.getObjRef())

    lock match {
      case ThisExpNd(str) =>
        for (classInv <- dlNd.asInstanceOf[ClassLike].directMembers) {
          classInv match {
            case ClassInvNd(exp) => {
              // assert defindness
              outputBuilder.newLine
              outputBuilder.put("//Assert defindness of object invariant and assume object invariance")
              outputBuilder.newLine
              nameExp = new ArrayBuffer[String]()
              val nameExps = ExpCodeGen.nameExpCodeGen(exp, nameExp)
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
              heapTransContext.reset()
              outputBuilder.newLine
              outputBuilder.put(stmt + " " + ExpCodeGen.InvExpCodeGen(exp, lockContext, heapTransContext) + ";") // due to lock we need different invariant generation
              outputBuilder.newLine

            }
            case _ =>
          }
        }
      case NameExpNd(nameNd) => //TODO
    }
  }

  //  def assertClassInv(lock: ExpNd) { //For Locked objects
  //    val lockContext = new TransContext("LockPermission", ECG.getNamefromLockExp(lock));
  //    transContext.reset()
  //    val baseContext = new TransContext(transContext.getHeap(), transContext.getObjRef())
  //    for (classInv <- dlNd.asInstanceOf[ClassLike].directMembers) {
  //      classInv match {
  //        case ClassInvNd(exp) => {
  //          // assert defindness
  //          outputBuilder.newLine
  //          outputBuilder.put("//Assert defindness of object invariant and assert object invariance")
  //          outputBuilder.newLine
  //          nameExp = new ArrayBuffer[String]()
  //          val nameExps = ECG.nameExpCodeGen(exp, nameExp)
  //          if (!(nameExps.isEmpty)) {
  //            outputBuilder.setError("Does not have enough permission(0)", exp.coord)
  //            outputBuilder.newLine
  //            outputBuilder.put("assert ");
  //            for (name <- nameExps)
  //              outputBuilder.put(lockContext.heap + "[" + lockContext.getObjRef() + "," + name + "] > 0.0;") // generating && true extra , need to fix this glitch
  //            outputBuilder.newLine
  //            outputBuilder.clearError
  //          }
  //          //assert invariant
  //          transContext.reset()
  //          outputBuilder.newLine
  //          outputBuilder.put("assert " + ECG.InvExpCodeGen(exp, lockContext, transContext) + ";") // due to lock we need different invariant generation
  //          outputBuilder.newLine
  //        }
  //        case _ =>
  //      }
  //    }
  //  }

  def objDeclCodeGen(isConst: Boolean, acc: Access, ty: TypeNd, fqn: String): String = {
    val objType: String = TypeCodeGen(ty)
    val objCode = "\nconst unique " + fqn + ":" + "Field " + objType + ";"
    return objCode
  }

}
