package boogieBackEnd;
import frontEnd.AST;
import frontEnd.AST._;
import util.Format;
import util.OutputBuilder;
import contracts.Contracts;
import checker.CheckerTypes._
import scala.collection.mutable.StringBuilder;
import scala.collection.mutable;
import frontEnd.AST.ClassInvNd;
import frontEnd.AST.LoopInvNd;
import scala.collection.mutable.ArrayBuffer;

private class ClassCodeGen(val dlNd: DeclNd, outputBuilder: OutputBuilder) {

  //global translation context has generic form of heap and object reference
  //local translation contexts are set when changing to particular Heap and object reference i.e preHeap, tempHeap, oldHeap, and *_this, *_that

  private var heapTransContext = new TransContext("Heap", "This." + dlNd.fqn.toString)

  private var arrayHeapTransContext = new TransContext("ArrayHeap", "This." + dlNd.fqn.toString)

  private var nameExp = ArrayBuffer[String](); // keep collected Names used in Expression

  /* Collect the invariant,local Declarations, class claim and global objects from List, to avoid traversing the list and finding node at various subroutines of code generation */

  private var localDeclNdList = List[LocalDeclNd]();

  private var classClaimList = new ArrayBuffer[ClaimNd] // Need to check all the claims must not be greater than '1.0' on one location

  private var threadClassClaimList = new ArrayBuffer[ClaimNd];

  private var methodDeclList = new ArrayBuffer[MethodDeclNd];

  private var newObject = 0;

  private var newParam = 0;

  private var newPerMap = 0;

  private var lockExpList = List[String]()

  private var lockPerMaps = List[String]()

  private var lockPermissionMapsList = List[String]()

  def getOutputBuilder(): OutputBuilder = outputBuilder;

  private var savedLine = outputBuilder.saveLine();

  def classCodeGen(): Unit = {

    /*******************************************************/
    /***** Class/Interface Identifier Code Generation *****/
    /*****************************************************/

    val interfaceName = dlNd.asInstanceOf[ClassLike].superTypes;
    if (!(interfaceName.isEmpty)) {
      outputBuilder.putln("const unique " + dlNd.fqn.toString + ":ClassName <: " + interfaceName(0) + ";")
    } else {
      outputBuilder.putln("const unique " + dlNd.fqn.toString + ":ClassName;")
    }

    /****************************************************/
    /***** Object Identifier Code Generation ***********/
    /**************************************************/

    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
      mem match {
        case ObjDeclNd(isGhost: Boolean, isConst: Boolean, acc: Access, ty: TypeNd, init: InitExpNd) => {
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
            //TODO evaluate the expression and get a final value if required at translation
            outputBuilder.putln("axiom " + mem.fqn + " == " + ExpCodeGen.initExpCodeGen(init, heapTransContext) + ";")
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

    /***************************************************/
    /***** Collect Local Variables Declaration ********/
    /*************************************************/

    def collectLocalVarDecls(block: CommandNd) {
      block match {

        case SeqCommandNd(fstCmd: CommandNd, sndCmd: CommandNd) => {
          collectLocalVarDecls(fstCmd)
          collectLocalVarDecls(sndCmd)
        }

        case LocalDeclCmdNd(decl: LocalDeclNd) => localDeclNdList :+ decl

        case AssignmentCmdNd(lhs, rhs) => None

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

    /************************************************************************/
    /***** Promote All Local Variable Declarations into Boogie Fields ******/
    /**********************************************************************/

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
        outputBuilder.putln("axiom " + decl.fqn.toString() + " == " + ExpCodeGen.simpleExpCodeGen(decl.init) + ";") //TODO evaluate the expression and get a final value if required
      }
    }

    /****************************************************************/
    /***** collection Method Declarations into MethDeclList ********/
    /**************************************************************/

    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case decl @ MethodDeclNd(acc, params, preCnds, postCnds, givesPers, takesPers, borrowsPers) => {
          methodDeclList.+=(decl)
        }
        case _ => {}
      }
    }

    /************************************************************************/
    /***************** Constructor Procedure Code Generation ***************/
    /**********************************************************************/

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
    outputBuilder.putln("{")
    outputBuilder.indent
    outputBuilder.putln("var oldHeap, preHeap, tempHeap:HeapType;")
    outputBuilder.putln("var Permission, oldPermission : PermissionType;")
    outputBuilder.putln("var ArrayPermission: ArrayPermissionType;")
    outputBuilder.putln("havoc " + heapTransContext.getHeap() + ";")

    //Add Permissions to Input Parameters of Constructor Procedure
    
    addPermissionToInputParameters(classDeclNd)
    

    //Class Claim Code Generation
    classClaimCodeGen(classDeclNd)

    //Objects initialization Code Generation, It Initializes the Heap
    objectsInitCodeGen(classDeclNd)

    //Class Invariant Code
    classInvCodeGen(classDeclNd)
    

    //Closing of Constructor Procedure
    outputBuilder.dedent
    outputBuilder.putln("}")

    threadProceduresCodeGen(classDeclNd)
    
  }

  
/*********************************************/
/***** Thread Procedure Code Generation *****/
/*******************************************/
  def threadProceduresCodeGen(classDeclNd: ClassDeclNd) {
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ThreadDeclNd(claimList: List[ClaimNd], block: CommandNd) => {
          outputBuilder.putln("procedure " + dlNd.name + "." + mem.name + " (" + heapTransContext.objRef + " : Ref)")
          outputBuilder.putln("requires dtype(" + heapTransContext.objRef + ") <: " + dlNd.fqn.toString + ";")
          outputBuilder.putln("modifies " + heapTransContext.getHeap() + ";")
          outputBuilder.putln("modifies LockPermission;")
          outputBuilder.putln("{")
          outputBuilder.indent
          outputBuilder.putln("var Permission : PermissionType where (forall <x> r: Ref, f: Field x :: Permission[r,f] == 0.0);")
          outputBuilder.putln("var ArrayPermission : ArrayPermissionType where (forall <x> r: ArrayRef x, f: int :: ArrayPermission[r,f] == 0.0);")
          outputBuilder.putln("var oldPermission: PermissionType;")
          outputBuilder.putln("var oldArrayPermission : ArrayPermissionType;")
          outputBuilder.putln("var oldHeap, preHeap, tempHeap: HeapType;")
          outputBuilder.putln("var oldArrayHeap, preArrayHeap, tempArrayHeap: ArrayHeapType;")
          savedLine = outputBuilder.saveLine() // save current line of outputBuilder and get all the lock variables  declared here
          
          //Thread Claim Code Generation
          
          /*********************************************/
          /******* Thread Claim Code Generation *******/
          /*******************************************/

          for (claim <- claimList)
            claimCodeGen(claim, classDeclNd)

          //Thread Body Code Generation, Commands/Statements inside thread
          
          commandCodeGen(block)
          
          outputBuilder.dedent
          outputBuilder.newLine
          outputBuilder.putln("}//end of Thread Procedure")
        }
        case _ => {}
      }
    }

    def commandCodeGen(cmd: CommandNd) {

      cmd match {

        case SkipCmdNd() => //Skip

/****************************************************/
/******* Sequence of Commands Code Generation *******/
/****************************************************/
        case SeqCommandNd(fstCmd, sndCmd) => {
          outputBuilder.indent
          commandCodeGen(fstCmd)
          commandCodeGen(sndCmd)
          outputBuilder.dedent
        }

/****************************************************/
/********** Local Declaration Code eneration ********/
/****************************************************/

        case LocalDeclCmdNd(decl) => {

          //add full Permission to local declaration
          outputBuilder.newLine
          outputBuilder.putln("Permission[" + heapTransContext.objRef + "," + decl.fqn.toString() + "] := 1.0;")
          val objType: String = TypeCodeGen(decl.ty)
          //Check definedness of initialization expression
          isExpressionDefined(decl.init, heapTransContext)
          outputBuilder.newLine
          outputBuilder.putln("Heap[" + heapTransContext.objRef + "," + decl.fqn.toString() + "] := " + ExpCodeGen.buildBoogieExp(decl.init, heapTransContext) + ";")
          //command code generation - in scope of this local declaration
          commandCodeGen(decl.cmd)
        }
/****************************************************/
/********** Assignment Command Code eneration ********/
/****************************************************/

        case AssignmentCmdNd(lhs, rhs) => {

          // Sum up all locked Permissions first, later put it in assignment defindness
          // Assert Write Permission on LHS and ReadPermission on LHS
          //isAssignmentDefined(lhs.toList, rhs.toList)

          outputBuilder.newLine
          outputBuilder.putln("//Check Assignment Defindness")
          val expPairs = lhs zip rhs
          for ((lhs, rhs) <- expPairs) {
            outputBuilder.newLine
            outputBuilder.putln("//Expression Definedness Start")
            isExpressionDefined(lhs, heapTransContext)
            isExpressionDefined(rhs, heapTransContext)
            outputBuilder.putln("//Expression Definedness Ends")
          }

          outputBuilder.newLine
          heapTransContext.reset()

          outputBuilder.putln("//Check Permissions For Assignment-Begin")

          val localTransContext = heapTransContext;
          localTransContext.setHeap("Permission")

          for ((lhs, rhs) <- expPairs) {

            // Write Permission Assertion - For LHS
            outputBuilder.putln("//Check Writing Permissions For Assignment - Begin")
            val lhs_result = ExpCodeGen.buildWritingPerExp(lhs, localTransContext)
            outputBuilder.setError("Do not have enough permission(s) on LHS of assignment", lhs.coord)
            if (!(lockPerMaps.isEmpty)) {
              var temp = "";
              for (lockPerMap <- lockPerMaps) {
                localTransContext.setHeap(lockPerMap)
                temp += "&&" + ExpCodeGen.buildWritingPerExp(lhs, localTransContext)
              }
              outputBuilder.put(s"assert ${lhs_result} ${temp};")
              localTransContext.reset()
            } else {
              outputBuilder.putln("assert " + lhs_result + ";")
              outputBuilder.newLine
            }
            outputBuilder.clearError
            outputBuilder.putln("//Check Writing Permissions For Assignment - End")

            // Read Permission Assertion - For RHS
            outputBuilder.putln("//Check Reading Permissions For Assignment - Begin")

            val rhs_result = ExpCodeGen.buildReadingPerExp(rhs, localTransContext)
            outputBuilder.setError("Do not have enough permission(s) on RHS of assignment", rhs.coord)

            if (!(lockPerMaps.isEmpty)) {
              var temp = "";
              for (lockPerMap <- lockPerMaps) {
                localTransContext.setHeap(lockPerMap)
                temp += "&&" + ExpCodeGen.buildReadingPerExp(rhs, localTransContext)
              }
              outputBuilder.putln(s"assert ${rhs_result} ${temp};")
            } else {
              outputBuilder.putln(s"assert ${rhs_result};")
              outputBuilder.newLine
            }
            outputBuilder.newLine
            outputBuilder.clearError
            outputBuilder.newLine
          }
          outputBuilder.putln("//Check Reading Permissions For Assignment - End")
          outputBuilder.putln("//Check Permissions For Assignment-Ends")

          // assignment translation context
          heapTransContext.reset()
          outputBuilder.newLine
          outputBuilder.put("//Assignment Command Begin")
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

/*******************************************************/
/********** Method Call Command Code Generation ********/
/*******************************************************/

        case CallCmdNd(method, argList) => {

          val argToParamMap = new mutable.HashMap[ExpNd, ParamDeclNd]
          val methDeclList = methodDeclList.toList
          outputBuilder.putln("preHeap := Heap;")
          outputBuilder.putln("preArrayHeap := ArrayHeap;")
         
          //Find declaration and map arguments to parameters
          method match {
            case NameExpNd(nameNd) => {
              for (methDecl <- methDeclList) {
                if (nameNd.toString() == methDecl.name && methDecl.params.size == argList.size) {
                  
                  //TODO Identify Method Overloading
                  // println("Seeeee\n nameNd.toString() : " + nameNd.toString() + " methDecl.name " + methDecl.name)
                  for ((arg, param) <- argList zip methDecl.params)
                    argToParamMap += ((arg, param))
                }
              }
            }
            case _ => {}
          }

          //Arguments Definedness
          if (argList.nonEmpty) {
            for (arg <- argList) {
              checkArgDefindnessAndCopy(arg)
            }
            def checkArgDefindnessAndCopy(arg: ExpNd) {
              arg match {
                case NameExpNd(nameNd) => {
                  val argName = "arg_" + newParam
                  outputBuilder.goToBefore(savedLine)
                  outputBuilder.putln(s"var ${argName}:= Ref;")
                  outputBuilder.goToEnd()
                  newParam = newParam + 1
                  argToParamMap.get(arg) match {
                    case param @ Some(ParamDeclNd(isGhost, ty, paramCategory)) => {
                      outputBuilder.putln(s"${param.get.name} := Heap[${heapTransContext.objRef},${argName}];")
                    }
                    case _ => { contracts.Contracts.unreachable("Parameter not defined") }
                  }
                }
                case IntLiteralExpNd(i) => {
                  isExpressionDefined(arg, heapTransContext)
                }
                case FloatLiteralExpNd(d) => {
                  isExpressionDefined(arg, heapTransContext)
                }
                case FetchExpNd(x) => {
                  isExpressionDefined(x, heapTransContext)
                  
                }
                case _ => contracts.Contracts.toDo("Add Members arguments and Expressions")
              }
            }
          }

          //Replacement Step
          outputBuilder.goToBefore(savedLine)
          outputBuilder.putln("var that : Ref;")
          outputBuilder.goToEnd()
          outputBuilder.putln(s"that := ${heapTransContext.initObjRef};")
          val replacedTransContext = new TransContext("Heap", "that") // Replacement only occur for pre condition and post condition.
          val PerTransContext = new TransContext("Permission", "that") // Does not need to be replaced for method calls

          method match {
            case NameExpNd(nameNd) => {
              for (methDecl <- methDeclList) {
                if (nameNd.toString() == methDecl.name && methDecl.params.size == argList.size) {
                  // Check Pre Condition
                  for (pre <- methDecl.preCnds) {
                    outputBuilder.setError("Pre Condition not defined, or does not hold", pre.condition.coord)
                    isExpressionDefined(pre.condition, replacedTransContext)
                    outputBuilder.putln("assert " + ExpCodeGen.buildBoogieExp(pre.condition, replacedTransContext))
                    outputBuilder.clearError
                  }

                  // Takes Specification - Remove Permission
                  for (tp <- methDecl.takesPers) {
                    for ((lsn, exp) <- tp.pmn.pm)
                      lsn match {
                        case ObjectIdLSN(nameExp) => {
                          outputBuilder.setError("Does not have enough permission to transfer to callee", lsn.getCoord())
                          outputBuilder.putln(s"assert Permission[${replacedTransContext.initObjRef},${NameManager.getFQN(lsn)}] == ${ExpCodeGen.simpleExpCodeGen(exp)}")
                          outputBuilder.putln(s"Permission[${replacedTransContext.initObjRef},${NameManager.getFQN(lsn)}] == Permission[${replacedTransContext.initObjRef},${NameManager.getFQN(lsn)}] - ${ExpCodeGen.simpleExpCodeGen(exp)};")
                          outputBuilder.clearError
                        }
                        case ArrayLSN(forDecl, offSet, bound, boundInclusive, locSet) => {
                          contracts.Contracts.toDo("Array Location Set Node")
                        }
                      }
                  }

                  // Takes Specification - Add Permission
                  for (gp <- methDecl.givesPers) {
                    for ((lsn, exp) <- gp.pmn.pm)
                      lsn match {
                        case ObjectIdLSN(nameExp) => {
                          outputBuilder.putln(s"if(Permission[${heapTransContext.initObjRef},${NameManager.getFQN(lsn)}] == 0.0)")
                          outputBuilder.putln("{")
                          outputBuilder.putln("havoc Heap_tmp;")
                          outputBuilder.putln(s"Heap[${replacedTransContext.initObjRef},${NameManager.getFQN(lsn)}] := Heap_tmp[${replacedTransContext.initObjRef},${NameManager.getFQN(lsn)}];")
                          outputBuilder.putln("}")
                          outputBuilder.putln(s"Permission[${replacedTransContext.initObjRef},${NameManager.getFQN(lsn)}] := Permission[${replacedTransContext.initObjRef},${NameManager.getFQN(lsn)}] + ${ExpCodeGen.simpleExpCodeGen(exp)};")
                        }
                        case ArrayLSN(forDecl, offSet, bound, boundInclusive, locSet) => {
                          contracts.Contracts.toDo("Array Location Set Node")
                        }
                      }
                  }

                  // Check Post Condition

                  for (post <- methDecl.postCnds) {
                    val postCndTransContext = new TransContext("oldHeap", "that")
                    outputBuilder.putln("oldHeap := Heap;")
                    outputBuilder.putln("havoc Heap;")
                    outputBuilder.putln(s"assume ${ExpCodeGen.buildPostCondExp(post.condition, postCndTransContext)};")
                    outputBuilder.putln("assume (forall<x> t:Ref, f : Field x :: (!(t==that && f==Counter.count) ==> Heap[t,f] == oldHeap[t,f]));")
                    var nameExpList = new ArrayBuffer[String]()
                    val nameExps = ExpCodeGen.nameExpCodeGen(post.condition, nameExpList)
                    if (!nameExps.isEmpty) {
                      for (fqn <- nameExps)
                        outputBuilder.putln(s"assume (forall<x> r:Ref, f : Field x :: !(r == ${postCndTransContext.getObjRef()} && f == " + fqn + ") ==>Heap[r, f] == oldHeap[r, f]);")
                    }
                  }
                }
              }
            }
            case _ => {}
          }
        }
/*******************************************************/
/************* If Command Code Generation **************/
/*******************************************************/

        case IfCmdNd(guard, thenCmd, elseCmd) => { //set translation context for guard
          // Translate Body of else, assert guard is defined
          outputBuilder.putln("//assert guard is defined")
          isExpressionDefined(guard, heapTransContext)
          //Check the thread has enough permission on guard expression
          outputBuilder.setError("Guard is not defined, do not have anough permission(s)", guard.coord)
          outputBuilder.put(ExpCodeGen.assertReadingPermission(guard, heapTransContext))
          outputBuilder.newLine
          outputBuilder.clearError
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

/*******************************************************/
/************** While Command Code Generation **********/
/*******************************************************/

        case WhileCmdNd(guard, lil, body) => {

          //Store Heap and ArrayHeap into oldHeap, and oldArrayHeap

          outputBuilder.putln("oldHeap := Heap;")
          outputBuilder.putln("oldArrayHeap := ArrayHeap;")
          
          guard match {
            case BooleanLiteralExpNd(b) => {
              outputBuilder.newLine
              outputBuilder.put(s"while(${ExpCodeGen.simpleExpCodeGen(guard)})")
            }
            case _ => {
              // build guard expression, definedness of expression
              outputBuilder.newLine
              outputBuilder.put("while(" + ExpCodeGen.buildBoogieExp(guard, heapTransContext) + ")")
              outputBuilder.setError("Loop invariant does not hold. Guard expression is not defined", guard.coord)
              outputBuilder.putln("invariant " + isExpressionDefined(guard, heapTransContext))
              outputBuilder.clearError
            }
          }

          // add invariant asserting guard is true



          if (lil.isEmpty) {
            heapTransContext.reset()
            heapTransContext.setHeap("Permission")
            outputBuilder.newLine
            outputBuilder.put("invariant (forall<x> r: Ref, f: Field x :: 0.0 <= " + heapTransContext.getHeap() + "[r,f]" + " && " + heapTransContext.getHeap() + "[r,f] <= 1.0);")
            heapTransContext.reset()
          } else {
            for (loopInv <- lil)
              loopInvCodeGen(loopInv)
          }

          outputBuilder.newLine
          outputBuilder.put("{")
          outputBuilder.indent
          commandCodeGen(body)
          outputBuilder.newLine
          outputBuilder.dedent
          outputBuilder.put("}")
        }

/*******************************************************/
/*************** For Command Code Generation ***********/
/*******************************************************/

        case ForCmdNd(decl, bounds, lil, body) => {

          val counter = heapTransContext.getHeap() + "[" + heapTransContext.getObjRef() + "," + decl.fqn.toString() + "]";
          val boundExp = ExpCodeGen.buildBoogieExp(bounds, heapTransContext)
          //Store Heap and ArrayHeap into oldHeap, and oldArrayHeap

          outputBuilder.putln("oldHeap := Heap;")
          outputBuilder.putln("oldArrayHeap := oldHeap;")

          outputBuilder.newLine
          outputBuilder.putln(s"while(${counter} < ${boundExp})")

          // add invariant asserting guard is true

          outputBuilder.setError("Loop invariant does not hold. Bound expression is not defined", bounds.coord)
          outputBuilder.putln("invariant assert" + s"${counter} < ${boundExp} )")
          outputBuilder.clearError

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
/**********************************************************/
/************** 'co' with 'for' Command Code Generation **********/
/**********************************************************/

        case CoForCmdNd(decl, repetitions, claimList, body) => {
          outputBuilder.indent
          //Translate body of Co For Command
          outputBuilder.dedent
        }
/**********************************************************/
/************** 'co' Command Code Generation **********/
/**********************************************************/

        case CoCmdNd(claimList, fstCmd, sndCmd) => {
          outputBuilder.indent
          //Translate fstCmd
          //Translate sndCmd
          outputBuilder.dedent
        }

/**********************************************************/
/*************** 'accept' Command Code Generation ***********/
/**********************************************************/
        case AcceptCmdNd(methodImplementationList) => {

          //List all the method implementations in one goto statement.
          outputBuilder.newLine
          outputBuilder.put("goto ")
          for (mi <- methodImplementationList) {
            if (!mi.equals(methodImplementationList.last))
              outputBuilder.putln(mi.nameNd.toString() + ",")
            else
              outputBuilder.putln(mi.nameNd.toString() + ";")
          }

          //Start of method translation
          for (mi <- methodImplementationList) {
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

                // Adding Permission to parameters
                for (param <- paramList) {
                  param.paramCategory match {
                    case InParamCategory => {
                      //Add Reading Permission
                      outputBuilder.putln("//Add Reading Permission to input parameters")
                      outputBuilder.putln(s"if(Permission[This.${dlNd.fqn.toString()},${param.fqn.toString()}] == 0.0)")
                      outputBuilder.putln("{")
                      outputBuilder.indent
                      outputBuilder.putln("havoc Heap_tmp;")
                      outputBuilder.putln(s"Heap[This.${dlNd.fqn.toString()},${param.fqn.toString()}] := Heap_tmp[This.${dlNd.fqn.toString()},${param.fqn.toString()}];")
                      outputBuilder.dedent
                      outputBuilder.putln("}");
                      outputBuilder.putln(s"Permission[This.${dlNd.fqn.toString()},${param.fqn.toString()}] := Permission[This.${dlNd.fqn.toString()},${param.fqn.toString()}] + 0.5;")
                    }
                    case OutParamCategory => {
                      //Add Writing Permission
                      outputBuilder.putln("//Add Writing Permission to input parameters")
                      outputBuilder.putln(s"if(Permission[This.${dlNd.fqn.toString()},${param.fqn.toString()}] == 0.0)")
                      outputBuilder.putln("{")
                      outputBuilder.indent
                      outputBuilder.putln("havoc Heap_tmp;")
                      outputBuilder.putln(s"Heap[This.${dlNd.fqn.toString()},${param.fqn.toString()}] := Heap_tmp[This.${dlNd.fqn.toString()},${param.fqn.toString()}];")
                      outputBuilder.dedent
                      outputBuilder.putln("}");
                      outputBuilder.putln(s"Permission[This.${dlNd.fqn.toString()},${param.fqn.toString()}] := Permission[This.${dlNd.fqn.toString()},${param.fqn.toString()}] + 1.0;")
                    }
                  }
                }

                //Takes Permission - transferring permission from caller to method
                outputBuilder.indent
                methTakesPerCodeGen(acc, methDecl.fqn.toString(), paramList, takesPerList)
                outputBuilder.dedent

                //Pre Conditions - Check the Pre condition
                outputBuilder.indent
                methPreCondCodeGen(acc, methDecl.fqn.toString(), paramList, preCndList)
                outputBuilder.dedent
                outputBuilder.newLine

                //Method Implementation Code Generation
                outputBuilder.putln("//Method Implementation")
                outputBuilder.indent
                outputBuilder.putln("preHeap := Heap;\npreArrayHeap := ArrayHeap;")
                commandCodeGen(mi.fstCmd)

                outputBuilder.newLine
                outputBuilder.dedent

                //Method Post Condition Code Generation
                outputBuilder.indent
                methPostCondCodeGen(acc, methDecl.fqn.toString(), paramList, postCndList)
                outputBuilder.newLine
                outputBuilder.dedent

                //Method Gives Permission Code Generation
                outputBuilder.indent
                methGivesPerCodeGen(acc, methDecl.fqn.toString(), paramList, givesPerList)
                commandCodeGen(mi.sndCmd)
                outputBuilder.newLine
                outputBuilder.dedent

                // Removing Permission on parameters
                for (param <- paramList) {
                  param.paramCategory match {
                    case InParamCategory => {
                      //Add Reading Permission
                      outputBuilder.putln("//Remove Reading Permission to input parameters")
                      outputBuilder.setError("Permission Amount in not greater than '0'", param.coord)
                      outputBuilder.putln(s"assert Permission[This.${dlNd.fqn.toString()},${param.fqn.toString()}] > 0.0")
                      outputBuilder.clearError
                      outputBuilder.putln(s"Permission[This.${dlNd.fqn.toString()},${param.fqn.toString()}] := 0.0;")
                    }
                    case OutParamCategory => {
                      //Add Writing Permission
                      outputBuilder.putln("//Remove Writing Permission to onput parameters")
                      outputBuilder.setError("Permission is not sufficient 'write permission' to remove.", param.coord)
                      outputBuilder.putln(s"assert Permission[This.${dlNd.fqn.toString()},${param.fqn.toString()}] == 1.0")
                      outputBuilder.clearError
                      outputBuilder.putln(s"Permission[This.${dlNd.fqn.toString()},${param.fqn.toString()}] := 0.0;")
                    }
                  }
                }
                // Go to End
                outputBuilder.newLine
                outputBuilder.put("goto Done;")
                outputBuilder.newLine
                outputBuilder.put("Done:")

              }
              case _ => "" // Check TypeChecker line 279-280
            }
          }
        }

/**********************************************************/
/************** 'with' Command Code Generation **********/
/**********************************************************/

        case WithCmdNd(lock, tpl, guard, command, gpl) => {

          //assume class invariant
          assumeClassInv(classDeclNd)
          
          val lockTransContext = new TransContext("LockPermission", ExpCodeGen.getNamefromLockExp(lock))
          heapTransContext.reset()
          heapTransContext.setHeap("Permission")
          outputBuilder.indent

          //Lock Translation

          outputBuilder.goToBefore(savedLine) // goto after saved line
          outputBuilder.newLine;
          outputBuilder.put("var " + lockTransContext.getObjRef() + ": Ref;")
          outputBuilder.newLine
          outputBuilder.goToEnd()
          outputBuilder.newLine
          outputBuilder.put(lockTransContext.getObjRef() + ":=" + heapTransContext.getObjRef() + ";");
          outputBuilder.newLine

          // goto last line of the outputBuilder

          outputBuilder.put("preHeap := Heap;")
          outputBuilder.newLine
          outputBuilder.put("havoc tempHeap;")
          outputBuilder.newLine
          outputBuilder.put("havoc LockPermission;")
          heapTransContext.reset()

          //Get permissions from invariant.

          assumeInvariantPermission(classDeclNd, lockTransContext)

          //assert class invariant is defined - locked
          lockedClassInv(lock, "assert")

          //Collect all the locks and while assignment sum up all the lock expressions

          // assert object invariant definedness and assume object invariant
          lock match {
            case ThisExpNd(str) => {
              //lockPermissionMapsList
              for (classInv <- dlNd.asInstanceOf[ClassLike].directMembers)
                classInv match {
                  case ClassInvNd(exp) => {
                    val expList = ArrayBuffer[String]();
                    lockExpList = ExpCodeGen.lockedInvExpCodeGen(exp, lockTransContext, expList)
                    lockPerMaps :+ "LockPermission" + newPerMap;
                    newPerMap = newPerMap + 1;
                  }
                  case _ => ""
                }
            }
            case NameExpNd(nameNd) => {
              outputBuilder.putln(ExpCodeGen.buildBoogieExp(lock, heapTransContext))
              lockPerMaps :+ "LockPermission" + nameNd.toString();
              newPerMap = newPerMap + 1;
            }
            case _ => {}
          }

          lockedClassInv(lock, "assume")
          //assuming all the locks sum is b/w 0.0 and 1.0
          lockExpList :+ "assume (forall<x> r: Ref, f: Field x :: 0.0 <= " + lockTransContext.getHeap() + "[r,f] &&" + lockTransContext.getHeap() + "[r,f] <= 1.0)"

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

          // takes permission list - transfer permission from lock to thread
          for (tp <- tpl) {
            for ((lsn, amnt) <- tp.pmn.pm) {
              lsn match {
                case ObjectIdLSN(nameExp) => {
                  val amount: String = ExpCodeGen.simpleExpCodeGen(amnt)
                  //TODO
                }
                case _ => contracts.Contracts.toDo("Array Location Set Node")
              }
            }
          }
          // Lock Body
          outputBuilder.indent
          commandCodeGen(command)
          outputBuilder.dedent

          //gives permission list - translate permission from thread to lock

          for (gp <- tpl) {
            for ((lsn, amnt) <- gp.pmn.pm) {
              lsn match {
                case ObjectIdLSN(nameExp) => {
                  val amount: String = ExpCodeGen.simpleExpCodeGen(amnt)
                  // TODO
                }
                case _ => contracts.Contracts.toDo("Array Location Set Node")
              }
            }
          }

          //Assert the definedfness of invariant and assert the invariant releasing lock

          lockedClassInv(lock, "assert")

          outputBuilder.dedent
        }

/**********************************************************/
/************** 'assert' Command Code Generation **********/
/**********************************************************/

        case AssertCmdNd(exp) => {
          outputBuilder.newLine
          outputBuilder.put("//Expression Definedness Start")
          isExpressionDefined(exp, heapTransContext)
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

/**********************************************************/
/************** 'assume' Command Code Generation **********/
/**********************************************************/
        case AssumeCmdNd(exp) => {
          outputBuilder.newLine
          outputBuilder.put("//Expression Definedness Start")
          isExpressionDefined(exp, heapTransContext)
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

/**********************************************************/
/********************* Supporting Methods *****************/
/**********************************************************/

    def isExpressionDefined(exp: ExpNd, transContext: TransContext) {
      outputBuilder.newLine
      outputBuilder.clearError
      exp match {
        case NameExpNd(name: NameNd) => {
          val decl$ = name.decl.get
          decl$ match {
            case ObjDeclNd(isGhost, isConst, acc, ty, init) => {
              val hasType = getBaseOfPrimitiveType(ty.tipe.get);
              outputBuilder.setError("Expresion is not defined", decl$.coord)
              outputBuilder.putln("assert is" + hasType + "(" + transContext.getHeap() + "[" + transContext.getObjRef() + "," + decl$.fqn.toString() + "]" + ");")
              outputBuilder.clearError
            }
            case ParamDeclNd(isGhost, ty, paramCategory) => {
              val hasType = getBaseOfPrimitiveType(ty.tipe.get);
              outputBuilder.setError("Expresion is not defined", decl$.coord)
              outputBuilder.putln("assert is" + hasType + "(" + transContext.getHeap() + "[" + transContext.getObjRef() + "," + decl$.fqn.toString() + "]" + ");")
              outputBuilder.clearError
            }
            case LocalDeclNd(isGhost, isConst, ty, init, cmd) => {
              val hasType = getBaseOfPrimitiveType(ty.tipe.get);
              outputBuilder.setError("Expresion is not defined", decl$.coord)
              outputBuilder.putln("assert is" + hasType + "(" + transContext.getHeap() + "[" + transContext.getObjRef() + "," + decl$.fqn.toString() + "]" + ");")
              outputBuilder.clearError
            }
            case ThreadDeclNd(claimList, block) => {}
            case _ =>
          }
        }
        case MemberExpNd(x, name) => {
          isExpressionDefined(x, transContext)
          outputBuilder.goToBefore(savedLine)
          outputBuilder.newLine
          outputBuilder.putln("var that_" + newObject + ": Ref;")
          outputBuilder.putln("that_" + newObject + " := " + ExpCodeGen.buildBoogieExp(x, transContext))
          newObject = newObject + 1
          outputBuilder.goToEnd()
        }

        case BinaryOpExpNd(op: BinaryOperator, x: ExpNd, y: ExpNd) => {
          val denom_exp = ExpCodeGen.buildBoogieExp(y, transContext);
          op match {
            case SlashDivOp => outputBuilder.putln(s"assert ${denom_exp} != 0;")
            case WordDivOp => outputBuilder.putln(s"assert ${denom_exp} != 0;")
            case RemOp => outputBuilder.putln(s"assert ${denom_exp} != 0;")
            case _ =>
          }
          isExpressionDefined(x, transContext)
          isExpressionDefined(y, transContext)
        }

        case UnaryOpExpNd(op: UnaryOperator, x: ExpNd) => {
          isExpressionDefined(x, transContext)
        }

        case MemberExpNd(x: ExpNd, name: String) => {
          isExpressionDefined(x, transContext)
        }

        case ChainExpNd(ops: List[ChainingOperator], operands: List[ExpNd]) => {
          for (x <- operands) isExpressionDefined(x, transContext)
        }
        case FetchExpNd(x: ExpNd) => isExpressionDefined(x, transContext)

        case AsExpNd(x: ExpNd, _) => isExpressionDefined(x, transContext)

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

    def getBaseOfPrimitiveType(type$: Type): String = {
      type$ match {
        case PrimitiveType(base) => base.toString()

        case LocationType(primitiveType) => getBaseOfPrimitiveType(primitiveType)

        case ArrayLocationType(arrayType) => getBaseOfPrimitiveType(arrayType)

        case ArrayType(base: Type, bound: AST.ExpNd) => getBaseOfPrimitiveType(base)

        case _ => contracts.Contracts.toDo("Only Primtive and Location Types are allowed yet")
      }

    }

  }

  //Add Permission to input parameters of constructor

  def addPermissionToInputParameters(classDeclNd: ClassDeclNd) {
    //TODO
  }

  //Class Claim Code Generation

  def classClaimCodeGen(classDeclNd: ClassDeclNd) {
    heapTransContext.set("Permission", heapTransContext.objRef)
    arrayHeapTransContext.set("ArrayPermission", arrayHeapTransContext.getObjRef())

    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ClaimNd(pmn) =>
          {
            classClaimList :+ mem
            val perm_pairs = pmn.pm;
            for ((loc, amnt) <- perm_pairs) {
              loc match {
                case ObjectIdLSN(nameExp) =>
                  {
                    val fqn = NameManager.getFQN(loc)
                    val amount: String = ExpCodeGen.simpleExpCodeGen(amnt);
                    outputBuilder.putln("//Claim")
                    outputBuilder.setError("Permission Ammount is not valid: it must be 0.0 _< PermValue _< 1.0", amnt.coord)
                    outputBuilder.putln("assert isValidPermission(" + amount + ");")
                    outputBuilder.clearError
                    outputBuilder.putln(heapTransContext.getHeap() + "[" + heapTransContext.getObjRef() + "," + fqn + "] := " + heapTransContext.getHeap() + "[" + heapTransContext.getObjRef() + "," + fqn + "] + " + amount + ";")
                    outputBuilder.setError("Claimed permission amount makes total permission invalid ", mem.coord)
                    outputBuilder.putln("assert " + "0.0 <= " + heapTransContext.getHeap() + "[" + heapTransContext.getObjRef() + "," + fqn + "]" + " && " + heapTransContext.getHeap() + "[" + heapTransContext.getObjRef() + "," + fqn + "]" + "<=" + "1.0;")
                    outputBuilder.clearError
                  }
                case ArrayLSN(forDecl: ForDecl, offSet: ExpNd, bound: ExpNd, boundInclusive: Boolean, locSet: LocSetNd) =>
                  {
                    outputBuilder.putln("oldArrayPermission := ArrayPermission;")
                    outputBuilder.putln("havoc ArrayPermission;")
                    val fqn = NameManager.getFQN(loc)
                    val amount: String = ExpCodeGen.simpleExpCodeGen(amnt);
                    locSet match {
                      case a @ ObjectIdLSN(exp: ExpNd) => exp match {
                        case BinaryOpExpNd(indexOp, arrayId, size) => arrayHeapTransContext.setObjRef(arrayId.toString)
                        case _ => {}
                      }
                      case _ => {}
                    }
                    outputBuilder.putln("assume (forall<x> r:ArrayRef x, f : int :: (r == " + arrayHeapTransContext.getHeap() + "[" + arrayHeapTransContext.getObjRef() + "," + fqn + "]" + ")" + "&&" + "(" + offSet + "<= f && f <" + bound + ") ==> ArrayPermission[r,f] == " + amount + ";");

                  }
                case _ => contracts.Contracts.toDo("Location Set Node with Array")
              }
            }
          }
        case _ =>
      }
    }
    if(classClaimList.nonEmpty) {
      outputBuilder.newLine
      outputBuilder.setError("Maximum Permission Amount Must Not Exceed \'1.0\'", dlNd.coord)
      outputBuilder.put("assert (forall<x> r: Ref, f: Field x :: 0.0 <= Permission[r,f] && Permission[r,f] <= 1.0);")
      outputBuilder.newLine
      outputBuilder.clearError
    }
    heapTransContext.reset()

  }
  //Object Initialization Code Generation

  def objectsInitCodeGen(classDeclNd: ClassDeclNd) {
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
    nameExp = new ArrayBuffer[String]()
    for (prc <- preCnds) {
      heapTransContext.setHeap("Heap")
      outputBuilder.newLine
      outputBuilder.put("assume " + ExpCodeGen.buildBoogieExp(prc.condition, heapTransContext) + ";")
      outputBuilder.newLine
      val nameExps = ExpCodeGen.nameExpCodeGen(prc.condition, nameExp)
      if (!(nameExps.isEmpty)) {
        outputBuilder.newLine
        outputBuilder.put("assume ");
        for (name <- nameExps) {
          outputBuilder.putln(s" (forall<x> r:Ref, f : Field x :: !((r== This.${dlNd.fqn.toString()} && f == ${name}) ==> Heap[r,f] == oldHeap[r,f]));")
        }
        outputBuilder.newLine
      }

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

  def claimCodeGen(claimNd: ClaimNd, classDeclNd: ClassDeclNd) {
    val perm_pairs = claimNd.pmn.pm;
    // var result = "";
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
      //Assume the initial values of claimed locations

      def assumeInitialValues(classDeclNd: ClassDeclNd) {
        var objDecl, objInits = ""
        for (mem <- classDeclNd.directMembers) {
          mem match {
            case ObjDeclNd(isGhost: Boolean, isConst: Boolean, acc: Access, ty: TypeNd, init: InitExpNd) => {
              objDecl = objDeclCodeGen(isConst, acc, ty, mem.fqn.toString)
              val objType: String = TypeCodeGen(ty)
              val initExp: String = ExpCodeGen.initExpCodeGen(init, heapTransContext)
              outputBuilder.putln("//Initialize Heap")
              outputBuilder.putln("assume " + heapTransContext.heap + "[" + heapTransContext.objRef + "," + mem.fqn.toString + "] := " + initExp + " ;")
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
    }
  }

  def classInvCodeGen(classDeclNd: ClassDeclNd) {
    outputBuilder.putln("//Class Invariant")
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
      mem match {
        case ClassInvNd(exp) => {
          val invString = ExpCodeGen.InvExpCodeGen(exp, heapTransContext)
          outputBuilder.setError("Invariant does not hold", exp.coord)
          outputBuilder.putln("assert " + invString + ";")
          outputBuilder.clearError
        }
        case _ => {}
      }
  }

  def assumeClassInv(classDeclNd: ClassDeclNd) {
    outputBuilder.putln("//Class Invariant")
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
      mem match {
        case ClassInvNd(exp) => {
          val invString = ExpCodeGen.InvExpCodeGen(exp, heapTransContext)
          outputBuilder.putln("assume " + invString + ";")
        }
        case _ => {}
      }
  }

  def assertLockedClassInv(classDeclNd: ClassDeclNd, transContext: TransContext) {
    outputBuilder.putln("//Class Invariant")
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
      mem match {
        case ClassInvNd(exp) => {
          val invString = ExpCodeGen.InvExpCodeGen(exp, transContext)
          outputBuilder.putln("assert " + invString + ";")
        }
        case _ => {}
      }
  }

  def assumeInvariantPermission(classDeclNd: ClassDeclNd, lockTransContext: TransContext) {
    outputBuilder.newLine
    outputBuilder.putln("//Class Invariant")
    var nameExp = new ArrayBuffer[String]()
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
      mem match {
        case ClassInvNd(exp) => {
          var inv: String = "";
          val invString = ExpCodeGen.InvPermExpCodeGen(exp, lockTransContext,inv)
          if(!invString.equals("")){
            outputBuilder.newLine
            outputBuilder.putln("assume " + invString + ";")
          }
          //Assume the locations not mentioned in the invariant have no permissions by thread.
          val nameList = ExpCodeGen.nameExpCodeGen(exp, nameExp)
          for (name <- nameList) {
            outputBuilder.putln(s"assume (forall<x> r:Ref, f: Field x :: !(r==${lockTransContext.getObjRef()} && f== ${name}) ==> LockPermission[r, f] == 0.0);")
          }
        }
        case _ => {}
      }
    heapTransContext.reset()
  }

  def lockForceClassInvariant(stmt: String, context: TransContext) {
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

  def loopInvCodeGen(loopInv: LoopInvNd) {
    //Check Loop Invariant is Defined
    heapTransContext.reset()
    outputBuilder.newLine
    outputBuilder.put("//loop Invariant")
    val invString = ExpCodeGen.InvExpCodeGen(loopInv.exp, heapTransContext)
    outputBuilder.newLine
    outputBuilder.setError("Invariant does not hold", loopInv.exp.coord)
    outputBuilder.putln("invariant " + invString + ";")
    outputBuilder.clearError

    //Inferring loop Invariant for frame problem
    // Unchanged parts of Heap must remain same. [Frame Condition]
    nameExp = new ArrayBuffer[String]()
    val nameExps = ExpCodeGen.nameExpCodeGen(loopInv.exp, nameExp)
    if (!nameExps.isEmpty) {
      for (fqn <- nameExps) {
        outputBuilder.putln("invariant (forall<x> r:Ref, f : Field x :: !(r == this && f == " + fqn + ") ==>Heap[r, f] == oldHeap[r, f]);")
      }
    }

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
    //assert the defindness of class invariant and assume the invariant
    val lockContext = new TransContext("LockPermission", ExpCodeGen.getNamefromLockExp(lock));
    heapTransContext.reset()
    val baseContext = new TransContext(heapTransContext.getHeap(), heapTransContext.getObjRef())

    lock match {
      case ThisExpNd(str) => {
        for (classInv <- dlNd.asInstanceOf[ClassLike].directMembers) {
          classInv match {
            case ClassInvNd(exp) => {
              // assert definedness
              outputBuilder.newLine
              outputBuilder.put("//Assert defindness of object invariant and assume object invariance")
              outputBuilder.newLine
              nameExp = new ArrayBuffer[String]()
              val nameExps = ExpCodeGen.nameExpCodeGen(exp, nameExp)
              if (!(nameExps.isEmpty)) {
                outputBuilder.setError("Does not have enough permission(0)", exp.coord)
                outputBuilder.newLine
                outputBuilder.put("assert ");
                if(nameExps.size > 1) {
                for( i <- 0 to nameExps.size-1) {
                  outputBuilder.put(lockContext.heap + "[" + lockContext.getObjRef() + "," + nameExps(i) + "] > 0.0 &&")
                }
                outputBuilder.put(lockContext.heap + "[" + lockContext.getObjRef() + "," + nameExps.last + "] > 0.0")
                }
                else {
                  outputBuilder.put(lockContext.heap + "[" + lockContext.getObjRef() + "," + nameExps(0) + "] > 0.0")
                }
                outputBuilder.put(";")
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
      }
      case NameExpNd(nameNd) => //TODO
      case _ => {}
    }
  }

  def objDeclCodeGen(isConst: Boolean, acc: Access, ty: TypeNd, fqn: String): String = {
    val objType: String = TypeCodeGen(ty)
    val objCode = "\nconst unique " + fqn + ":" + "Field " + objType + ";"
    return objCode
  }

}
