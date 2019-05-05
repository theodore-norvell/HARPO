package boogieBackEnd;
import frontEnd.AST;
import frontEnd.AST._;
import util.Format;
import util.OutputBuilder;
import contracts.Contracts;

private class ClassCodeGen(val dlNd: DeclNd, var outputBuffer: OutputBuilder) {

  //global translation context has most generic form of heap and object reference,
  //local translation contexts are made while changing to particular Heap and object reference i.e preHeap, tempHeap, oldHeap, and *_this, *_that 

  var transContext = new TransContext("Heap","this_"+dlNd.fqn.toString) 
  
  val expObj = new ExpCodeGen;

  def classCodeGen(): OutputBuilder = {

    // Class identifier

    val className = dlNd.name;
    outputBuffer.newLine
    outputBuffer.put("const unique " + dlNd.fqn.toString + ":className;")

    //Object Identifier

    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
      mem match {
        case ObjDeclNd(isGhost: Boolean, 
                        isConst: Boolean, 
                        acc: Access, 
                        ty: TypeNd, 
                        init: InitExpNd) => {
          val objType: String = TypeCodeGen(ty)
          outputBuffer.newLine
          outputBuffer.put("const unique " + mem.fqn.toString +": Field " + objType + ";")
          outputBuffer.newLine
        }
        case _ => {}
      }

    // Constructor Procedure

    outputBuffer.newLine
    outputBuffer.put("procedure " + dlNd.fqn.toString + ".constructor" + "("+transContext.getObjRef()+":Ref)") // dlNd.fqn is class name
    outputBuffer.newLine
    outputBuffer.put("requires dtype("+transContext.getObjRef()+") <: " + dlNd.fqn.toString)
    outputBuffer.newLine
    outputBuffer.put("modifies "+ transContext.getHeap()+ ";")
    outputBuffer.newLine
    outputBuffer.put("modifies Permission;")
    outputBuffer.newLine
    outputBuffer.put("{")
    outputBuffer.newLine
    outputBuffer.put("var Permission : PermissionType where (forall <x> r: Ref, f: Field :: Permission[r,f] == 0.0)")
    outputBuffer.newLine
    outputBuffer.put("var oldHeap:HeapType;")
    outputBuffer.newLine
    outputBuffer.put("havoc "+ transContext.getHeap()+ ";")

    //Class Claim Code Generation

    classClaimCodeGen()

    //Objects initialization Code Generation, It Initializes the Heap

    objectsInitCodeGen()

    //Class Invariant Code

    classInvCodeGen()

    //Closing of Constructor Procedure
    outputBuffer.newLine
    outputBuffer.put("}")

    //Thread Procedure

    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)    
      mem match {
        case ThreadDeclNd(claimList: List[ClaimNd], block: CommandNd) => {
          outputBuffer.newLine
          outputBuffer.put("procedure " + dlNd.name + "." + mem.name + " (" + transContext.getObjRef() + " : Ref)")
          outputBuffer.newLine
          outputBuffer.put("dtype("+ transContext.getObjRef()+ ") <: " + dlNd.fqn.toString)
          outputBuffer.newLine
          outputBuffer.put("modifies "+ transContext.getHeap()+ ";")
          outputBuffer.newLine
          outputBuffer.put("{")
          outputBuffer.newLine
          outputBuffer.put("var Permission: PermissionType where (forall<x> r: Ref, f: Field x :: Permission[r,f] == 0.0)")
          outputBuffer.newLine
          outputBuffer.put("var oldHeap, preHeap, tempHeap: HeapType;")
          outputBuffer.newLine

          for (claim <- claimList) { //Thread Claim Code Generation
            claimCodeGen(claim)
          }

          commandCodeGen(block) //Thread Body Code Generation
        }

        case _ => {}
      }
    outputBuffer
  }
  
  //Class Claim Code Generation

  private def classClaimCodeGen() {
    val localTransContext = new TransContext("Permission", transContext.getObjRef)
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ClaimNd(pmn) =>
          val perm_pairs = pmn.pm;
          var result = "";
          for ((loc, amnt) <- perm_pairs) {
            loc match {
              case ObjectIdLSN(nameExp) => {
                val fqn = dlNd.name + "." + nameExp.name.qn.toString
                 val amount: String = expObj.expCodeGen(amnt);
                outputBuffer.newLine
                outputBuffer.put("//Claim")
                outputBuffer.newLine
                outputBuffer.put(localTransContext.getHeap()+"["+localTransContext.getObjRef()+"," + fqn + "] := "+ localTransContext.getHeap() + "["+localTransContext.getObjRef()+"," + fqn + "] + " + amount + ";")
              }
              case _ => contracts.Contracts.toDo("Location Set Node with Array")
            }
          }
        case _ =>
      }
    }
    outputBuffer.newLine // error message
    outputBuffer.put("assert(forall<x> r: Ref, f: Field x :: Permission[r,f] <= 1.0);")
  }
  
  //Object Initialization Code Generation
  
  private def objectsInitCodeGen() {
    var objDecl, objInits = ""
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {
        case ObjDeclNd(isGhost: Boolean, isConst: Boolean, acc: Access, ty: TypeNd, init: InitExpNd) => {
          objDecl = objDeclCodeGen(isConst, acc, ty, mem.fqn.toString)
          val objType: String = TypeCodeGen(ty)
          val exp: String = new ExpCodeGen().initExpCodeGen(init, mem.fqn.toString)
          outputBuffer.newLine
          outputBuffer.put("//Initialize Heap")
          outputBuffer.newLine
          outputBuffer.put(transContext.getHeap()+"["+transContext.getObjRef()+"," + mem.fqn.toString + "] := " + exp + ";")
          // building the expression not required for this subroutine, This subroutine contains the initialization of objects
          outputBuffer.newLine
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
        commandCodeGen(fstCmd)
        commandCodeGen(sndCmd)
      }

      case LocalDeclCmdNd(decl) => {
        val objType: String = TypeCodeGen(decl.ty)
        outputBuffer.newLine
        outputBuffer.put("Heap[" + transContext.getObjRef() + "," + decl.fqn + "] := " + expObj.expCodeGen(decl.init) + ";")
        outputBuffer.newLine
      }

      case AssignmentCmdNd(lhs, rhs) => {

        // Check the Permissions

        for (l_exp <- lhs.init) {
          outputBuffer.newLine
          outputBuffer.put(expObj.expCodeGen(l_exp)) // build with TransContext
          if (!(l_exp.equals(lhs.last))) // Not to put comma for last expression in sequence
            outputBuffer.put(",")
        }
        outputBuffer.put(expObj.expCodeGen(lhs.last))
        outputBuffer.put(" = ")

        for (r_exp <- rhs) {
          outputBuffer.put(expObj.expCodeGen(r_exp))
          if (!(r_exp.equals(rhs.last))) // Not to put comma for last expression in sequence
            outputBuffer.put(",")
        }
        outputBuffer.put(expObj.expCodeGen(rhs.last))
        outputBuffer.put(";")
      }

      case CallCmdNd(method, argList) => {
      }

      case IfCmdNd(guard, thenCmd, elseCmd) => {
      }

      case WhileCmdNd(guard, lil, body) => { //TODO use the Loop Invariant
        
        outputBuffer.newLine
        outputBuffer.put("while(" + "true" + ")") // build guard expression
        outputBuffer.newLine
        outputBuffer.put("{")
        commandCodeGen(body)
        outputBuffer.newLine
        outputBuffer.put("}")
      }

      case ForCmdNd(decl, repetitions, lil, body) => {
      }

      case CoForCmdNd(decl, repetitions, claimList, body) => {
      }

      case CoCmdNd(claimList, fstCmd, sndCmd) => {
      }

      case AcceptCmdNd(methodImplementationList) => {
        for (mi <- methodImplementationList) {
          outputBuffer.newLine
          outputBuffer.put("goto " + mi.nameNd + ";")
          outputBuffer.newLine
          outputBuffer.put(mi.nameNd + ":")
          val methDecl = for (mem <- dlNd.asInstanceOf[ClassLike].directMembers)
            mem match {
              case MethodDeclNd(acc, 
                                paramList, 
                                preCndList, 
                                postCndList, 
                                givesPerList, 
                                takesPerList, 
                                borrowsPerList) => {
                if (mem.name.toString.equals(mi.nameNd.toString)) { // consider the parameters and borrows
                  methTakesPerCodeGen(acc, mem.name, paramList, takesPerList)
                  methPreCondCodeGen(acc, mem.name, paramList, preCndList)  
                  outputBuffer.newLine
                  outputBuffer.put("//Method Implementatiom")
                  //Figure out the need of parameter list
                  commandCodeGen(mi.fstCmd)
                  methPostCondCodeGen(acc, mem.name, paramList, postCndList)
                  methGivesPerCodeGen(acc, mem.name, paramList, givesPerList)
                  commandCodeGen(mi.sndCmd)
                }
              }
              case _ => "" // Check TypeChecker line 279-280
            }
        }
      }

      case WithCmdNd(lock, tpl, guard, command, gpl) => {

        //lock expression must be an object implementing interface lock
        //guard translation

        //Lock Translation
        outputBuffer.newLine;
        outputBuffer.put("var that : Ref;")
        outputBuffer.newLine
        outputBuffer.put("that := " + dlNd.fqn.toString+"_"+expObj.lockExpCodeGen(lock) + ";");
        outputBuffer.newLine
        outputBuffer.put("preHeap := Heap;")
        // access class invariant for this object

        // Assume The class invariant with for Locked object
        assumeClassInvariant()

        // takes permission list
        for (tp <- tpl)
          for ((lsn, amnt) <- tp.pmn.pm)
            lsn match {
              case ObjectIdLSN(nameExp) => {
                val amount : String = expObj.expCodeGen(amnt)
                outputBuffer.newLine
                outputBuffer.put("oldPermission := Permission;")
                outputBuffer.newLine
                outputBuffer.put("if(Permission[this," + lsn.getName() + "] == 0.0")
                outputBuffer.newLine
                outputBuffer.put("{")
                outputBuffer.newLine
                outputBuffer.put("havoc Heap_temp")
                outputBuffer.newLine
                outputBuffer.put("Heap[this," + lsn.getName() + "];")
                outputBuffer.newLine
                outputBuffer.put("Permission[this," + lsn.getName() + "] := " + "Permission[this," + lsn.getName() + "] + " + amount + ";")
              }
              case _ => contracts.Contracts.toDo("Array Location Set Node")
            }
        commandCodeGen(command)

        assertClassInvariant()

        //gives permission list
      }

      case AssertCmdNd(assertion) => {
        val expCode = expObj.expCodeGen(assertion)
        val parts = expCode.split(" ")
        val per = "\nassert Permission[this," + parts(0) + "] > 0.0 ;" // First Part
        val assertCmdCode = per + "\nassert Heap[this," + parts(0) + "] " + parts(1) + ";"
        outputBuffer.newLine
        outputBuffer.put(assertCmdCode)
      }

      case AssumeCmdNd(assumption) => {
        val expCode = expObj.expCodeGen(assumption)
        val parts = expCode.split(" ")
        val per = "\nassume Permission[this," + parts(0) + "] > 0.0 ;" //First Part
        val assumeCmdCode = per + "\nassume Heap[this," + parts(0) + "] " + parts(1) + ";"
        outputBuffer.newLine
        outputBuffer.put(assumeCmdCode)
      }
      case _ => {}
    }
  }

  //Method's 'pre' Condition Code Generation
  def methPreCondCodeGen(acc: Access, name: String, params: List[ParamDeclNd], preCnds: List[PreCndNd]) {
    outputBuffer.newLine
    outputBuffer.put("//Pre Condition(s)")
    outputBuffer.newLine
    outputBuffer.put("oldHeap := Heap;")
    outputBuffer.newLine
    outputBuffer.put("havoc Heap")
    for (prc <- preCnds) {
      val exp = prc.condition;
      transContext.set("Heap", "this")
      outputBuffer.newLine
      outputBuffer.put("assume " + expObj.buildExp(prc.condition, transContext)) // assume and assert
    }
  }

  //Method's 'takes' Specification Code Generation
  def methTakesPerCodeGen(acc: Access, name: String, params: List[ParamDeclNd], takesPers: List[TakesPerNd]) {
    outputBuffer.newLine
    outputBuffer.put("//Taking Permission(s)")
    for (tp <- takesPers)
      for ((lsn, expNd) <- tp.pmn.pm)
        lsn match {
          case ObjectIdLSN(nameExp) => {
            val amount: String = new ExpCodeGen().expCodeGen(expNd)
            outputBuffer.newLine
            outputBuffer.put("oldPermission := Permission;")
            outputBuffer.newLine
            outputBuffer.put("if(Permission[this," + lsn.getName() + "] == 0.0)")
            outputBuffer.newLine
            outputBuffer.put("{")
            outputBuffer.newLine
            outputBuffer.put("havoc Heap_temp")
            outputBuffer.newLine
            outputBuffer.put("Heap[this," + dlNd.name + "." + lsn.getName() + "];")
            outputBuffer.newLine
            outputBuffer.put("Permission[this," + dlNd.name + "." + lsn.getName() + "] := " + "Permission[this," + dlNd.name + "." + amount + "];")
            outputBuffer.newLine
            outputBuffer.put("}")
          }
          case _ => contracts.Contracts.toDo("Array Location Set Node")
        }
  }

  //Method's 'post' Condition Code Generation
  def methPostCondCodeGen(acc: Access, name: String, params: List[ParamDeclNd], postCnds: List[PostCndNd]) {
    outputBuffer.newLine
    outputBuffer.put("//Post Condition(s)")
    for (poc <- postCnds) {
      val exp = poc.condition;
      val tempObj = new ExpCodeGen;
      transContext.set("Heap", "this")
      outputBuffer.newLine
      val nameExps = tempObj.nameExpCodeGen(exp)
      for (name <- nameExps) {
        outputBuffer.newLine
        outputBuffer.put("assert Permission[this," + name + "] > 0.0;")
      }
      outputBuffer.newLine
      outputBuffer.put("assert " + expObj.buildExp(poc.condition, transContext)) // assume and assert
    }
  }

  //Method's 'gives' Specification Code Generation
  
  def methGivesPerCodeGen(acc: Access, name: String, params: List[ParamDeclNd], givesPers: List[GivesPerNd]) {
    outputBuffer.newLine
    outputBuffer.put("//Giving Permissions(s)")
    for (tp <- givesPers)
      for ((lsn, amnt) <- tp.pmn.pm)
        lsn match {
          case ObjectIdLSN(nameExp) => {
            val amount = expObj.expCodeGen(amnt)
            transContext.set("Permission", "this")
            outputBuffer.newLine
            outputBuffer.put("assert "); expObj.buildExp(nameExp, transContext); outputBuffer.put(" := "); expObj.buildExp(nameExp, transContext); outputBuffer.put(" - "); outputBuffer.put(amount + ";");
          }
          case _ => contracts.Contracts.toDo("Array Location Set Node")
        }
  }
  
  def assertClassInvariant() {
    outputBuffer.newLine;
    outputBuffer.put("//Asserting class invariant for Locked object")
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {       
        case ClassInvNd(exp) => {
          val invString = expObj.InvExpCodeGen(exp,mem.fqn.toString, transContext.getObjRef())
          outputBuffer.newLine
          outputBuffer.setError("Invariant does not hold", mem.coord)
          outputBuffer.put("assert "+invString)
          outputBuffer.newLine
          outputBuffer.clearError
          }
        
        case _ => {}
      }
    }
  }

  def assumeClassInvariant() {
    outputBuffer.newLine;
    outputBuffer.put("//Asserting class invariant for Locked object")
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) {
      mem match {       
        case ClassInvNd(exp) => {
          val invString = expObj.InvExpCodeGen(exp,mem.fqn.toString, transContext.getObjRef())
          outputBuffer.newLine
          outputBuffer.put("assume "+invString)
          outputBuffer.newLine
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
      val amount: String = expObj.expCodeGen(amnt)
      outputBuffer.newLine
      outputBuffer.put("//Claim")
      outputBuffer.newLine
      outputBuffer.put("Permission["+transContext.getObjRef()+ "," + name + "] := Permission[" +transContext.getObjRef()+ "," + name + "] + " + amount + ";")
    }
  }

  private def classInvCodeGen() {
    outputBuffer.newLine
    outputBuffer.put("//Class Invariant")
    for (mem <- dlNd.asInstanceOf[ClassLike].directMembers) 
      mem match {
        case ClassInvNd(exp) => {
          val invString = expObj.InvExpCodeGen(exp,dlNd.fqn.toString,transContext.getObjRef())
          outputBuffer.newLine
          outputBuffer.setError("Invariant does not hold", mem.coord)
          outputBuffer.put("assert "+ invString)
          outputBuffer.newLine
          outputBuffer.clearError
        }
        case _ => {}
  }
  }
  
  def loopInvCodeGen(loopInv : LoopInvNd) {
    outputBuffer.newLine
    outputBuffer.put("//loop Invariant")
          val invString = expObj.InvExpCodeGen(loopInv.exp, dlNd.fqn.toString, transContext.getObjRef())
          outputBuffer.newLine
          outputBuffer.setError("Invariant does not hold", loopInv.coord)
          outputBuffer.put("assert "+invString)
          outputBuffer.newLine
          outputBuffer.clearError
  }
    
  def objDeclCodeGen(isConst: Boolean, acc: Access, ty: TypeNd, fqn: String): String = {
    val objType: String = TypeCodeGen(ty)
    val objCode = "\nconst unique " + fqn + ":" + "Field " + objType + ";"
    return objCode
  }

}