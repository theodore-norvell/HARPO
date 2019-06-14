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

private class ClassCodeGen( val dlNd : DeclNd, val outputBuffer : OutputBuilder ) {

  //global translation context has most generic form of heap and object reference,
  //local translation contexts are made while changing to particular Heap and object reference i.e preHeap, tempHeap, oldHeap, and *_this, *_that

  var transContext = new TransContext( "Heap", "This_" + dlNd.fqn.toString )

  val expObj = new ExpCodeGen;

  var nameExp = ArrayBuffer[String]();

  var classInvList = List[ClassInvNd]();

  var classClaimList = List[ClaimNd]();

  var globalObjectsList = List[ObjDeclNd]();

  var lockExpList = List[String]() //lock expression must be an object implementing interface lock

  def classCodeGen() : Unit = {

    classIdentifierCodeGen()

    objectIdentifierCodeGen()

    constructorProcedureCodeGen()

    threadProcedureCodeGen()
  }

  def getOutputBuilder : OutputBuilder = this.outputBuffer;

  // Class Identifier Code Generation
  private def classIdentifierCodeGen() {
    outputBuffer.newLine
    outputBuffer.put( "const unique " + dlNd.fqn.toString + ":ClassName;" )
  }

  //Object Identifier Code Generation

  private def objectIdentifierCodeGen() {
    for ( mem <- dlNd.asInstanceOf[ClassLike].directMembers )
      mem match {
        case ObjDeclNd( isGhost : Boolean,
          isConst : Boolean,
          acc : Access,
          ty : TypeNd,
          init : InitExpNd ) => {
          globalObjectsList :+ mem
          val objType : String = TypeCodeGen( ty )
          outputBuffer.newLine
          outputBuffer.put( "const unique " + mem.fqn.toString + ": Field " + objType + ";" )
          outputBuffer.newLine
        }
        case _ => {}
      }
  }

  // Constructor Procedure Code Generation

  private def constructorProcedureCodeGen() {

    outputBuffer.newLine
    outputBuffer.put( "procedure " + dlNd.fqn.toString + ".constructor" + "(" + transContext.objRef + ":Ref)" ) // dlNd.fqn is class name
    outputBuffer.newLine
    outputBuffer.put( "requires dtype(" + transContext.objRef + ") <: " + dlNd.fqn.toString + ";" )
    outputBuffer.newLine
    outputBuffer.put( "modifies " + transContext.getHeap() + ";" )
    outputBuffer.newLine
    outputBuffer.put( "{" )
    outputBuffer.newLine
    outputBuffer.indent
    outputBuffer.put( "var Permission : PermissionType where (forall <x> r: Ref, f: Field x :: Permission[r,f] == 0.0);" )
    outputBuffer.newLine
    outputBuffer.put( "var oldHeap:HeapType;" )
    outputBuffer.newLine
    outputBuffer.put( "havoc " + transContext.getHeap() + ";" )
    //Class Claim Code Generation
    classClaimCodeGen()
    //Objects initialization Code Generation, It Initializes the Heap
    objectsInitCodeGen()
    //Class Invariant Code
    classInvCodeGen()
    //Closing of Constructor Procedure
    outputBuffer.newLine
    outputBuffer.dedent
    outputBuffer.put( "}" )

  }

  //Thread Procedure Code Generation

  private def threadProcedureCodeGen() {
    for ( mem <- dlNd.asInstanceOf[ClassLike].directMembers )
      mem match {
        case ThreadDeclNd( claimList : List[ClaimNd], block : CommandNd ) => {
          outputBuffer.newLine
          outputBuffer.put( "procedure " + dlNd.name + "." + mem.name + " (" + transContext.objRef + " : Ref)" )
          outputBuffer.newLine
          outputBuffer.put( "requires dtype(" + transContext.objRef + ") <: " + dlNd.fqn.toString + ";" )
          outputBuffer.newLine
          outputBuffer.put( "modifies " + transContext.getHeap() + ";" )
          outputBuffer.newLine
          outputBuffer.put( "{" )
          outputBuffer.newLine
          outputBuffer.indent
          outputBuffer.put( "var Permission: PermissionType where (forall<x> r: Ref, f: Field x :: Permission[r,f] == 0.0);" )
          outputBuffer.newLine
          outputBuffer.put( "var oldPermission: PermissionType where (forall<x> r: Ref, f: Field x :: oldPermission[r,f] == 0.0);" )
          outputBuffer.newLine
          outputBuffer.put( "var lockPermission: PermissionType where (forall<x> r: Ref, f: Field x :: lockPermission[r,f] == 0.0);" )
          outputBuffer.newLine
          outputBuffer.put( "var oldHeap, preHeap, tempHeap: HeapType;" )
          outputBuffer.newLine

          //Thread Claim Code Generation
          for ( claim <- claimList ) {
            claimCodeGen( claim )
          }

          commandCodeGen( block ) //Thread Body Code Generation

          outputBuffer.newLine
          outputBuffer.dedent
          outputBuffer.put( "}//end of Thread Procedure" )
        }

        case _ => {}
      }
  }

  //Class Claim Code Generation

  private def classClaimCodeGen() {
    transContext.set( "Permission", transContext.objRef )
    for ( mem <- dlNd.asInstanceOf[ClassLike].directMembers ) {
      mem match {
        case ClaimNd( pmn ) =>
          classClaimList :+ mem
          val perm_pairs = pmn.pm;
          var result = "";
          for ( ( loc, amnt ) <- perm_pairs ) {
            loc match {
              case ObjectIdLSN( nameExp ) => {
                val fqn = dlNd.name + "." + nameExp.name.qn.toString
                val amount : String = expObj.simpleExpCodeGen( amnt );
                outputBuffer.newLine
                outputBuffer.put( "//Claim" )
                outputBuffer.newLine
                outputBuffer.put( transContext.getHeap() + "[" + transContext.getObjRef() + "," + fqn + "] := " + transContext.getHeap() + "[" + transContext.getObjRef() + "," + fqn + "] + " + amount + ";" )
              }
              case _ => contracts.Contracts.toDo( "Location Set Node with Array" )
            }
          }
        case _ =>
      }
    }
    outputBuffer.newLine
    outputBuffer.setError( "Maximum Permission Amount Must Not Exceed \'1.0\'", dlNd.coord )
    outputBuffer.put( "assert (forall<x> r: Ref, f: Field x :: Permission[r,f] <= 1.0);" )
    outputBuffer.newLine
    outputBuffer.clearError
    transContext.reset()
  }

  //Object Initialization Code Generation

  private def objectsInitCodeGen() {
    var objDecl, objInits = ""
    for ( mem <- dlNd.asInstanceOf[ClassLike].directMembers ) {
      mem match {
        case ObjDeclNd( isGhost : Boolean, isConst : Boolean, acc : Access, ty : TypeNd, init : InitExpNd ) => {
          objDecl = objDeclCodeGen( isConst, acc, ty, mem.fqn.toString )
          val objType : String = TypeCodeGen( ty )
          val initExp : String = expObj.initExpCodeGen( init )
          outputBuffer.newLine
          outputBuffer.put( "//Initialize Heap" )
          outputBuffer.newLine
          outputBuffer.put( transContext.heap + "[" + transContext.objRef + "," + mem.fqn.toString + "] := " + initExp + ";" )
          // building the expression not required for this subroutine, This subroutine contains the initialization of objects
          outputBuffer.newLine
        }
        case _ => ""
      }
    }
  }

  private def commandCodeGen( cmd : CommandNd ) {

    cmd match {

      case SkipCmdNd() => { //Do Nothing
      }

      case SeqCommandNd( fstCmd, sndCmd ) => {
        outputBuffer.indent
        commandCodeGen( fstCmd )
        commandCodeGen( sndCmd )
        outputBuffer.dedent
      }

      case LocalDeclCmdNd( decl ) => {
        val objType : String = TypeCodeGen( decl.ty )
        outputBuffer.newLine
        outputBuffer.put( "Heap[" + transContext.objRef + "," + decl.fqn.toString() + "] := " + expObj.simpleExpCodeGen( decl.init ) + ";" )
        outputBuffer.newLine
      }

      case AssignmentCmdNd( lhs, rhs ) => {

        // Sum up all locked Permissions first, later put it in assignment defindness
        // Assert Write Permission on LHS and ReadPermission on LHS
        //isAssignmentDefined(lhs.toList, rhs.toList)

        outputBuffer.newLine
        outputBuffer.put( "//Check Assignment Defindness" )
        outputBuffer.newLine
        transContext.reset()
        val localTransContext = transContext;
        localTransContext.setHeap( "Permission" )
        val expPairs = lhs zip rhs
        for ( ( lhs, rhs ) <- expPairs ) {
          val lhs_result = expObj.buildWritingPerExp( lhs, localTransContext )
          outputBuffer.setError( "Do not have enough permission(s) on LHS of assignment", lhs.coord )
          if ( !( lockExpList.isEmpty ) ) {
            for ( lockPer <- lockExpList ) {
              outputBuffer.put( "assert " )
              outputBuffer.put( lockPer.toString() + " + " )
            }
            outputBuffer.put( lhs_result + ";" ) // Two asserts instead of one
            outputBuffer.newLine
            outputBuffer.clearError
          } else {
            outputBuffer.put( "assert " + lhs_result + ";" ) // Two asserts instead of one
            outputBuffer.newLine
            outputBuffer.clearError
          }
          nameExp = new ArrayBuffer[String]()
          val nameExps = expObj.nameExpCodeGen( rhs, nameExp )
          if ( !( nameExps.isEmpty ) ) {
            outputBuffer.newLine
            outputBuffer.setError( "Permission amount should be greater than 0.0", rhs.coord )

            if ( !( lockExpList.isEmpty ) ) {
              outputBuffer.newLine
              outputBuffer.put( "assert " )
              val lockExp$ = lockExpList.dropRight( 1 );
              for ( lockPer <- lockExp$ )
                outputBuffer.put( lockPer.toString() + "> 0.0" + " && " )
              outputBuffer.put( lockExpList.last + " > 0.0;" )
              outputBuffer.newLine
              outputBuffer.put( "assert " )
              val nameExp$ = nameExps.dropRight( 1 )
              for ( name <- nameExp$ )
                outputBuffer.put( "Permission[" + localTransContext.getObjRef() + "," + name + "]" + " > 0" + "&&" )
              outputBuffer.put( "Permission[" + localTransContext.getObjRef() + "," + nameExps.last + "] > 0.0;" )
              outputBuffer.newLine
              outputBuffer.clearError
            } else {
              outputBuffer.newLine
              outputBuffer.put( "assert " )
              val nameExp$ = nameExps.dropRight( 1 )
              for ( name <- nameExp$ )
                outputBuffer.put( "Permission[" + localTransContext.getObjRef() + "," + name + "]" )
              outputBuffer.put( "Permission[" + localTransContext.getObjRef() + "," + nameExps.last + "] > 0.0;" )
              outputBuffer.newLine
              outputBuffer.clearError
            }
          }
        }
        //val rhs_result = expObj.buildReadingPerExp(rhs, localTransContext)
        // one glitch while translating the IntLiteralExpressionNode
        outputBuffer.newLine
        outputBuffer.put( "//Check Assignment Defindness Ends" )

        // assignment translation context
        transContext.reset()
        outputBuffer.newLine
        outputBuffer.put( "//Assignment Command" )
        outputBuffer.newLine
        for ( l_exp <- lhs.init ) {
          outputBuffer.put( expObj.buildBoogieExp( l_exp, transContext ) ) // build with TransContext
          if ( !( l_exp.equals( lhs.last ) ) ) // Not to put comma for last expression in sequence
            outputBuffer.put( "," )
        }
        outputBuffer.put( expObj.buildBoogieExp( lhs.last, transContext ) )
        outputBuffer.put( " := " )

        for ( r_exp <- rhs.init ) {
          outputBuffer.put( expObj.buildBoogieExp( r_exp, transContext ) )
          if ( !( r_exp.equals( rhs.last ) ) ) // Not to put comma for last expression in sequence
            outputBuffer.put( "," )
        }
        outputBuffer.put( expObj.buildBoogieExp( rhs.last, transContext ) )
        outputBuffer.put( ";" )
        outputBuffer.newLine
        outputBuffer.put( "//Assignment Command Ends" )
      }

      case CallCmdNd( method, argList ) => {

      }

      case IfCmdNd( guard, thenCmd, elseCmd ) => { //set translation context for guard

        outputBuffer.indent
        // Translate body of if
        outputBuffer.dedent
        //else
        outputBuffer.indent
        // Translate Body of else
        outputBuffer.dedent

      }

      case WhileCmdNd( guard, lil, body ) => { //TODO use the Loop Invariant

        var loopInvList = lil;
        if ( "true" == expObj.checkGuard( guard ) ) {
          outputBuffer.newLine
          outputBuffer.put( "while(true)" )
        } else {
          outputBuffer.newLine
          outputBuffer.put( "while(" + expObj.buildBoogieExp( guard, transContext ) + ")" ) // build guard expression, definedness of exxpression
        }
        if ( lil.isEmpty ) {
          transContext.reset()
          transContext.setHeap( "Permission" )
          outputBuffer.newLine
          outputBuffer.put( "//invariant forall<x> r: Ref, f: Field x :: 0.0 <= " + transContext.getHeap() + "[r,f] <= 1.0;" )
          transContext.reset()
        } else {
          for ( loopInv <- lil ) { loopInvCodeGen( loopInv ) } // Invariant Df[guard]
        }

        outputBuffer.newLine
        outputBuffer.put( "{" )
        outputBuffer.indent
        commandCodeGen( body )
        outputBuffer.newLine
        outputBuffer.dedent
        outputBuffer.put( "}" )
      }

      case ForCmdNd( decl, repetitions, lil, body ) => {
        outputBuffer.indent
        //Translate body of For loop
        outputBuffer.dedent
      }

      case CoForCmdNd( decl, repetitions, claimList, body ) => {
        outputBuffer.indent
        //Translate body of Co For Command
        outputBuffer.dedent
      }

      case CoCmdNd( claimList, fstCmd, sndCmd ) => {
        outputBuffer.indent
        //Translate fstCmd
        //Translate sndCmd
        outputBuffer.dedent
      }

      case AcceptCmdNd( methodImplementationList ) => {
        for ( mi <- methodImplementationList ) {
          outputBuffer.newLine
          outputBuffer.put( "goto " + mi.nameNd.toString + ";" )
          outputBuffer.newLine
          outputBuffer.put( mi.nameNd.toString + ":" )

          val methDecl = mi.nameNd.decl.get
          methDecl match {
            case MethodDeclNd( acc,
              paramList,
              preCndList,
              postCndList,
              givesPerList,
              takesPerList,
              borrowsPerList ) => {
              // consider the parameters and borrows
              outputBuffer.indent
              methTakesPerCodeGen( acc, methDecl.fqn.toString(), paramList, takesPerList )
              methPreCondCodeGen( acc, methDecl.fqn.toString(), paramList, preCndList )
              outputBuffer.newLine
              outputBuffer.put( "//Method Implementation" )
              //Figure out the need of parameter list
              outputBuffer.indent
              commandCodeGen( mi.fstCmd )
              outputBuffer.dedent
              methPostCondCodeGen( acc, methDecl.fqn.toString(), paramList, postCndList )
              methGivesPerCodeGen( acc, methDecl.fqn.toString(), paramList, givesPerList )
              commandCodeGen( mi.sndCmd )
              outputBuffer.dedent
              // Go to End? end of Randezevous, omit in case of last branch
              outputBuffer.newLine
              outputBuffer.put( "goto end;" )
              outputBuffer.newLine
              outputBuffer.put( "end:" )

            }
            case _ => "" // Check TypeChecker line 279-280
          }
        }
      }

      case WithCmdNd( lock, tpl, guard, command, gpl ) => {

        val lockTransContext = new TransContext( "LockPermission", expObj.getNamefromLockExp( lock ) )
        transContext.reset()
        transContext.setHeap( "Permission" )
        outputBuffer.indent
        //Lock Translation
        outputBuffer.newLine;
        outputBuffer.put( "var " + lockTransContext.getObjRef() + ": Ref;" )
        outputBuffer.newLine
        outputBuffer.put( lockTransContext.getObjRef() + ":=" + transContext.getObjRef() + ";" ); // is not it breakable?
        outputBuffer.newLine
        outputBuffer.put( "preHeap := Heap;" )
        outputBuffer.newLine
        outputBuffer.put( "havoc tempHeap;" ) // havocs the locations where thread has no permission
        outputBuffer.newLine
        outputBuffer.put( "havoc LockPermission;" )
        transContext.reset()

        //Get permissions from invariant.
        //Collect all the locks and while assignment sum up all the lock expressions

        // assert object invariant defindness and assume object invariant
        assumeClassInv( lock )

        for ( classInv <- dlNd.asInstanceOf[ClassLike].directMembers ) {
          classInv match {
            case ClassInvNd( exp ) => {
              lockExpList = lockExpList :+ ( lockTransContext.getHeap() + "[" + lockTransContext.getObjRef() + "," + transContext.getObjRef() + "]" )
            }
            case _ => ""
          }
          //ArrayBuffer, ListBuffer
        }
        //assuming all the locks sum is b/w 0.0 and 1.0
        lockExpList :+ "assume (forall<x> r: Ref, f: Field x :: 0.0 <= " + lockTransContext.getHeap() + "[r,f] <= 1.0)"

        if ( isGuardPresent( guard ) ) {

          transContext.setHeap( "Permission" );

          //assert guard is defined
          outputBuffer.newLine
          outputBuffer.put( "//assert guard is defined" )
          outputBuffer.newLine
          outputBuffer.setError( "Guard is not defined, do not have anough permission(s)", guard.coord )
          outputBuffer.put( "assert " + expObj.buildReadingPerExp( guard, transContext ) )

          transContext.reset()
          //assume the guard
          transContext.setHeap( "Heap" )
          val guardExp = expObj.buildBoogieExp( guard, transContext )
          outputBuffer.newLine
          outputBuffer.put( "//assume the guard expression" )
          outputBuffer.newLine
          outputBuffer.put( "assume " + guardExp )
          transContext.reset()
        }

        // takes permission list
        for ( tp <- tpl ) {
          for ( ( lsn, amnt ) <- tp.pmn.pm ) {
            lsn match {
              case ObjectIdLSN( nameExp ) => {
                val amount : String = expObj.simpleExpCodeGen( amnt )

              }
              case _ => contracts.Contracts.toDo( "Array Location Set Node" )
            }
          }
        }
        // Body
        outputBuffer.indent
        commandCodeGen( command )
        outputBuffer.dedent

        assertClassInv( lock )

        //gives permission list

        for ( gp <- tpl ) {
          for ( ( lsn, amnt ) <- gp.pmn.pm ) {
            lsn match {
              case ObjectIdLSN( nameExp ) => {
                val amount : String = expObj.simpleExpCodeGen( amnt )
                // not required this point
              }
              case _ => contracts.Contracts.toDo( "Array Location Set Node" )
            }
          }
        }
        outputBuffer.dedent
      }

      case AssertCmdNd( exp ) => {
        transContext.reset()
        transContext.setHeap( "Permission" )
        val expReadingPerCode = expObj.buildReadingPerExp( exp, transContext )
        outputBuffer.newLine
        outputBuffer.setError( "Do not have enough permission(s)", exp.coord )
        outputBuffer.put( "assert " + expReadingPerCode )
        outputBuffer.newLine
        outputBuffer.clearError
        transContext.reset()
        transContext.setHeap( "Heap" )
        val expCode = expObj.buildBoogieExp( exp, transContext )
        outputBuffer.newLine
        outputBuffer.setError( "Assertion might not hold", exp.coord )
        outputBuffer.put( "assert " + expCode )
        outputBuffer.newLine
        outputBuffer.clearError
      }

      case AssumeCmdNd( exp ) => {
        transContext.reset()
        transContext.setHeap( "Permission" )
        val expReadingPerCode = expObj.buildReadingPerExp( exp, transContext )
        outputBuffer.newLine
        outputBuffer.setError( "Do not have enough permission(s)", exp.coord )
        outputBuffer.put( "assert " + expReadingPerCode )
        outputBuffer.newLine
        outputBuffer.clearError
        transContext.reset()
        transContext.setHeap( "Heap" )
        val expCode = expObj.buildBoogieExp( exp, transContext )
        outputBuffer.newLine
        outputBuffer.put( "assume " + expCode )
        outputBuffer.newLine
        outputBuffer.clearError
      }
      case _ => {}
    }
  }

  //Method's 'pre' Condition Code Generation
  def methPreCondCodeGen( acc : Access, name : String, params : List[ParamDeclNd], preCnds : List[PreCndNd] ) {
    transContext.reset()
    outputBuffer.newLine
    outputBuffer.put( "//Pre Condition(s)" )
    outputBuffer.newLine
    outputBuffer.put( "oldHeap := Heap;" )
    outputBuffer.newLine
    outputBuffer.put( "havoc Heap;" )
    for ( prc <- preCnds ) {
      transContext.setHeap( "Heap" )
      outputBuffer.newLine
      outputBuffer.put( "assume " + expObj.buildBoogieExp( prc.condition, transContext ) + ";" )
      outputBuffer.newLine
    }
  }

  //Method's 'takes' Specification Code Generation
  def methTakesPerCodeGen( acc : Access, name : String, params : List[ParamDeclNd], takesPers : List[TakesPerNd] ) {
    transContext.reset()
    outputBuffer.newLine
    outputBuffer.put( "//Taking Permission(s)" )
    for ( tp <- takesPers )
      for ( ( lsn, exp ) <- tp.pmn.pm )
        lsn match {
          case ObjectIdLSN( nameExp ) => {
            val amount : String = expObj.simpleExpCodeGen( exp )
            outputBuffer.newLine
            outputBuffer.put( "oldPermission := Permission;" )
            outputBuffer.newLine
            outputBuffer.put( "if(Permission[" + transContext.objRef + "," + lsn.getName().decl.get.fqn.toString() + "] == 0.0)" )
            outputBuffer.newLine
            outputBuffer.put( "{" )
            outputBuffer.newLine
            outputBuffer.put( "havoc tempHeap;" )
            outputBuffer.newLine
            outputBuffer.put( "Heap[" + transContext.objRef + "," + dlNd.name + "." + lsn.getName() + "] := tempHeap[" + transContext.objRef + "," + dlNd.name + "." + lsn.getName() + "];" )
            outputBuffer.newLine
            outputBuffer.put( "Permission[" + transContext.objRef + "," + dlNd.name + "." + lsn.getName() + "] := " + "Permission[" + transContext.objRef + "," + dlNd.name + "." + lsn.getName() + "]+" + amount + ";" )
            outputBuffer.newLine
            outputBuffer.put( "}" )
          }
          case _ => contracts.Contracts.toDo( "Array Location Set Node" )
        }
  }

  //Method's 'post' Condition Code Generation
  def methPostCondCodeGen( acc : Access, name : String, params : List[ParamDeclNd], postCnds : List[PostCndNd] ) {
    transContext.reset()
    outputBuffer.newLine
    outputBuffer.put( "//Post Condition(s)" )
    for ( poc <- postCnds ) {
      val exp = poc.condition;
      val tempObj = new ExpCodeGen;
      outputBuffer.newLine
      nameExp = new ArrayBuffer[String]()
      val nameExps = tempObj.nameExpCodeGen( exp, nameExp )
      for ( name <- nameExps ) {
        outputBuffer.newLine
        outputBuffer.setError( "Permission amount should be greater than 0.0", poc.condition.coord )
        outputBuffer.put( "assert Permission[" + transContext.objRef + "," + name + "] > 0.0;" )
        outputBuffer.newLine
        outputBuffer.clearError
      }
      outputBuffer.newLine
      outputBuffer.setError( "Post Condition does not satisfy", poc.condition.coord )
      outputBuffer.put( "assert " + expObj.buildBoogieExp( poc.condition, transContext ) + ";" ) // assume and assert
      outputBuffer.newLine
      outputBuffer.clearError
    }
  }

  //Method's 'gives' Specification Code Generation

  def methGivesPerCodeGen( acc : Access, name : String, params : List[ParamDeclNd], givesPers : List[GivesPerNd] ) {
    outputBuffer.newLine
    outputBuffer.put( "//Giving Permissions(s)" )
    transContext.reset()
    transContext.setHeap( "Permission" )
    for ( tp <- givesPers )
      for ( ( lsn, amnt ) <- tp.pmn.pm )
        lsn match {
          case ObjectIdLSN( nameExp ) => {
            val amount = expObj.simpleExpCodeGen( amnt )
            // Assert at least permission, and then the subtract the permission
            outputBuffer.newLine
            //assert the amount of permission at least the amount going to subtract
            outputBuffer.setError( "Can not give permission(s)", lsn.getName().coord )
            outputBuffer.put( "assert " + expObj.buildBoogieExp( nameExp, transContext ) + " >= " + amount + ";" )
            // Subtract the Permission
            outputBuffer.newLine
            outputBuffer.put( expObj.buildBoogieExp( nameExp, transContext ) + " := " + expObj.buildBoogieExp( nameExp, transContext ) + " - " + amount + ";" );
            outputBuffer.newLine
            outputBuffer.clearError
          }
          case _ => contracts.Contracts.toDo( "Array Location Set Node" )
        }
  }

  def assertClassInvariant() {
    outputBuffer.newLine;
    outputBuffer.put( "//Asserting class invariant for Locked object" )
    for ( mem <- dlNd.asInstanceOf[ClassLike].directMembers ) {
      mem match {
        case ClassInvNd( exp ) => {
          val invString = expObj.InvExpCodeGen( exp, mem.fqn.toString, transContext )
          outputBuffer.newLine
          outputBuffer.setError( "Invariant does not hold", exp.coord )
          outputBuffer.put( "assert " + invString )
          outputBuffer.newLine
          outputBuffer.clearError
        }

        case _ => {}
      }
    }
  }

  def assumeClassInvariant() {
    outputBuffer.newLine;
    outputBuffer.put( "//Asserting class invariant for Locked object" )
    for ( mem <- dlNd.asInstanceOf[ClassLike].directMembers ) {
      mem match {
        case ClassInvNd( exp ) => {
          val invString = expObj.InvExpCodeGen( exp, mem.fqn.toString, transContext )
          outputBuffer.newLine
          outputBuffer.put( "assume " + invString )
          outputBuffer.newLine
        }

        case _ => {}
      }
    }
  }

  //Thread Claim Code Generation

  def claimCodeGen( claimNd : ClaimNd ) {
    val perm_pairs = claimNd.pmn.pm;
    var result = "";
    for ( ( loc, amnt ) <- perm_pairs ) {
      var name = "";
      loc match {
        case ObjectIdLSN( nen ) => {
          name = dlNd.name + "." + nen.name.toString();
        }
        case _ => contracts.Contracts.toDo( "Location Set Node with Array" )
      }
      val amount : String = expObj.simpleExpCodeGen( amnt )
      outputBuffer.newLine
      outputBuffer.put( "//Claim" )
      outputBuffer.newLine
      outputBuffer.put( "Permission[" + transContext.objRef + "," + name + "] := Permission[" + transContext.objRef + "," + name + "] + " + amount + ";" )
    }
  }

  private def classInvCodeGen() {
    outputBuffer.newLine
    outputBuffer.put( "//Class Invariant" )
    for ( mem <- dlNd.asInstanceOf[ClassLike].directMembers )
      mem match {
        case ClassInvNd( exp ) => {
          classInvList :+ mem;
          val invString = expObj.InvExpCodeGen( exp, dlNd.fqn.toString, transContext )
          outputBuffer.newLine
          outputBuffer.setError( "Invariant does not hold", exp.coord )
          outputBuffer.put( "assert " + invString + ";" )
          outputBuffer.newLine
          outputBuffer.clearError
        }
        case _ => {}
      }
  }

  private def loopInvCodeGen( loopInv : LoopInvNd ) {
    //Check Loop Invariant is Defined
    transContext.reset()
    outputBuffer.newLine
    outputBuffer.put( "//loop Invariant" )
    val invString = expObj.InvExpCodeGen( loopInv.exp, dlNd.fqn.toString, transContext )
    outputBuffer.newLine
    outputBuffer.setError( "Invariant does not hold", loopInv.exp.coord )
    outputBuffer.put( "assert " + invString )
    outputBuffer.newLine
    outputBuffer.clearError
  }

  def isAssignmentDefined( lhsList : List[ExpNd], rhsList : List[ExpNd] ) {
    outputBuffer.newLine
    outputBuffer.put( "//Check Assignment Defindness" )
    transContext.reset()
    val localTransContext = transContext;
    localTransContext.setHeap( "Permission" )
    val expPairs = lhsList zip rhsList
    for ( ( lhs, rhs ) <- expPairs ) {
      val lhs_result = expObj.buildWritingPerExp( lhs, localTransContext )
      outputBuffer.newLine
      outputBuffer.setError( "Do not have enough permission(s) on LHS of assignment", lhs.coord )
      outputBuffer.put( "assert " + lhs_result ) // Two asserts instead of one
      outputBuffer.newLine
      outputBuffer.clearError
      nameExp = new ArrayBuffer[String]()

      val nameExps = expObj.nameExpCodeGen( rhs, nameExp )
      for ( name <- nameExps ) {
        outputBuffer.newLine
        outputBuffer.setError( "Permission amount should be greater than 0.0", rhs.coord )
        outputBuffer.put( "assert Permission[" + localTransContext.getObjRef() + "," + name + "] > 0.0;" )
        outputBuffer.newLine
        outputBuffer.clearError
      }
    }
    //val rhs_result = expObj.buildReadingPerExp(rhs, localTransContext)
    // one glitch while translating the IntLiteralExpressionNode
    outputBuffer.newLine
    outputBuffer.put( "//Check Assignment Defindness Ends" )

  }

  def isGuardPresent( exp : ExpNd ) : Boolean = {

    exp match {
      case NameExpNd( name : NameNd ) => true

      case BinaryOpExpNd( op : BinaryOperator, x : ExpNd, y : ExpNd ) => true

      case UnaryOpExpNd( op : UnaryOperator, x : ExpNd ) => true

      case MemberExpNd( x : ExpNd, name : String ) => true

      case ChainExpNd( ops : List[ChainingOperator], operands : List[ExpNd] ) => true

      case FetchExpNd( x : ExpNd ) => true

      case AsExpNd( x : ExpNd, _ ) => true

      case CanReadOp( x ) => true

      case CanWriteOp( y ) => true

      case PermissionOp( x ) => true

      case _ => false
    }

  }
  def assumeClassInv( lock : ExpNd ) { // For locked objects
    val lockContext = new TransContext( "LockPermission", expObj.getNamefromLockExp( lock ) );
    transContext.reset()
    val baseContext = new TransContext( transContext.getHeap(), transContext.getObjRef() )
    for ( classInv <- dlNd.asInstanceOf[ClassLike].directMembers ) {
      classInv match {
        case ClassInvNd( exp ) => {
          // assert defindness
          outputBuffer.newLine
          outputBuffer.put( "//Assert defindness of object invariant and assume object invariance" )
          outputBuffer.newLine
          nameExp = new ArrayBuffer[String]()
          val nameExps = expObj.nameExpCodeGen( exp, nameExp )
          if ( !( nameExps.isEmpty ) ) {
            outputBuffer.setError( "Does not have enough permission(0)", exp.coord )
            outputBuffer.newLine
            outputBuffer.put( "assert " );
            for ( name <- nameExps )
              outputBuffer.put( lockContext.heap + "[" + lockContext.getObjRef() + "," + name + "] > 0.0;" ) // generating && true extra , need to fix this glitch
            outputBuffer.newLine
            outputBuffer.clearError
          }

          //assume invariant
          transContext.reset()
          outputBuffer.newLine
          outputBuffer.put( "assume " + expObj.InvExpCodeGen( exp, classInv.fqn.toString, lockContext, transContext ) + ";" ) // due to lock we need different invariant generation
          outputBuffer.newLine

        }
        case _ =>
      }
    }
  }

  def assertClassInv( lock : ExpNd ) { //For Locked objects
    val lockContext = new TransContext( "LockPermission", expObj.getNamefromLockExp( lock ) );
    transContext.reset()
    val baseContext = new TransContext( transContext.getHeap(), transContext.getObjRef() )
    for ( classInv <- dlNd.asInstanceOf[ClassLike].directMembers ) {
      classInv match {
        case ClassInvNd( exp ) => {
          // assert defindness
          outputBuffer.newLine
          outputBuffer.put( "//Assert defindness of object invariant and assert object invariance" )
          outputBuffer.newLine
          nameExp = new ArrayBuffer[String]()
          val nameExps = expObj.nameExpCodeGen( exp, nameExp )
          if ( !( nameExps.isEmpty ) ) {
            outputBuffer.setError( "Does not have enough permission(0)", exp.coord )
            outputBuffer.newLine
            outputBuffer.put( "assert " );
            for ( name <- nameExps )
              outputBuffer.put( lockContext.heap + "[" + lockContext.getObjRef() + "," + name + "] > 0.0;" ) // generating && true extra , need to fix this glitch
            outputBuffer.newLine
            outputBuffer.clearError
          }

          //assert invariant
          transContext.reset()
          outputBuffer.newLine
          outputBuffer.put( "assert " + expObj.InvExpCodeGen( exp, classInv.fqn.toString, lockContext, transContext ) + ";" ) // due to lock we need different invariant generation
          outputBuffer.newLine

        }
        case _ =>
      }
    }
  }

  def objDeclCodeGen( isConst : Boolean, acc : Access, ty : TypeNd, fqn : String ) : String = {
    val objType : String = TypeCodeGen( ty )
    val objCode = "\nconst unique " + fqn + ":" + "Field " + objType + ";"
    return objCode
  }

}
