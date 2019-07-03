package boogieBackEnd

import scala.collection.mutable.ArrayBuffer

import contracts.Contracts
import frontEnd.AST
import frontEnd.ErrorRecorder
import util.OutputBuilder

/** Provide a summary of the output from a verification tool such as Boogie.
 *  @author theo
 */
class VerificationReport extends Contracts {
  class Error( val lineNumber : Int, val boogieMessage : String ) {
    val associatedLineNumbers : ArrayBuffer[Int] = new ArrayBuffer[Int]

    def addAssociatedLineNumber( lineNumber : Int ) : Unit =
      associatedLineNumbers.append( lineNumber )
  }

  var latestError : Error = null

  val verificationErrorList = new ArrayBuffer[Error]

  val otherErrorList = new ArrayBuffer[Error]

  def verificationErrorCount : Int = this.verificationErrorList.size

  def putVerificationError( lineNumber : Int, boogieMessage : String ) : Unit = {
    latestError = new Error( lineNumber, boogieMessage )
    verificationErrorList.append( latestError )
  }

  def getVerificationError( i : Int ) : Error = verificationErrorList( i )

  def addAssociatedLineNumber( lineNumber : Int ) : Unit =
    pre( latestError != null )
      .in {
        latestError.addAssociatedLineNumber( lineNumber )
      }

  def otherErrorCount : Int = this.otherErrorList.size

  def putOtherError( lineNumber : Int, boogieMessage : String ) : Unit = {
    otherErrorList.append( new Error( lineNumber, boogieMessage ) )
  }

  def getOtherError( i : Int ) : Error = otherErrorList( i )
  
  def reportErrors( errorRecorder : ErrorRecorder, outputBuilder : OutputBuilder ) : Unit = {
    for( vError <- verificationErrorList ) {
      val opt = outputBuilder.getError( vError.lineNumber ) 
      var mess : String = ""
      var coord : AST.Coord = null
      opt match {
        case None => {
          mess = "Verification failed with with no location known.\n" +
          "Line number in Boogie file is " + vError.lineNumber +
          "The error from the verifier is \"" + vError.boogieMessage +"\"" 
          coord = AST.noCoord 
        }
        case Some( (m, c) ) => {
          mess = m
          coord = c
        }
      }
      for( associatedLineNo <- vError.associatedLineNumbers ) {
        outputBuilder.getError( associatedLineNo ) match {
          case None =>
            mess += "\nAssociated line number at unknown location in Harpo. Boogie line number is " + associatedLineNo ;
          case Some( (m,c) ) =>
            mess += "\nAssociated line " + c.toString 
        }
      }
      errorRecorder.reportVerification(mess, coord) ;
    } // End for
    for( oError <- otherErrorList ) {
      val mess = "Internal error: Unexpected error from boogie\n" +
                 "Line number in Boogie file is " + oError.lineNumber +
                 "The error from the verifier is \"" + oError.boogieMessage +"\"" 
      errorRecorder.reportFatal( mess, AST.noCoord  )
    }
  }
}
