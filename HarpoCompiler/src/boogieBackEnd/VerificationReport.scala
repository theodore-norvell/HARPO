package boogieBackEnd

import scala.collection.mutable.ArrayBuffer
import contracts.Contracts

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
}
