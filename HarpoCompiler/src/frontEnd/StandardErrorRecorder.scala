package frontEnd

import scala.collection.mutable._

class StandardErrorRecorder extends ErrorRecorder with ErrorReport {
    
    private class HarpoError( val text : String, val coord : AST.Coord )

    private val errors : Buffer[HarpoError] = new ArrayBuffer[HarpoError]() ;
    
    private val fatals : Buffer[HarpoError] = new ArrayBuffer[HarpoError]() ;
    
    private val warnings : Buffer[HarpoError] = new ArrayBuffer[HarpoError]() ;
    
    private val verifications : Buffer[HarpoError] = new ArrayBuffer[HarpoError]() ;
  
    def reportFatal( mess : String, coord : AST.Coord ) {
        val err = new HarpoError( mess, coord )
        errors.append(  err )
        fatals.append( err )
    }
    
    def reportWarning( mess : String, coord : AST.Coord ) {
        val err = new HarpoError( mess, coord )
        errors.append(  err )
        warnings.append(  err )
    }
    
    def reportVerification( mess : String, coord : AST.Coord ) {
        val err = new HarpoError( mess, coord )
        errors.append(  err )
        verifications.append(  err )
    }
    
    def bailOut() : Nothing = throw new CompilerBailOutException() 
    
    def getTotalErrorCount() : Int = errors.length
    
    def getErrorText( i : Int ) : String = errors(i).text
    
    def getErrorCoord( i : Int ) : AST.Coord = errors(i).coord
    
    def getFatalCount(): Int = fatals.length
    
    def getFatalText(i: Int): String = fatals(i).text
    
    def getFatalCoord(i: Int): frontEnd.AST.Coord = fatals(i).coord
    
    def getWarningCount() : Int = warnings.length
    
    def getWarningText( i : Int ) : String = warnings(i).text
    
    def getWarningCoord( i : Int ) : AST.Coord = warnings(i).coord
    
    def getVerificationCount() : Int = verifications.length
    
    def getVerificationText( i : Int ) : String = verifications(i).text
    
    def getVerificationCoord( i : Int ) : AST.Coord = verifications(i).coord
    
    def printErrors( out : java.io.PrintStream ) {
        out.println( "Fatal errors" );
        for( err <- fatals ) {
          out.print( err.coord )
          out.print( " " )
          out.println( err.text )
        }
        out.println( "Warning errors" );
        for( err <- warnings ) {
          out.print( err.coord )
          out.print( " " )
          out.println( err.text )
        }
        out.println( "Verification errors" );
        for( err <- verifications ) {
          out.print( err.coord )
          out.print( " " )
          out.println( err.text )
        }
    }
    
}
